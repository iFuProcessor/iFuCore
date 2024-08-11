
(重构dcache之后)
1.15
关于转发的问题：
对于syncReadMem，在verilator同周期读写可以内部转发，reg不自带内部转发
因此在wordData和Meta类中都加了转发检查机制
在wordData中，当周期判断，下周期转发
```scala
  // bypass
    val bypass = Wire(Vec(memWidth, Bool()))
    for (w <- 0 until memWidth) {
        // 当周期判断，下周期转发
        bypass(w) := rvalid(w) && wvalid && (ridx1v(w) === widx1v)
        when (RegNext(bypass(w))) {
            io.read(w).resp.bits.data := RegNext(wreq.data)
        }
    }

```

在meta中
```scala
// bypass
    val bypass = Wire(Vec(memWidth, Bool()))
    for (w <- 0 until memWidth) {
        // 当周期判断，下周期转发
        bypass(w) := rvalid(w) && wvalid && (ridx(w) === widx)
        when (RegNext(bypass(w))) {
            // 看看write操作对应位有修改吗，如果有，用写的值，没有的话，还是保留原来读到的rmetaSet的值
            io.read(w).resp.bits.rmetaSet(wpos).valid := Mux(RegNext(wreq.setvalid.valid), RegNext(wreq.setvalid.bits), rmetaSet(w)(wpos).valid)
            io.read(w).resp.bits.rmetaSet(wpos).dirty := Mux(RegNext(wreq.setdirty.valid), RegNext(wreq.setdirty.bits), rmetaSet(w)(wpos).dirty)
            io.read(w).resp.bits.rmetaSet(wpos).readOnly := Mux(RegNext(wreq.setreadOnly.valid), RegNext(wreq.setreadOnly.bits), rmetaSet(w)(wpos).readOnly)
            io.read(w).resp.bits.rmetaSet(wpos).tag := Mux(RegNext(wreq.setTag.valid), RegNext(wreq.setTag.bits), rmetaSet(w)(wpos).tag)
        }
    }
```

这里的RegNext是为当周期判断下周期用设置的


2. 关于mshr的FakefirstMiss

当一个refill进行到最后一个，进行到s2的时候，会
- 写回newmeta
- 并且通告mshr对应的一表项，一表项当周期给对应二表项激活一次，下周期就被清掉了
直到被清掉，这个表项还是waiting状态

在这个周期之前运行到s2的二次miss都可以正常匹配，正常工作，正常被激活
在这个时候由于设置了meta内部转发，s0的同地址快读已经可以命中了

唯独此时的s1被判断成了miss，
一个周期后，当他s2进表的时候对应的表一项早就没了，因此他以为自己是首次miss
导致了fakefirstmiss的出现，这会导致重新refill，破坏缓存一致性

解决方法：做一个一周期之前信息的转发，如果发现一周期之前有一个fetchReady，
并且那时对应的块地址和当前的地址一样，就视为一个secondMiss，存入即激活(fastWakeUp机制)


3.brmask , kill 的检查

s1，s2 都要检查是不是kill了,目前s2valid是
```scala
for(w <- 0 until memWidth){
        s2valid(w) := RegNext(s1valid(w) &&
                            !(s1state === lsu && (isStore(s1req(w)) && s2StoreFailed)) && 
                            !(s1state === lsu && (!isStore(s1req(w)) && IsKilledByBranch(io.lsu.brupdate, s1req(w).uop))) &&
                            !(s1state === replay && (!isStore(s1req(w)) && IsKilledByBranch(io.lsu.brupdate, s1req(w).uop)))
                            )
}
```
这只考虑了s1的kill情况，在s2的时候也要当周期更新brmask之后检查一下



4. 有关指令miss之后的行为
- miss之后在s2经过mshr判断能不能存进来，是firstmiss还是secondmiss，需不需要快启动

- 之后总线空闲的时候会发出mshrread请求，这个请求在s0传入地址，s1从meta中返回可被替换的行号，（替换前后idx肯定相同，不用专门传）



5. 对于mshrreplay可能导致的乱序
在refill的最后一个周期，由于内部转发，此时s0的一个用到该meta的指令会认为自己命中，这可以多做一个乱序ld

问题：在一个storemiss ， 进了mshr，lsu会从下一个st指令一直重发，这里应该不允许做完（保证顺序！）

解决此问题需要利用的机制是，s1判断，如果mshr里面有store，就判为miss，然后再以s2的时候hasStore接不了而引起重发
<!-- 这个不好 -->


然而，如果refill最后一个到第二周期，是一个曾经miss的store指令发起的，就会导致这个store在下个周期才能被replay

然而下个周期：
replay到s0的时候，s2的storemiss发现没有hasStore，就可以存进去，s1上个周期被meta内部转发判为命中，如果s1此时是后面重发的st指令，这会导致s1乱序



作出改动：首先一个一表项直到fetchReady后的两周期才会被清掉，多留两个周期的"影子"，然后，所有s1判断到mshr.io.hasStore的st会被判为miss

- refill，fetchReady，s1的miss，一周期之后进去，一表项还在，根据影子判断是secondMiss
- refill，fetchReady，s0的超前命中st，一周期之后，一表项还在，hasStore还是为高，自己被判为miss，两周期之后，一表项还在，自己装不进去，会发回nack，导致重发

这两个特殊的周期由此解决、

从某种意义上讲，这里当且仅当两周期之后第一个真正的st的replay才被算做完，那时候才算一表项被清，也正常

移除fakeFirstMiss，保留快速唤醒，将hasStore转变为一二表项共同的判断逻辑
并且这两个特殊的存活周期中该表项会变成ready，并且记录下replay的pos，
如果在特殊周期有二次命中，肯定不能从头开始等（那一个一表项马上消失，等不来的），必须快速唤醒，并且从那个一表项拿replaypos

6. 对上面乱序的补充
判断st乱序的情况不能只看mshr的hasStore,还要看流水线里面有无比他前面的store，如果有，自己要按miss论处，判断逻辑在s1，因此hasStore要看mshr和s2里面的store

```scala
    // mshr有store，或者s2里面有一个即将失败的store请求，还在s1的store就要把自己判断为miss
    val hasStore = mshrs.io.hasStore || (s2valid(0) &&isStore(s2req(0)) && !s2hit(0)) 

    s1hit(w) := Mux(isStore(s1req(w)) && hasStore, false.B, true.B)
```

7. reseting
dcache一共64*8*16个字需要很多个周期才reset为0，这期间如果请求已经发过来了，就可能被reset覆盖掉！
因此先不做数据的reset试一下


8. 
把这里的req.valid变成了req.fire，否则lsu没发这边已经开始拿过来做了就会导致问题发生
```scala
// 流水线里面有mmioresp的时候，下一个fire的请求不要进来
lsuMMIOValid := io.lsu.req.fire && lsuhasMMIO && axiReady
lsuNormalValid := io.lsu.req.fire && !lsuhasMMIO && io.lsu.req.ready
```

9. 有关fenceClear的meta行处理
目前写回脏行之后不动其他位,只做dirty位
如果仅仅在写回脏行之后清掉meta的dirty位,那以后有个对该行的uncacheable的写请求,就会造成不一致
但是上述操作本身就是具有破坏缓存一致性的风险,这应该是不会发生的

fence之后必须彻底清除掉这一行

原因在于st miss
在fence写回脏行的时候,会变成readOnly状态,如果有store 这一行,会进mshr,等所谓的脏位清零,
然而接下来mshrread不会匹配,一定会找个invalid的路作为自己refill的路----即使此时那一行"还在",等他refill完,他那就有两个项,以后的命中判断就出问题了

因此在fence结束必须彻底清除掉那一行,这样以后的命中判断才可以正常按照新拿来的那一行进行

在普通的替换策略是不需要担心,因为如果那被替换的一行变成readonly,就意味着它在wb,它一定会在wb后fetch被销毁掉,等st来mshrread,这一行就被新的覆盖了
这时是正常的寻路替换

                                                            
10. TODO: 协调lsu的信号控制:如果仅仅是isunique,就只判断stqEmpty,只有fence指令才会给dcache发force_order,才会判断dcache的ordered

11. 对于mshr的fenceClear:
不需要,unique亮起来的时候,此时后端所有的指令都是一定要完成的,不用清什么ld之类的



12. 资源节约 priority Encoder 是否真的需要？
当前的位置信息使用优先级编码器，方便debug,但是这里会不会造成LUT过多的浪费？
以及这篇文章所说的
https://zhuanlan.zhihu.com/p/650745488
是不是错误将我们的BRAM识别成了LUT？

已解决,关键问题是物理寄存器过多导致寄存器重命名和寄存器堆转发逻辑



13. 对于store指令的二次miss欺骗问题
一个store进了mshr，发现一表里面有一个ld，于是自己进了二表等待唤醒，但问题是
那个ld指令可能会被分支kill掉，于是就只剩store指令留在二表项，再也无法被唤醒

一个解决方案是，st指令必须作为一二表项同时去存储
上面这个并不能解决根本,因为ld依赖ld的情况也会出现

最后的解决方案,由于一表项只负责指导fetch地址,没必要传入brupdate检测kill掉,因为不知道自己代表的块地址有没有后面二次miss的在等着用.即使被kill了,他也要坚持把自己这个地址取完(即使可能取了没有任何其他人等着用)
而二表项才是负责产生replay信号的,传入brupdate,如果要kill,就在这里kill.这样就不会出现上面的问题了


14. 当前TODO
给Dcache行扩充大小，一拍只有16个，但是要实现refillCycle = 4 之类连续发起四次AXI请求

15. 修改了hasDirty的生成逻辑，如果是个seq的bool，reduce（_|_）需要的逻辑门很多，而一开始定义成Vec，之后asUInt.orR就可以转化成verilog里面的缩位逻辑了

16. 记录参数影响，目前stq16 ldq 16  Rob 64 issunit 减半，
ds影响不大，ipc还是1.27

17. 之前设readOnly和fixed都是Mem，这会随机初始化，可能导致fixed为真一直换不出去。现在改成了RegInit，这样就不会随机初始化了



18. 逻辑优化
收到fetchReady的时候
原来的设计是,对于一表项，需要晚两个周期被reset
原因是
fetchReady到s2的时候才会给mshr，这个时候，s0,s1都有其他的事务了，如果这条指令是miss重填好的st指令，那么当他进入s0的时候，s1，s2那些lsu发过来的在他之后的的st指令就会被先做完，这是不允许的，因此s2的时候，有一条说正常做完的st指令要看mshr里面有没有hasStore，配合上一表项两周期的驻留，就可以保证这个指令不会提前做完，而是再一次去重发。

但是完全可以在s0就判断出来送给mshr去让他下个周期replay，就不会有这个问题，并且当s2refill完成的时候，如没有更高优先级的东西，s1正好执行到那条replay的东西读数据，此时s2写最后一个字,被内部转发保证了数据的正确性使得逻辑更加清晰了.


19. mshr的secondmiss导致重取行的问题
![Alt text](imgs\image.png)

分析refill
当最后一个字的refill进行到s0时候，fetchReady和fetchedPos拉高
s0 refill
s1 肯定miss的 lsu1
s2 肯定miss的 lsu2 此时它可以看到一表项，没问题

下个周期一表项就将被删除了

s0 将要miss的lsu2 (一般是紧跟replay，但可能replay的被branchKill了，传来的是普通的lsu指令)
s1 refill
s2 肯定miss的lsu1 ，但此时一表项已经被删除了，就做不了secondmiss了
因此需要一个regnext的fetchready和pos。来避免这个问题

再下个周期
当refill结束作用于meta的时候

s0 xxx，没问题，就是hit了
s1 如果是lsu，此时实际上被判miss
s2 refill

再下个周期，miss的lsu进行到s2，然而此时一表项已经被清除掉了，就做不了secondmiss了。
因此需要两个regnext的fetchready和pos。来避免这个问题

一表项得晚三个周期重置，这三个周期正好适合被调整为活跃状态(两个周期配合refill的s2，再一个周期留给refill后面的那条lsu)，便于二表项快速唤醒判断，根据fetchReady信号和pos信号，来判断是否快速唤醒

这三个周期相比于至少16个周期的refill周期，是不会出现新的重填冲突的

20. readOnly设置时间
之前说当refill到该行的第一个字才设成readOnly，为的是能多做几条st指令，然而这是错误的

没有dirty的一行，mshrread送入wfu，此时wfu认为只需要refill新的行就好

在mshrread到refill第一个字的期间
如果有st这一行的指令进来，就会命中，它不知道readOnly，hit上做了一个字，但wfu显然不会感知到这个问题，因为他认为只要refill新的行就好，于是访存不一致

因此要尽可能提早地设置readOnly，不是refill第一个字，而是在mshrread到s2的时候就要设置readOnly

此时情况如下
- s0 没问题，感知到readOnly
- s1 如果是那种st指令，一周期之前还不知道该行被设为readOnly，仍然hit要做
- s2 刚刚把那一行设为readOnly

因此附加机制，fastReadOnly，存储上述情况（要readOnly但那还没拉高）的那一行，在metalogic里面，只读行的条件不仅仅那一行readOnly，也要或上正好匹配到fastReadOnly，这样才是完整的只读行条件

21. 技术性问题
a = RegInit(0.U)
只是初始值为0
如果不指定a始终有默认值a := 0
如果之后有哪怕一个周期触发了a := 1,那么a就不会再是0了，并不是自动默认值
因此还是要加上a := 0 ,然后再去某个分支条件里面赋值a:=1


22. 写回行的时候，如果该行clean，但是前面一个周期有store
- s0 mshrread 要向meta读行，感知不到前面的dirty
- s1 lsu命中这一行要写，这一行将边dirty
- s2 不会干扰mshrread

如果感知不到这一行是dirty，wfu拿到的会误以为是clean，只做refill，不wb那么就会导致访存不一致

那么需要在里面加fastDirty，记录这种情况，如果有fastDirty，那么meta的dirty位就不是0，而是快速地1，以方便给wfu
使之正常的先发起wb，再refill



23. meta逻辑的转发
由于meta是s0读而s2才写，因此需要一个周期s2->s1的转发
需要读meta的：
lsu看valid和readonly
mshrread看valid和dirty

需要写meta的：
mshr写readonly
refill写新的meta行
replay和lsu写dirty
(添加lsLsuStore 和 isReplayStore 两个信号，
lsu如果hit了，其idx，hitpos对应的meta要做转发，当然，可能lsu hit但是到s2因为mshr有其他st，并不会做，可能会导致后面的mshrread读到的meta“认为”是dirty，但是没问题，因为将干净的行重写回去，不会影响。
对于replay，需要s0传入自己将写入的idx和pos（pos之前没加，现在加了7.14），从而完成转发
)

- mshrread 写readonly要让前一周期lsu看到(done)
- refill第一个字会废meta行，但是前一个周期的只读load的lsu还是可见的，此时行的第一个字被破坏了，因此调整到wb结束的时候就废除meta行**wb就除掉，到refill的时候至少隔一个周期，所有命中的ld数据行不会被refill破坏**，从而可以保证到refill写第一个字的时候那一行已经作废，lsu ld全部已经算miss(done)
- refill最后一个字会写meta行，前一个lsu可能还是会误判为miss，解决方案如19所示（done）
- 写dirty的replay和lsu要让前一周期mshrread看到从而正确反映给wfu(done) 至于valid，只有refill会改动，二者互斥，不会出现问题


时序问题，对于lsu看到mshr 的 replace_find 万一是store就会错， 直接bubble掉，mshr在s1不让lsu发请求，不然在判断hitoh就会有若干复杂逻辑
```java
/*  & ~(s2_inv_mask & Fill (nWays, s2_inv_val && s2_inv_idx === ridx)) */
```
事实证明这对ipc几乎没影响

而且现在也不做只读的判断了，在mshr的s2周期直接报销掉那个cache行
那么mshr的replace_find -> (wb 的refill_logout) -> refill 第一个字的refill_logout
现在就很简单了，wb和refill没必要去报废meta行了

24. mshr hasstore 逻辑
hasStore可以只看二表项里面是否有store，然后对外做一个regNext(hasStore)以减少时序计算
需要在s1计算hit，同时做一个转发，保证s2当周期将存入的miss Store 前递

 需要被转发信息的那条指令包括： s1的要看s2的s2enter_missStore，来判断store miss ， s2不能参与写入mshr，并且发回nack和storeFailed

正确性：
一条st，miss之后不允许后面任何的cached st提前做完
|s0 | s1 | s2 | mshr |
|---|----|----| ---- |
| st3  | st2（forwarded hasStore） | st1(miss,into mshr) | no entry no hasStore |
| st4  | st3         | st2(must miss, send Nack and StoreFailed, wont enter mshr) | has entry no hasStore |
| st2  | st4(killed) | st3(killed) | has entry hasStore |
| st3  | st2(get io.hasStore) | st4(killed) | has entry hasStore |

于是下一条st2不会先于st1做完

当st1replay的时候，要保证后面的st2（可能和s1同一块地址）不能再miss（再refill会出问题）
|s0 | s1 | s2| mshr |
|---|---|---| --- |
| st1（replay）| * | * | hasentry hasStore |
| st2 | st1(replay) | * | no entry hasStore |
| st3 | st2(hit, wont miss again) | st1(replay) | no entry no hasStore |