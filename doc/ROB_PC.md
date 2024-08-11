# ROB 重定向PC选取

## 1. Introduction
本文主要关注 ROB 产生重定向信号时，如何选出重定向的 PC。

## 2. 选择信号

### 2.1 产生选择信号的方法
在 ROB 中，如果有重定向的需求，最终会通过一个名为`FlushTypes`的类来产生重定向信号，其定义如下：
```scala
object FlushTypes {
    def SZ =3
    def apply() = UInt(SZ.W)
    def none = 0.U
    def xcpt = 1.U
    def eret = (2+1).U
    def refetch = 2.U
    def next = 4.U

    def useCsrEvec(typ: UInt): Bool = typ === eret || typ === xcpt
    def useSamePC(typ: UInt): Bool  = typ === refetch
    def usePCplus4(typ: UInt): Bool = typ === next

    def getType(valid: Bool, i_xcpt: Bool, i_eret: Bool, i_refetch: Bool): UInt = {
        val ret = Mux(!valid    , none   ,
                  Mux( i_xcpt   , xcpt   ,
                  Mux( i_eret   , eret   ,
                  Mux( i_refetch, refetch,
                                  next   ))))
        ret
    }
}
```
可以看到，`FlushTypes`定义了 5 种重定向类型，分别是`none`、`xcpt`、`eret`、`refetch`和`next`。接下来，我们逐个解释其含义：
- `none`：无需重定向，即不产生重定向信号；当传入的`valid`为`false`时，产生此信号。
- `xcpt`：异常重定向，当 ROB 检测到异常时，会使用此信号。
- `eret`：当 ROB 检测到一条`eret`指令被提交时，产生此信号。
- `refetch`：如果触发的异常是 CPU 内部自定义的异常，需要重新从异常指令处开始执行，则采用此信号。
- `next`：默认情况下，PC 会递增 4，即下一条指令的地址。该信号用于处理`flush on commit`的情况。

另外，由于是采用`Mux`实现，实际上这几种信号存在优先级关系，即`xcpt` > `eret` > `refetch` > `next`。
实际上存在冲突的情况有：
- `xcpt`和`refetch`：异常包括 ISA 中定义的异常和 CPU 内部自定义的异常，优先处理 ISA 中定义的异常。
- `eret`和`next`：`eret`用于处理`eret`指令，`refetch`用于处理其余设置了`flush on commit`的指令。

### 2.2 ROB 中传递的控制信号
在 ROB 中，采用上述的`FlushTypes.getType`来获取重定向类型，实现如下：
```scala
  io.flush.bits.flush_typ := FlushTypes.getType(
    flushVal                                ,
    exceptionThrown && !isMiniException     ,
    flushCommit && flushUop.uopc === uopERET,
    refetchInst
)
```
同样的，我们逐个解释其含义：
1. `flushVal`：ROB 是否要产生重定向信号。对应的代码如下：
    ```scala
    val flushVal = exceptionThrown || flushCommit
    ```
    从中可以看出，当检测到异常或提交`flush on commit`的指令时，会将`flushVal`置为`true`。
2. `exceptionThrown && !isMiniException`：异常重定向。当检测到异常且不是 CPU 内部自定义的异常时，会产生此信号。目前的`isMiniException`用于处理访存违例的情况。此时，重新执行一遍就可以解决此问题，不需要进入异常处理流程。
3. `flushCommit && flushUop.uopc === uopERET`：`eret`重定向。
4. `refetchInst`：需要从异常处重新执行，对应的代码如下：
    ```scala
    val insnSysPc2epc =
        robHeadVals.reduce(_||_) &&
        PriorityMux(robHeadVals, io.commit.uops.map{u => u.is_sys_pc2epc})
    val refetchInst = exceptionThrown || insnSysPc2epc
    ```
    当检测到异常且不是 CPU 内部自定义的异常时，会将`refetchInst`置为`true`。
    实际上，`insnSysPc2epc`并没有意义，因为拥有`is_sys_pc2epc`标志的指令只有`syscall`和`break`，但是这两条指令同时会带有异常标志，依据上文的优先级关系，会直接进入异常处理流程。
    综上，`refetchInst`等效于`exceptionThrown`。但是由于优先级的关系，其实际上生效的情况只有访存违例。

## 3. 选择 PC
当 core 中检测到 ROB 抛出重定向信号后，会选取重定向的 PC。具体实现如下：
```scala
    when (RegNext(rob.io.flush.valid)) {
        ...
        val flush_type = RegNext(rob.io.flush.bits.flush_typ)
        ...
        when (FlushTypes.useCsrEvec(flush_type)) {
            ifu.io.core.redirect_pc := csr.io.redirect_pc
        } .otherwise {
            val flush_pc = (
                AlignPCToBoundary(ifu.io.core.getFtqPc(0).pc, iCacheLineBytes) +
                RegNext(rob.io.flush.bits.pc_lob)
            )
            val flush_pc_next = flush_pc + coreInstrBytes.U
            ifu.io.core.redirect_pc := Mux(
                FlushTypes.useSamePC(flush_type),
                flush_pc, flush_pc_next
            )
        }
        ...
    } ...
```
可以看到，当 ROB 抛出重定向信号后，下个周期会根据上述方法计算出的`flush_type`来选择 PC。具体来说，有以下几种情况：
1. `FlushTypes.useCsrEvec`：对应的是`eret`和`xcpt`。此时，使用 CSR 模块中的`redirect_pc`作为重定向的 PC。
2. `FlushTypes.useSamePC`：对应的是`refetch`。上面已经解释过了，不过多赘述。
