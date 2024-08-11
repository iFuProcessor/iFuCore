# LSU 异常处理机制初探

## 1. Introduction
本文将从 LSU 对外表现的异常情况（也就是`io.lsu_xcpt`）出发，阐述 LSU 处理访存阶段发生的异常的方法。
首先，我们先介绍一些基础背景：
- LSU 所处理的异常，只会有两种来源：一是 LSU 自身乱序行为带来的写后读异常（此时称为`MINI_EXCEPTION_MEM_ORDERING`），而是 TLB 在进行地址翻译时检测到的异常。

## 2. 相关变量溯源

### 2.1 Part 1
1. 首先，我们从最顶层的`io.lsu_xcpt`开始，其直接来源于一个名为`r_xcpt`的寄存器（当然，还有一个名为`r_xcpt_valid`的寄存器，用于标记`r_xcpt`是否有效）。但是，在对外传递异常时，`还需要额外检测当前 ROB 是否正在处理另外的异常，以及发生异常的指令有没有被分支杀掉。
2. 接下来，我们先看`r_xcpt_valid`，我们观察其代码：
    ```scala
    r_xcpt_valid :=
        (ld_xcpt_valid || tlb_xcpt_valid)             &&
        !io.core.exception                            &&
        !IsKilledByBranch(io.core.brupdate, xcpt_uop)
    ```
    我们逐项进行分析解释：
    1. 第一部分表示是否有异常发生，而二者正是我们上文提到的两种异常来源。
    2. 第二部分检测当前 ROB 是否正在处理另外的异常，注意，这和上面的检查并不重复，因为`r_xcpt_valid`是一个寄存器，因此此处的检查相较于上文中的检查，提前了一个周期。
    3. 检查发生异常的指令有没有被分支杀掉，同样的，这个检查也是提前了一个周期。
3. 在看完`r_xcpt_valid`之后，不妨先点到为止，把目光转向`r_xcpt`。同样的，我们直接给出其代码：
    ```scala
    r_xcpt.uop := xcpt_uop
    r_xcpt.uop.brMask := GetNewBrMask(io.core.brupdate, xcpt_uop)
    r_xcpt.cause := Mux(use_tlb_xcpt, tlb_xcpt_cause, MINI_EXCEPTION_MEM_ORDERING)
    r_xcpt.badvaddr := tlb_xcpt_vaddr
    ```
    1. 首先需要明确的是，`r_xcpt`会直接存储发生异常的指令，以及异常的原因，对应的地址信息。
    2. 异常指令直接来自于`xcpt_uop`，但是需要在当前周期更新其分支掩码。
    3. 根据`use_tlb_xcpt`的值，来选择异常的原因。
4. 截至目前，我们分析了`r_xcpt_valid`和`r_xcpt`，也即依赖深度为 1 的变量。二者都是作为寄存器存在，因此，我们可以知道，检测到异常后，并非立刻抛出，而是等到下一个周期再抛出。这也符合我们通常的处理方式。

### 2.2 Part 2
在第一部分中，我们实际上留下了不少坑，例如，`ld_xcpt_valid`和`tlb_xcpt_valid`的来源是什么，在哪个阶段产生？`xcpt_uop`、`use_tlb_xcpt`、`tlb_xcpt_cause`、`tlb_xcpt_vaddr`这类变量的来源又是什么？
因此，在这一部分，我们开始分析这些依赖深度为 2 的变量。
1. 首先，我们先看`ld_xcpt_valid`，其代码如下：
    ```scala
    val ld_xcpt_valid = failed_loads.reduce(_|_)
    ```
    从这里可以看出，`ld_xcpt_valid`检测所有`load entry`，只要存在访存违例的情况，就会被置为`true`。
2. 接下来，我们看`tlb_xcpt_valid`，其代码如下：
    ```scala
    val tlb_xcpt_valids = RegNext(widthMap(w =>
        exe_tlb_valid(w)                                    &&
        dtlb.io.resp(w).exception.valid                     &&
        !io.core.exception                                  &&
        !IsKilledByBranch(io.core.brupdate, exe_tlb_uop(w))
    ))
    tlb_xcpt_valid := tlb_xcpt_valids.reduce(_||_)
    ```
    1. `exe_tlb_valid(w)`表示当前周期是否发送了一条 TLB 请求。请求都没有，何谈异常？
    2. `dtlb.io.resp(w).exception.valid`表示 DTLB 是否返回了一个异常。
    3. 之后的两项检查和之前的类似，不再赘述。
    4. 最后，`tlb_xcpt_valid`汇总当前周期所有的异常情况。
3. 先讨论`use_tlb_xcpt`，代码如下：
    ```scala
    val use_tlb_xcpt =
        (
            tlb_xcpt_valid && IsOlder(tlb_xcpt_uop.robIdx, ld_xcpt_uop.robIdx, io.core.rob_head_idx)
        ) ||
        !ld_xcpt_valid
    ```
    1. 如果当前没有访存违例，那么直接使用 TLB 的异常（可能此时 TLB 也没有异常，但是无妨，因为相关的`valid`位会被置为`false`）。
    2. 如果二者同时存在，会选取更早的那条指令。这是通过比较其在 ROB 中的位置来实现的（ROB 中的指令是顺序排列的）。
4. 然后是`xcpt_uop`，同样找到其代码：
    ```scala
    val xcpt_uop = Mux(use_tlb_xcpt, tlb_xcpt_uop, ld_xcpt_uop)
    ```
    在以及有了`use_tlb_xcpt`的情况下，便可以通过一个`Mux`选择异常指令。

### 2.3 Part 3
同样的，在上面的分析中，新引入的坑有哪些呢？`failed_loads`、`tlb_xcpt_uop`、`ld_xcpt_uop`。其余的内容都比较明显，不过多的赘述。
1. `failed_loads`，根据存储的信息，检查每个槽位的`load`指令是否有访存违例。检查的过程在这里暂时不做展开。
2. `tlb_xcpt_uop`，记录s0 阶段发起 TLB 请求的指令，依据 TLB 的返回结果，取出发生异常的指令。
3. `ld_xcpt_uop`，依据检查的结果，直接取出最早的一条发生访存违例的指令（这里检测到后会马上抛出异常，除非其被更前面的异常抑制，那么此时，会发生回滚，也就会取消掉这条指令，访存违例也就不会发生，所以就算没有被处理，也没有什么问题）。

## 3. 总结
经过上述的分析，我们可以得到如下的一个大致的异常处理流程：
1. s0 阶段：将指令的地址送到 TLB 进行地址翻译
2. s1 阶段：处理 TLB 的返回结果，以及查看访存违例的情况（实际上每周期都会检查，为了便于理解，我们将其放在这里）
3. s2 阶段：根据检测到的异常情况，将异常信息送往上层处理
