# iFu-IssueStage

[toc]

## 架构设计

### 基类 -- `AbsIssueUnit`

#### IO 接口
- `disUop` [input] 从dispatch阶段发送过来的`uop`。
- `wakeupPorts` [input] 从外界传入的`wakeup`端口。
- `specLdWakeupPorts` [input] load指令推测唤醒端口。
- `ldMiss` [input] load指令发生miss，不能及时进行数据前递。该信号未指定具体发生miss的指令，此处我们简答处理，视为所有load指令均发生miss。
- `fuTypes` [input] 下一阶段（执行阶段）能接收哪些类型的指令，因为不同的功能单元能执行的功能不完全相同。
- `brUpdate` [input] 分支预测更新。
- `flushPipeline` [input] 清空流水线。
- `issueValids` [output] 发射出去的指令的有效位。
- `issueUops` [output] 发射出去的指令。

#### 内部元件
- `numIssueSlots`个`IssueSlot`组成的`slots`。
   作为顶层，`AbsIssueUnit`会将唤醒信息、分支更新信息、flush信息等传递给`slots`。

#### 内部逻辑
1. [组合逻辑] `dispatch`阶段发来的`uop`并不带有`issue window state`，此时需要计算`iwState`。一般都是`s_valid_1`，但是对于写内存指令（store指令等）需要被标记为`s_valid_2`（因为其可能会分为两个阶段被发射：地址计算和数据计算）。
2. [组合逻辑] `iw_p1_poisoned`和`iw_p2_poisoned`被默认置为`false`。
3. [组合逻辑] 将发过来的`uop`连同上述信号统一放到`disUops`中。

### `IssueSlot`

#### IO 接口
- `valid` [output] 该槽位中的指令是否有效。
- `willBeValid` [output] 当前周期的指令虽然被选中发射了，但是由于store指令分两次发射、load推测唤醒错误等情况，导致未能发射出去。
- `request` [output] 数据已就绪，请求发射。
- `grant` [input] 该槽位被选中，当前周期发射。
- `brUpdate` [input] 分支预测更新。
- `kill` [input] 清空该槽位，直接来自`flushPipeline`。
- `clear` [input] 清空该槽位。该槽位指令被移出，且当前周期没有指令移入。
- `ldSpecMiss` [input] load指令发生miss。直接来自`ldMiss`。
- `wakeupPorts` [input] wakeup端口。
- `specLdWakeupPorts` [input] load指令推测唤醒端口。
- `inUop` [input] 当前周期输入的`uop`。
- `outUop` [output] 从当前槽位输出的`uop`。用于构成压缩队列。
- `uop` [output] 被发射出去的`uop`。

#### 内部元件
- `state` 记录当前槽位存储的指令的状态。
- `slot_uop` 槽中存储的指令。
- `p1` 指令的第一个操作数是否就绪。
  `p1`与`slot_uop`是处于不同时序的。具体来说，对于同一条指令，当指令位于`slot_uop`中时，`p1`表达的是该指令的第一个操作数在上一个周期时（包括上一个周期）是否就绪。也就是说，指令会经历至少一周期的唤醒（哪即使数据已经就位），才会被发射。
- `p2` 指令的第二个操作数是否就绪。
- `p1_poisoned` 第一个操作数就绪的来源是load指令的推测唤醒。
- `p2_poisoned` 第二个操作数就绪的来源是load指令的推测唤醒。

#### 内部逻辑
1. [组合逻辑] 会依据当前的`state`和`slot_uop`以及传入的信息计算出`next_state`和`next_uop`。
   1. `next_state`主要是会受到`brUpdate`、`kill`、`clear`、指令发射这几类情况的影响。
   2. `next_uop`同样也是会跟着更新。另外，`next_uop`会将`p1`等信息记录到`uop`中。
2. [组合逻辑] 根据`in_uop`和传入的`wakeup`信息，更新`p1`和`p2`。
   1. 这也就是`p1`等信号早一周期的原因。
3. [组合逻辑] 根据`p1`等信号，以及`state`，决定是否发射。
   1. 发射时可能会修改`uopcode`和`lrs1_type1`等信息。（主要是因为store可能会分两次发射）
4. [组合逻辑] 会计算当前槽位下一周期是否还会有效。
   1. 有没有新的有效指令进来。
   2. 当前的指令是否被发射出去。
5. [时序逻辑] 由于是一个压缩队列，所以数据来源有两个，一是`in_uop`，二是上一个周期的`next_uop`。因此这里会有一个环。

### 具体实现 -- `IssueUnitAgeOrdered`
- 继承自`AbsIssueUnit`，并采用了`AgeOrder`的发射策略。底层的实现方法是将`IssueSlot`组织成一个压缩队列。每周期至多前移4个槽位。

#### 内部逻辑
- 主要分为两块：压缩队列逻辑和发射逻辑。

##### 压缩队列的实现
- 压缩通过计算移位的偏移来实现。其中，偏移量使用one hot的格式来表示。通过两层`for`循环（外层遍历所有槽位，内层遍历所有偏移量）来做压缩。
  - `maxShift`为`dispatchWidth`，这里做shift最主要的目的是要接收被派遣过来的几条指令，因此只需要至多移位`dispatchWidth`。这样可以避免出现过大的组合逻辑电路。
  - `vacnats`取的是每个槽位的`valid`信号的反，即表示该槽位是否为空，再在后面拼接上新派遣过来的指令的`valid`信号的反。
  - `shamtOH`表示的是每个槽位需要往前移动的位数，用one hot的形式表示。
  - `getShamtOH`的逻辑是：当前槽的移位量至少等于前一个槽位，如果前一个槽位是空的，则比前一个槽位多移一个位置。但是，如果前一个槽位的移位量已经是`maxShift`，则不再多移动。另外，移位量从`0`到`1`需要显式的检查，后续的移位量则可以通过左移一位来实现。
  - 移位是通过把后面的槽位的`outUop`赋值给前面的槽位的`inUop`来实现的。在移位时，遍历所有的槽位，然后遍历这个槽位往后的`maxShift`个槽位，检查其`shamtOH`是否等于`1 << (j - 1)`，如果是，即表示其后的第`j`个槽位需要移动到当前槽位中。
  - 最后，如果发生了移位，需要`clear`掉发生移位的槽位。不然就会有重复的指令。
- 存在反压现象，当队列不足以接收所有`dispatch_uop`时，会通过`ready`信号，实现只接受一部分指令，并与上一阶段完成消息的传递。此处计数用的是直接对空槽位进行`popcount`。

##### 选择发射的指令
- 使用两层`for`循环实现，此处会用到`chisel`中的变量。
- 外层循环遍历所有槽位，内层循环遍历所有功能单元。
- 如果指令和执行单元的功能标志位匹配，则称之为可分配。
- 如果可分配、改槽位发起了发射请求、该槽位的指令此前没有被发射过、该功能单元此前没有接受过指令，则发射该指令。

## 一些实现细节
- 被发射出去的`uop`并没有使用`brUpdate`来更新，在后续的`rrd`阶段会进行更新。（具体来说是`rrd-1`阶段）

## 最后修改日期
- 2024/04/17
- 2023/07/01
