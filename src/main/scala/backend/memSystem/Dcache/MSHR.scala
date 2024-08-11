package iFu.backend
import chisel3._
import chisel3.util._
import iFu.common._
import iFu.util._
import iFu.common.Consts._

class MSHRdata extends CoreBundle with HasDcacheParameters{
    //该项是否有效
    val valid = Bool()

    // 该项的id（用于一表里面某一项对应的fetch结束时向二表中搜索同id的表项）
    val id = UInt(log2Up(nFirstMSHRs).W)

    // 该项的请求
    val req = new DCacheReq

    val pos = UInt(log2Up(nWays).W)

    // //该项将来充填，必然命中于replaceway，replay的时候不用再进行命中判断，而是直接去找那个路
    // 取消掉这个变量,让他自己出去匹配hit就行了
    // val way = UInt(log2Ceil(nWays).W)
    
    // 该项是否在等待fetch
    val waiting = Bool()


    // 该项是否已经fetch完毕，这时候就等待被使用replay
    val ready = Bool()

}


class MSHREntry extends CoreModule with HasDcacheParameters {
    val io = IO{new Bundle{
        // 写入请求111
        val req = Flipped(Decoupled(new DCacheReq))

        // 传出请求111
        val replayReq = Decoupled(new DCacheReq)
        // 
        val replaypos = Output(UInt(log2Up(nWays).W))

        // 分支预测调整TODO
        val brupdate = Input(new BrUpdateInfo())

        val exception = Input(Bool())

        // 之后发进来的每条新的miss请求的地址111
        val newBlockAddr = Input(UInt(nBlockAddrBits.W))
        // 是不是用的同一块111
        val newblockAddrMatch = Output(Bool())

        // reset信号111
        val reset = Input(Bool())
        
        // 发进来的每条fetch请求的地址111
        val fetchedBlockAddr = Input(UInt(nBlockAddrBits.W))
        // 判断是不是fetch的就是自己这一块11
        val fetchedBlockAddrMatch = Output(Bool())
        // fetch完毕的信号111
        val fetchReady = Input(Bool())
        // 该行所在的位置
        val fetchedpos = Input(UInt(log2Up(nWays).W))

        // 该项是否有效并且已经fetch完毕（可以发出去replay）11
        val active = Output(Bool())

        // 该项是否在等待fetch111
        val waiting = Output(Bool())


        // 对于一表，这里是一表所在对应行的索引，对于二表，这里是一表中对应的id111
        val id = Input(UInt(log2Up(nFirstMSHRs).W))
        // 该项的id（用于一表里面某一项对应的fetch结束时向二表中搜索同id的表项）111
        val getID = Output(UInt(log2Up(nFirstMSHRs).W))
        //111
        val hasStore = Output(Bool())

        val wakeUp = Input(Bool())

        // 快速唤醒
        val fastWakeUp = Input(Bool())

    }}

    // 数据存储
    val mshr = RegInit(0.U.asTypeOf(new MSHRdata))
    // if(!FPGAPlatform)dontTouch(mshr)
    // if(!FPGAPlatform)dontTouch(io)

    // 告诉外界是否存了一个store指令
    io.hasStore := isStore(mshr.req) && mshr.valid

    // 该项是否在等待fetch
    io.waiting := mshr.waiting && mshr.valid

    // 该项是否可以写入新的请求（如果无效就证明这里可以用）
    io.req.ready := !mshr.valid

    // 该项是否可以发出去replay（如果有效并且已经fetch完毕就可以发出去）
    io.replayReq.valid := mshr.valid && mshr.ready
    
    io.replayReq.bits := mshr.req

    io.replaypos := mshr.pos
    
    io.active := mshr.valid && mshr.ready

    //分支预测调整 或reset 或对于load的execption
    when (io.reset || IsKilledByBranch(io.brupdate, mshr.req.uop.brMask) || (io.exception && !isStore(mshr.req))){
        mshr.valid := false.B
        mshr := 0.U.asTypeOf(new MSHRdata)
    }
    // reset 和写入调换了顺序，见fixlog说明

    mshr.req.uop.brMask := GetNewBrMask(io.brupdate, mshr.req.uop)

    when(io.req.fire){
        // 满足则写入新的请求
        mshr.valid := true.B
        mshr.id := io.id
        mshr.req := io.req.bits
        mshr.req.uop.brMask := GetNewBrMask(io.brupdate, io.req.bits.uop)

        when(io.fastWakeUp){
            // 直接转为就绪状态
            mshr.waiting := false.B
            mshr.ready := true.B
            mshr.pos := io.fetchedpos
        }.otherwise{
            mshr.waiting := true.B
            mshr.ready := false.B
            mshr.pos := 0.U
        }
    }
    // 传出id
    io.getID := mshr.id

    val mshrBlockAddr = Mux(mshr.valid, getBlockAddr(mshr.req.addr), 0.U)
    // if(!FPGAPlatform)dontTouch(mshrBlockAddr)
    // 给外界返回match判断
    io.newblockAddrMatch     := mshr.valid && IsEqual(mshrBlockAddr, io.newBlockAddr)
    io.fetchedBlockAddrMatch := mshr.valid && IsEqual(mshrBlockAddr, io.fetchedBlockAddr)
    // 状态机
    when (mshr.valid) {
        when (io.wakeUp){
            mshr.ready := true.B
            mshr.waiting := false.B
            mshr.pos := io.fetchedpos
        }

        when (mshr.waiting) {
            // 正在等待,一旦取好转为redy
            when (io.fetchedBlockAddr === mshrBlockAddr && io.fetchReady) {
                mshr.waiting  := false.B
                mshr.ready    := true.B
                // 存入位置
                mshr.pos      := io.fetchedpos
            }
        }
        when (mshr.ready && io.replayReq.fire) {
            // 成功发出replay信号，默认必完成(除了分支和xcpt的kill),直接转为无效
            mshr := 0.U.asTypeOf(new MSHRdata)
        }
    }
}


class MSHRFile extends CoreModule with HasDcacheParameters{
    val io = IO(new Bundle {

        // miss指令写入MSHR
        // 发生miss的请求
        val req  = Flipped(Decoupled(new DCacheReq))
        // 发起新的fetch请求

        // 传出所需的fetch请求
        // RPU是否空闲作ready,空闲才可以发出新的fetch请求 
        val newFetchreq = Valid(new DCacheReq) 

            
        // 发出去的replay请求
        val replay = Decoupled(new DCacheReq) 

        val replaypos = Output(UInt(log2Up(nWays).W))

        // RPU的激活信号 
        // RPU传入的fetch地址
        val fetchedBlockAddr = Input(UInt(nBlockAddrBits.W))
        // fetchReady，当RPU成功拿到了一行并写回，就会发出这个信号，通知mshr根据这个地址，
        // 去进行激活操作111
        val fetchReady = Input(Bool())
        val fetchedpos = Input(UInt(log2Up(nWays).W))

        // 是否有store指令
        val hasStore = Output(Bool())

        // 告诉外界已满111
        val full = Output(Bool())

        // 传入的更新信息，这回逐一检测每一个LOAD指令是不是分支错误，如果是，就把它删掉
        // 对于STORE来说，由于LSU传来的STORE一定是分支预测正确的，所以不用管111
        val brupdate = Input(new BrUpdateInfo())
        
        val exception = Input(Bool())
            
        })

    if(!FPGAPlatform)dontTouch(io)
    val firstMSHRs = VecInit((Seq.fill(nFirstMSHRs)(Module(new MSHREntry))).map(_.io))
    val secondMSHRs = VecInit((Seq.fill(nSecondMSHRs)(Module(new MSHREntry))).map(_.io))

    //记录一表match的信息
    // 一表中的每一项是否match
    val newblockAddrMatches = WireInit(0.U.asTypeOf(Vec(nFirstMSHRs, Bool())))
    val firstNewMatchway = WireInit(0.U(log2Up(nFirstMSHRs).W))
    // 一表中的每一项是否是fetch对应的地址
    val fetchedBlockAddrMatches = WireInit(0.U.asTypeOf((Vec(nFirstMSHRs, Bool()))))
    val firstFetchMatchway = WireInit((0.U(log2Up(nFirstMSHRs).W)))
    // 一表中的每一项是否可以写入新的请求
    val firstAllocatable = WireInit(0.U.asTypeOf(Vec(nFirstMSHRs, Bool())))
    // 一表是否已满
    val firstFull = WireInit(false.B)

    // 搜索是否有等待fetch的项
    val waitinglist = WireInit(0.U.asTypeOf(Vec(nFirstMSHRs, Bool())))
    val haswait = waitinglist.asUInt.orR
    val waitingpos = PriorityEncoder(waitinglist)

    // if(!FPGAPlatform)dontTouch(waitinglist)
    // if(!FPGAPlatform)dontTouch(haswait)

    val firstHasStores = WireInit(0.U.asTypeOf(Vec(nFirstMSHRs, Bool())))

    for(i <- 0 until nFirstMSHRs){
    firstMSHRs(i).req.valid := false.B
    firstMSHRs(i).req.bits := io.req.bits

    // 一表的id是定好的行号
    firstMSHRs(i).id := i.U(log2Up(nFirstMSHRs).W)

    // 进行匹配，找空位等操作
    firstAllocatable(i) := firstMSHRs(i).req.ready

    firstMSHRs(i).newBlockAddr := getBlockAddr(io.req.bits.addr)
    newblockAddrMatches(i) := firstMSHRs(i).newblockAddrMatch
    // 一表只起到作为代表去指导取地址，因此不要传入分支更新，即使被kill掉，由于不知道有没有人依赖这个
    // 一表项，所以不会被分支抹掉,还需要继续取完
    firstMSHRs(i).brupdate := /* io.brupdate */ 0.U.asTypeOf(new BrUpdateInfo)
    firstMSHRs(i).exception := false.B

    firstMSHRs(i).reset :=  false.B

    firstMSHRs(i).fetchedBlockAddr := io.fetchedBlockAddr
    firstMSHRs(i).fetchReady := io.fetchReady
    firstMSHRs(i).fetchedpos := io.fetchedpos
    fetchedBlockAddrMatches(i) := firstMSHRs(i).fetchedBlockAddrMatch

    firstMSHRs(i).replayReq.ready := false.B
    firstMSHRs(i).wakeUp := false.B

    firstMSHRs(i).fastWakeUp := false.B

    // 查看1表有没有正在等候的
    waitinglist(i) := firstMSHRs(i).waiting

    firstHasStores(i) := firstMSHRs(i).hasStore

}

    


    // 找一表中的match项
    firstFetchMatchway := PriorityEncoder(fetchedBlockAddrMatches.asUInt)
    firstNewMatchway := PriorityEncoder(newblockAddrMatches.asUInt)

    firstFull := !(firstAllocatable.asUInt.orR)

    // 传入的请求是不是首次miss, 如果是store指令，必须作为首次miss(见log)
    val firstMiss = !(newblockAddrMatches.asUInt.orR)
    val allocFirstMSHR = PriorityEncoder(firstAllocatable.asUInt)

    // 二表中的每一项是否可以写入新的请求
    val secondAllocatable = WireInit(0.U.asTypeOf(Vec(nSecondMSHRs, Bool())))
    val allocSecondMSHR = PriorityEncoder(secondAllocatable.asUInt)
    // 二表是否已满
    val secondFull = !(secondAllocatable.asUInt.orR)

    val secondHasStores = WireInit(0.U.asTypeOf(Vec(nSecondMSHRs, Bool())))
    // 这里用regNext是为了更好的时序
    val hasStore = RegNext(secondHasStores.asUInt.orR) /* || firstHasStores.asUInt.orR */

    // hasStore
    io.hasStore := hasStore
    

    for(i <- 0 until nSecondMSHRs) {
        // 二表相对于一表，只用来写入，和brupdate调整
        secondMSHRs(i).req.valid := false.B
        secondMSHRs(i).req.bits := io.req.bits

        secondMSHRs(i).id := 0.U
        
        secondAllocatable(i) := secondMSHRs(i).req.ready

        secondMSHRs(i).fetchedBlockAddr := DontCare

        // 先统一赋为假，之后会赋值
        secondMSHRs(i).fetchReady := false.B
        secondMSHRs(i).fetchedpos := 0.U
        
        secondMSHRs(i).brupdate := io.brupdate

        secondMSHRs(i).exception := io.exception

        secondMSHRs(i).reset := false.B

        secondMSHRs(i).replayReq.ready := false.B
        
        // 搜索存储store的信息
        secondHasStores(i) := secondMSHRs(i).hasStore

        secondMSHRs(i).newBlockAddr :=  DontCare
        //唤醒
        secondMSHRs(i).wakeUp := false.B

        secondMSHRs(i).fastWakeUp := false.B

    }

    // 任意一个满了，外界都不能再往里面写入新的请求
    val mshrFull = firstFull || secondFull
    io.full := mshrFull

    // 不满，并且不会再已有store的时候再存入新的store，才接收外界信号
    io.req.ready := !mshrFull && !(hasStore && isStore(io.req.bits))
    when(io.req.fire){
        // 首次miss，一表二表都要写入
        when(firstMiss){
            firstMSHRs(allocFirstMSHR).req.valid := io.req.valid
            // 二表的id是一表中的行号id
            secondMSHRs(allocSecondMSHR).id := allocFirstMSHR
            secondMSHRs(allocSecondMSHR).req.valid := io.req.valid
        }.otherwise{
        // 非首次miss，只写入二表，id是一表它对应的那一项的id
            secondMSHRs(allocSecondMSHR).id := firstNewMatchway
            secondMSHRs(allocSecondMSHR).req.valid := io.req.valid
            // 快速唤醒见devlog
            val matchActiveFirst = (
                                    (io.fetchReady && fetchedBlockAddrMatches(firstNewMatchway))//当周期匹配到一个将要被唤醒的
                                    || firstMSHRs(firstNewMatchway).active//一个被唤醒保持活跃状态的表项
                                    )
            
            when(matchActiveFirst){
                // 快速唤醒
                secondMSHRs(allocSecondMSHR).fastWakeUp := true.B
                // 一表项在唤醒的时候会将自己变成ready并且记录下pos,此时拿判断是当周期传入的pos还是活跃一表的pos
                secondMSHRs(allocSecondMSHR).fetchedpos := Mux(firstMSHRs(firstNewMatchway).active 
                                                                ,firstMSHRs(firstNewMatchway).replaypos 
                                                                ,io.fetchedpos )
            }.otherwise{
                // 什么都不做，按正常运行 
                secondMSHRs(allocSecondMSHR).fastWakeUp := false.B
            }
        }
    }.otherwise{
        // 否则根本不接受外界信号
        // io.req.ready := false.B
    }

    // 如果一个fetch取好了，就把它从一表中删掉，然后激活二表中的对应项，等待之后用到它的时候一项一项的拿
    when(io.fetchReady){
        // 激活second表中的fetch地址项,同时告诉二表这一行所在的位置
        for(i <- 0 until nSecondMSHRs) {
            when(secondMSHRs(i).getID === firstFetchMatchway){
                secondMSHRs(i).wakeUp := true.B
                secondMSHRs(i).fetchedpos := io.fetchedpos
            }
        }
    }
    // 删除first表中的fetch地址项(两个周期之后，refill进行到s2，正式写meta
    val s2fetchReady = RegNext(RegNext(io.fetchReady))
    val s2firstFetchMatchway = RegNext(RegNext(firstFetchMatchway))
    // 再一个周期，留给refill后面可能紧跟的，被判为miss的lsu请求（见devlog）
    val s3fetchReady = RegNext(s2fetchReady)
    val s3firstFetchMatchway = RegNext(s2firstFetchMatchway)
    firstMSHRs(s3firstFetchMatchway).reset := s3fetchReady

    // 传出新的fetch地址(如果有的话)
    val fetchable = haswait

    io.newFetchreq := 0.U.asTypeOf(Valid(new DCacheReq))
    // 向外发出需要fetch的信号
    when(fetchable){
        io.newFetchreq.valid := true.B
        io.newFetchreq.bits.addr := (firstMSHRs(waitingpos).replayReq.bits.addr)
    }.otherwise{

    }

    // 记录被激活的二表项
    val actives = WireInit(0.U.asTypeOf(Vec(nSecondMSHRs, Bool())))
    for(i <- 0 until nSecondMSHRs) {
        actives(i) := secondMSHRs(i).active
    }

    // 选取replay阶段
    // 是否真的有被激活的二表项
    val repalyactive = actives.asUInt.orR
    // 选一个被激活的二表项，发出去replay
    val replayIdx = PriorityEncoder(actives)

    io.replay.bits  := 0.U.asTypeOf(new DCacheReq)
    when(repalyactive){
        io.replay.valid := true.B
        io.replay.bits := secondMSHRs(replayIdx).replayReq.bits
        // 如果外面接了，就告诉二表项，我已经取走了你的请求，可以准备清空了
        io.replaypos := secondMSHRs(replayIdx).replaypos
        secondMSHRs(replayIdx).replayReq.ready := io.replay.ready
    }.otherwise{
        io.replay.valid := false.B
        io.replaypos := 0.U
    }

}
