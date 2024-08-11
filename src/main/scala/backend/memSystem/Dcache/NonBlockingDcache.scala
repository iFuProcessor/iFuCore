
package iFu.backend

import chisel3._
import chisel3.util._

import iFu.sma._

import iFu.common._
import iFu.common.Consts._
import iFu.util._
import iFu.lsu.utils._

import iFu.difftest._

trait HasDcacheParameters extends HasCoreParameters {
    val nTagBits       = dcacheParameters.nTagBits
    val nIdxBits       = dcacheParameters.nIdxBits
    val nOffsetBits    = dcacheParameters.nOffsetBits
    val n1vIdxBits     = dcacheParameters.n1vIdxBits
    val nAgeBits       = dcacheParameters.nAgebits
    val nRowBytes      = dcacheParameters.nRowBytes
    val nRowWords      = dcacheParameters.nRowWords
    val nTotalWords    = dcacheParameters.nTotalWords
    val nSets          = dcacheParameters.nSets
    val nWays          = dcacheParameters.nWays
    val nBlockAddrBits = dcacheParameters.nBlockAddrBits
    val nFirstMSHRs    = dcacheParameters.nFirstMSHRs
    val nSecondMSHRs   = dcacheParameters.nSecondMSHRs

    def getWordOffset(vaddr: UInt): UInt    = dcacheParameters.getWordOffset(vaddr)
    def getIdx(vaddr: UInt): UInt       = dcacheParameters.getIdx(vaddr)
    def getTag(vaddr: UInt): UInt       = dcacheParameters.getTag(vaddr)
    def getBlockAddr(vaddr: UInt): UInt = dcacheParameters.getBlockAddr(vaddr)
    def isStore(req : DCacheReq): Bool  = dcacheParameters.isStore(req)

    def isMMIO(req : DCacheReq): Bool  = dcacheParameters.isMMIO(req)

    def isSC(req: DCacheReq): Bool = dcacheParameters.isSC(req)
}

class DCacheBundle extends CoreBundle with HasDcacheParameters {
    val lsu  = Flipped(new LSUDMemIO)
    val smar = Vec(2, new SMAR)
    val smaw = Vec(2, new SMAW)
}

class NonBlockingDcache extends Module with HasDcacheParameters {
    val io = IO(new DCacheBundle)

    // replay      : 来自 mshr 重发的此前 miss 的访存请求
    // wb          : wfu 发起的读 dcache 请求，用于写回到内存
    // refill      : wfu 发起的写 dcache 请求，用于填充 dcache
    // replace_find: mshr 发起的读 dcache 请求，用于获取待替换的行的信息
    // lsu         : 来自 lsu 的访存请求
    // mmio_req    : 来自 lsu 的 mmio 访存请求
    // fence_read  : 清空 dcache 时，获取 dcache 行的信息，可能会触发写回
    // fence_clear : wfu 完成了一个只写回的请求，需要清空对应的 meta 行
    // cacop       : 未实现
    // nil         : 空操作
    val (s_replay   :: s_wb        :: s_refill     :: s_replace_find :: s_lsu   ::
      s_mmio_req    :: s_mmio_resp :: s_fence_read :: s_fence_clear  :: s_cacop :: s_nil :: Nil) = Enum(11)

// -----------------------------------------------------------------------------------
// module defination
    val mmiou = Module(new MMIOUnit)
    mmiou.io.mmioReq.valid := false.B
    mmiou.io.mmioReq.bits  := DontCare

    io.smaw(0) <> mmiou.io.smaw
    io.smar(0) <> mmiou.io.smar

    val wfu = Module(new WriteFetchUnit)
    wfu.io.req_valid           := false.B
    wfu.io.meta_resp           := DontCare
    wfu.io.req_wb_only         := false.B
    wfu.io.wfu_read_resp.valid := false.B
    wfu.io.wfu_read_resp.bits  := DontCare

    io.smaw(1) <> wfu.io.smaw
    io.smar(1) <> wfu.io.smar

    val meta = Module(new DcacheMetaLogic)

    meta.io.lsuRead(0).valid   := false.B
    meta.io.lsuRead(1).valid   := false.B
    meta.io.replayRead.valid   := false.B
    meta.io.missReplace.valid  := false.B
    meta.io.fetchDirty.valid   := false.B

    meta.io.lsuRead(0).bits    := DontCare
    meta.io.lsuRead(1).bits    := DontCare
    meta.io.replayRead.bits    := DontCare
    meta.io.missReplace.bits   := DontCare
    meta.io.fetchDirty.bits    := DontCare

    meta.io.lsuWrite     := 0.U.asTypeOf(Valid(new DcacheMetaReq))
    meta.io.replaceLogout   := 0.U.asTypeOf(Valid(new DcacheMetaReq))
    meta.io.wfuWrite     := 0.U.asTypeOf(Valid(new DcacheMetaReq))
    meta.io.replayWrite  := 0.U.asTypeOf(Valid(new DcacheMetaReq))
    meta.io.lineClear    := 0.U.asTypeOf(Valid(new DcacheMetaReq))
    meta.io.cacopRead    := DontCare
    meta.io.cacopWrite   := DontCare

    val data = Module(new DcacheDataLogic)

    data.io.read(0).req.valid := false.B
    data.io.read(1).req.valid := false.B
    data.io.write.req.valid   := false.B

    data.io.read(0).req.bits := DontCare
    data.io.read(1).req.bits := DontCare
    data.io.write.req.bits   := DontCare

    val mshrs = Module(new MSHRFile)
    mshrs.io.brupdate  := io.lsu.brupdate
    mshrs.io.exception := io.lsu.exception
// -----------------------------------------------------------------------------------

// -----------------------------------------------------------------------------------
// io port
    // 只要meta没有dirty，就可以回应fence，不需要管流水线和mshr状态（如果里面有没做完的指令，lsu肯定非空，unique仍然会停留在dispatch）
    io.lsu.ordered := !meta.io.hasDirty
// -----------------------------------------------------------------------------------

// ================================ s0 ==================================

    val wbValid     = wfu.io.wfu_read_req.valid
    val refillValid = wfu.io.wfu_write_req.valid

    val wfuReady          = WireInit(false.B)
    val replace_findValid = mshrs.io.newFetchreq.valid && wfuReady
    val mshrReplayValid   = mshrs.io.replay.valid
    val fenceReadValid    = io.lsu.fence_dmem && meta.io.hasDirty && wfuReady
    val fenceClearValid   = wfu.io.line_clear_req.valid
    val lsuhasMMIO        = io.lsu.req.bits.map(req =>
        req.valid && isMMIO(req.bits)
    ).reduce(_||_)
    val lsuNormalValid    = io.lsu.req.fire && !lsuhasMMIO
    val lsuMMIOValid      = io.lsu.req.fire &&  lsuhasMMIO

    val mshr_bubble = WireInit(false.B)
    io.lsu.req.ready := !(
        wbValid || refillValid || fenceClearValid || mshrReplayValid || replace_findValid || fenceReadValid || mmiou.io.mmioResp.valid
        || mshr_bubble
    )

    mmiou.io.mmioResp.ready := !(
        wbValid || refillValid || fenceClearValid 
    )

    val s0valid = Wire(Vec(memWidth, Bool()))
    for (w <- 0 until memWidth) {
        if (w == 0) {
            s0valid(w) := (
                (io.lsu.req.bits(w).valid && !mshr_bubble)||
                wbValid || refillValid || fenceClearValid || mshrReplayValid ||
                replace_findValid || fenceReadValid || mmiou.io.mmioResp.valid
            )
        } else {
            s0valid(w) := io.lsu.req.bits(w).valid && !mshr_bubble
        }
    }
    // 总线相关请求是最高优先级，包括 wb 和 refill 这两个请求，他们互斥，只有一个会发生
    // lsuMMIOValid 和 mmioRespValid 也是总线请求
    // fenceClear 和 fenceRead也都属于总线请求
    // (同一时间,以上六个最多只有一个发生)

    // 然后是 mshrrepplay ，和 replace_find
    // 然后lsu的请求
    // 然而fenceRead的时候其实是清除脏位,往往配合未完成的store指令一起做完才算完事,因此要给lsu和mshr,replay让步
    // 优先级很低,可以等着,但是fenceClear是要点名清除某一行,那个周期发了必须做,之后就没这个信号了,因此fenceClear优先级要在总线最高级那里

    val dontCareReq = 0.U.asTypeOf(new DCacheReq)
    val s0req = Wire(Vec(memWidth, new DCacheReq))
    for (w <- 0 until memWidth) {
        if (w == 0) {
            s0req(w) := Mux(wbValid                 , wfu.io.wfu_read_req.bits,
                        Mux(refillValid             , wfu.io.wfu_write_req.bits,
                        Mux(fenceClearValid         , wfu.io.line_clear_req.bits,
                        // 这里的mmioresp是DcacheReq作为载体,data是可能的rdata,uop是对应uop
                        Mux(mmiou.io.mmioResp.valid , mmiou.io.mmioResp.bits,
                        Mux(mshrReplayValid         , mshrs.io.replay.bits,
                        Mux(replace_findValid       , mshrs.io.newFetchreq.bits,
                        Mux(fenceReadValid          , dontCareReq,
                        
                                               io.lsu.req.bits(w).bits)))))))
        } else {
            // 只可能是lsu的请求
            s0req(w) := io.lsu.req.bits(w).bits
        }
    }

    val s0state = Mux(wbValid                       , s_wb,
                  Mux(refillValid                   , s_refill,
                  Mux(fenceClearValid               , s_fence_clear,
                  Mux(mmiou.io.mmioResp.valid       , s_mmio_resp,
                  Mux(mshrReplayValid               , s_replay,
                  Mux(replace_findValid             , s_replace_find,
                  Mux(fenceReadValid                , s_fence_read,
                                                      s_lsu)))))))

    // 做判断接replay请求
    mshrs.io.replay.ready := s0state === s_replay

    // 需要s0pos的一定是单条流水线并且处理cache的请求类型
    val s0pos = Mux(wbValid        , wfu.io.pos,
                Mux(refillValid    , wfu.io.pos,
                Mux(mshrReplayValid, mshrs.io.replaypos,
                Mux(fenceClearValid, wfu.io.pos,
                                     0.U))))

    // fetchReady ，s0的状态一定是refill，
    // 如果取好（refill最后一个字），通告mshr的地址，以及refill到的行号
    val s0fetchReady = wfu.io.fetch_ready
    // 最后一个refill进行到s2到告诉mshr去激活(将表1的waiting转成ready)
    mshrs.io.fetchReady       := s0fetchReady
    mshrs.io.fetchedBlockAddr := getBlockAddr(wfu.io.fetched_addr)
    mshrs.io.fetchedpos       := s0pos

    meta.io.lsuRead zip io.lsu.req.bits map {
        case (m, r) => {
            m.bits.tag     := getTag(r.bits.addr)
            m.bits.idx     := getIdx(r.bits.addr)
            m.bits.isStore := isStore(r.bits)
        }
    }
    
    meta.io.replayRead.bits.tag     := getTag(mshrs.io.replay.bits.addr)
    meta.io.replayRead.bits.idx     := getIdx(mshrs.io.replay.bits.addr)
    meta.io.replayRead.bits.pos     := s0pos
    meta.io.replayRead.bits.isStore := isStore(mshrs.io.replay.bits)
    meta.io.missReplace.bits.idx    := getIdx(mshrs.io.newFetchreq.bits.addr)


    when (s0state === s_lsu) {
        meta.io.lsuRead zip s0valid map { case (m, v) => m.valid := v }
    } .elsewhen(s0state === s_replay) {
        meta.io.replayRead.valid := s0valid(0)
    } .elsewhen (s0state === s_replace_find) {
        meta.io.missReplace.valid := s0valid(0)
    } .elsewhen (s0state === s_refill) {
        // do nothing
    } .elsewhen (s0state === s_wb) {
        // do nothing
    } .elsewhen (s0state === s_mmio_req) {
        // do nothing
    } .elsewhen (s0state === s_mmio_resp){
        // do nothin
    }.elsewhen (s0state === s_fence_read) {
        meta.io.fetchDirty.valid        := true.B
        meta.io.fetchDirty.bits.isFence := true.B
    } .elsewhen (s0state === s_fence_clear) {
        // do nothing
    }

// ================================ s1 ==================================

    // 检查全局，如果此时流水线的s0和s1阶段状态为lsu并且有store，就将其kill掉
    val s2StoreFailed = WireInit(false.B)

    val s1valid = Wire(Vec(memWidth, Bool()))
    for(w <- 0 until memWidth){
        s1valid(w) := RegNext(
            s0valid(w)                                                                                       &&
            !(s0state === s_lsu && (isStore(s0req(w)) && s2StoreFailed))             &&
            !(s0state === s_lsu && (!isStore(s0req(w)) && IsKilledByBranch(io.lsu.brupdate, s0req(w).uop)))    &&
            !(s0state === s_replay && (!isStore(s0req(w)) && IsKilledByBranch(io.lsu.brupdate, s0req(w).uop))) &&
            !(s0state === s_lsu && (!isStore(s0req(w)) && io.lsu.exception))                                   &&
            !(s0state === s_replay && (!isStore(s0req(w)) && io.lsu.exception))
        )
    }

    val s1state = RegNext(s0state)

    mshr_bubble := s1state === s_replace_find

    val s1req = RegNext(s0req)
    // 所有真正的lsu事务都需要在s1阶段进行brmask的更新(mmio_req,replay,lsu)
    for (w <- 0 until memWidth){
        s1req(w).uop.brMask := GetNewBrMask(io.lsu.brupdate,s0req(w).uop)
    }

    // replace_find 时，将此地址发送给 wfu 用于从内存中读取出对应的数据
    val s1_fetch_addr = RegNext(s0req(0).addr)
    wfu.io.req_addr := s1_fetch_addr

    // refill好的信息
    val s1fetchReady = RegNext(s0fetchReady)
    val s1newMeta = RegNext(wfu.io.new_meta)

    // 这个是除了普通lsu请求之外需要的s1pos
    val s1pos = RegNext(s0pos)

    // 这个是给lsu请求看hit路用的
    val s1hitpos = WireInit(0.U.asTypeOf(Vec(memWidth , UInt(log2Ceil(nWays).W))))
    for(w <- 0 until memWidth){
        s1hitpos(w) := meta.io.readResp(w).bits.pos
    }
    // 如果hit,记录下hit的Pos
    val s1hit = WireInit(0.U.asTypeOf(Vec(memWidth , Bool())))


    for(w <- 0 until memWidth){
        data.io.read(w).req.valid := s1valid(w)
        data.io.read(w).req.bits.idx := getIdx(s1req(w).addr)
        data.io.read(w).req.bits.pos := Mux(s1state === s_lsu, s1hitpos(w) , s1pos)
        data.io.read(w).req.bits.offset := getWordOffset(s1req(w).addr)
    }


    when(s1state === s_lsu){
        for(w <- 0 until memWidth){
            when(meta.io.readResp(w).valid){
                //在resp之前，meta内部判断是否是store,如果是store,就要判断是否是readOnly,如果是readOnly,就回传来miss
                when(meta.io.readResp(w).bits.hit || (isSC(s1req(w)) && !io.lsu.llbit )) {
                    s1hit(w) := s1valid(w)
                    // 接下来要去读data
                }.otherwise{
                    s1hit(w) := false.B
                }
            }
        }
    }.elsewhen(s1state === s_replay){
        // 只保存读出的meta，然后根据他自己的信息去读data
    }.elsewhen(s1state === s_replace_find){
        // 直接把mshr的被替换的行的返回结果以及fetchAddr拉给wfu,激活wfu
        wfu.io.req_valid := s1valid(0)
        wfu.io.meta_resp := meta.io.readResp(0).bits
    }.elsewhen(s1state === s_refill){
        // 不用去读什么
    }.elsewhen(s1state === s_wb){
        // 去读data
    }.elsewhen(s1state === s_mmio_req){
        // mmio_req:s1不干活，等着后面发请求给axi
    }.elsewhen(s1state === s_mmio_resp){
        // do nothing
    }.elsewhen(s1state === s_fence_read){
        when(meta.io.readResp(0).bits.rmeta.dirty && meta.io.readResp(0).bits.rmeta.valid){
        // 如果有效的脏行，才把fetchDirty的结果拉给wfu
            wfu.io.req_valid := s1valid(0)
            wfu.io.req_wb_only := true.B
            wfu.io.meta_resp := meta.io.readResp(0).bits
        }
    }

    val s1_hasMMIO = s1req.map(x => isMMIO(x)).reduce(_||_)
    // ================================ s2 ==================================

    val s2state = RegNext(Mux(s1state === s_lsu && s1_hasMMIO, s_mmio_req, s1state))
    val s2req   = RegNext(s1req)

    for (w <- 0 until memWidth){
        s2req(w).uop.brMask := GetNewBrMask(io.lsu.brupdate,s1req(w).uop)
        io.lsu.s2_hit(w) := RegNext(s1hit(w)) // 不看store，因为这个信号是给loadwakeup看的
    }

    val s2valid = WireInit(0.U.asTypeOf(Vec(memWidth , Bool())))
    for(w <- 0 until memWidth){
                        // 上个周期没被kill
        s2valid(w) := RegNext(
            (s1valid(w) && !io.lsu.s1_kill(w))                                                                &&
            !((s1state === s_lsu) && (isStore(s1req(w)) && s2StoreFailed))              &&
            !(s1state === s_lsu && (!isStore(s1req(w)) && IsKilledByBranch(io.lsu.brupdate, s1req(w).uop)))     &&
            !(s1state === s_replay && (!isStore(s1req(w)) && IsKilledByBranch(io.lsu.brupdate, s1req(w).uop)))  &&
            !(s1state === s_lsu && (!isStore(s1req(w)) && io.lsu.exception)) &&
            !(s1state === s_replay && (!isStore(s1req(w)) && io.lsu.exception))
        ) && (
            // s2周期没有被kill才行，s2周期被kill的只可能分支kill,s2storeFailed本身不会对自己kill
            !(s2state === s_lsu && (!isStore(s2req(w)) && IsKilledByBranch(io.lsu.brupdate, s2req(w).uop))) &&
            !(s2state === s_replay && (!isStore(s2req(w)) && IsKilledByBranch(io.lsu.brupdate, s2req(w).uop))) &&
            !(s2state === s_lsu && (!isStore(s2req(w)) && io.lsu.exception)) &&
            !(s2state === s_replay && (!isStore(s2req(w)) && io.lsu.exception))
        )
    }

    val s2enter_missStore = WireInit(false.B)

    val s2hit = WireInit(0.U.asTypeOf(Vec(memWidth , Bool())))
    for(w <- 0 until memWidth){
        // 对于lsu的store请求，要现在mshr里面找，如果有store，就当作miss处理
        s2hit(w) := RegNext(s1hit(w) && 
                            // 在lsu状态下的store，以下情况需要转变为false
                                !(s1state === s_lsu && isStore(s1req(w)) &&
                                    (
                                        // s1的时候看到mshrs有store
                                        mshrs.io.hasStore ||
                                        // 看到s2即将向mshr成功存入一个miss的store（实际上是hasStore信号由于延迟一周期的转发）
                                        s2enter_missStore
                                    )
                                 )
                            )
    }


    //用于对于没有dirty的fence的指示信号，直接清除掉对应行
    val s2fence_read_not_dirtyline = RegNext(!meta.io.readResp(0).bits.rmeta.dirty || !meta.io.readResp(0).bits.rmeta.valid)
    val s2fence_read_idx = RegNext(meta.io.readResp(0).bits.idx)
    val s2fence_read_pos = RegNext(meta.io.readResp(0).bits.pos)


    // 其他状态的pos (对于mshr分配到的pos,或者fetchDirty的行，在s1才拿到自己分到的pos，这里要及时更新)
    val s2pos = RegNext(Mux(s1state === s_replace_find, meta.io.readResp(0).bits.pos, s1pos))

    // lsu请求下hit的pos
    val s2hitpos = RegNext(s1hitpos)

    // lsu阶段，如果发生了miss，将由missArbiter来决定写入mshr行为
    val missArbiter = Module(new Missarbiter)
    missArbiter.io.req  := s2req
    missArbiter.io.miss := s2hit.map(x => !x)
    for (w <- 0 until memWidth) {
        missArbiter.io.alive(w) := s2valid(w) && s2state === s_lsu
    }
    missArbiter.io.mshrReq <> mshrs.io.req
    // 此时的s2 为lsu状态（区分mmioresp）即将向mshr成功存入一个miss的store
    s2enter_missStore := missArbiter.io.store_enter_mshr


    // fetchAddr成功送给axi之后,就可以将告诉mshr去fetching状态转换
    val s2fetchAddr    = RegNext(s1_fetch_addr)
    val s2fetchReady   = RegNext(s1fetchReady)
    val s2newMeta      = RegNext(s1newMeta)

    val sendResp = WireInit(0.U.asTypeOf(Vec(memWidth,Bool())))
    val sendNack = WireInit(0.U.asTypeOf(Vec(memWidth,Bool())))

    // 按情况返回resp或nack(replay不用看kill，lsukiil自然为假)
    for(w <- 0 until memWidth){
        io.lsu.nack(w).valid := sendNack(w)
        io.lsu.resp(w).valid := sendResp(w)
    }
    for (w <- 0 until memWidth) {
        io.lsu.nack(w).bits      := s2req(w)
        io.lsu.resp(w).bits.uop  := s2req(w).uop
        io.lsu.resp(w).bits.data := DontCare
    }

    when (s2state === s_lsu) {
        sendNack := missArbiter.io.sendNack
        sendResp := missArbiter.io.sendResp

        s2StoreFailed := missArbiter.io.storeFailed

        // 以上将所有中途kill或者miss的请求都处理完了，接下来处理hit且活着的请求
        for (w <- 0 until memWidth) {
            when (s2hit(w)) {
                sendResp(w) := s2valid(w)
                sendNack(w) := false.B

                when (isStore(s2req(w))) {
                    // meta,data写操作
                    // meta拉高dirty位
                    meta.io.lsuWrite.valid    := s2valid(w)
                    meta.io.lsuWrite.bits.idx := getIdx(s2req(w).addr)
                    meta.io.lsuWrite.bits.pos := s2hitpos(w)

                    meta.io.lsuWrite.bits.setdirty.valid := true.B
                    meta.io.lsuWrite.bits.setdirty.bits  := true.B

                    // 由于一次写的粒度是一个字 因此需要读出原来的数据 在此基础上修改得到新的数据
                    val rdata = data.io.read(w).resp.bits.data
                    val wdata = WordWrite(s2req(w) , rdata)

                    /* data.io.lsuWrite.req.valid       := Mux(isSC(s2req(w)), s2valid(w) && io.lsu.llbit, s2valid(w)) */
                    data.io.write.req.valid       := s2valid(w) && !(isSC(s2req(w)) && !io.lsu.llbit)
                    data.io.write.req.bits.idx    := getIdx(s2req(w).addr)
                    data.io.write.req.bits.pos    := s2hitpos(w)
                    data.io.write.req.bits.offset := getWordOffset(s2req(w).addr)
                    data.io.write.req.bits.data   := wdata

                    io.lsu.resp(w).bits.data := io.lsu.llbit.asUInt // only sc can write regfile
                } .otherwise {
                    /* io.lsu.resp(w).bits.data := data.io.lsuRead(w).resp.bits.data */
                    io.lsu.resp(w).bits.data := loadDataGen(
                        s2req(w).addr(1, 0),
                        data.io.read(w).resp.bits.data,
                        s2req(w).uop.mem_size,
                        s2req(w).uop.mem_signed
                    )
                }
            }
        }
    } .elsewhen (s2state === s_replay) {
        // 统一在0号位处理
        sendResp(0) := s2valid(0)
        sendNack(0) := false.B
        sendResp(1) := false.B
        sendNack(1) := false.B

        // 只要s2valid(0)就是hit
        when (isStore(s2req(0))) {
            // meta拉高dirty位
            meta.io.replayWrite.valid    := s2valid(0)
            meta.io.replayWrite.bits.idx := getIdx(s2req(0).addr)
            meta.io.replayWrite.bits.pos := s2pos

            meta.io.replayWrite.bits.setdirty.valid := true.B
            meta.io.replayWrite.bits.setdirty.bits  := true.B

            val replay_rdata = data.io.read(0).resp.bits.data
            val replay_wdata = WordWrite(s2req(0), replay_rdata)
            // data执行写操作
            /* data.io.replayWrite.req.valid := Mux(isSC(s2req(0)) , s2valid(0) && io.lsu.llbit , s2valid(0)) */
            data.io.write.req.valid       := s2valid(0) && !(isSC(s2req(0)) && !io.lsu.llbit)
            data.io.write.req.bits.idx    := getIdx(s2req(0).addr)
            data.io.write.req.bits.pos    := s2pos
            data.io.write.req.bits.offset := getWordOffset(s2req(0).addr)
            data.io.write.req.bits.data   := replay_wdata

            io.lsu.resp(0).bits.data := io.lsu.llbit.asUInt
        }.otherwise {
            /* io.lsu.resp(0).bits.data := data.io.replayRead.resp.bits.data */
            io.lsu.resp(0).bits.data := loadDataGen(
                s2req(0).addr(1, 0),
                data.io.read(0).resp.bits.data,
                s2req(0).uop.mem_size,
                s2req(0).uop.mem_signed
            )
        }
    } .elsewhen (s2state === s_replace_find) {
        // 激活wfu在1阶段就做完了
        // 不管readOnly，而是直接清除这一行
        meta.io.replaceLogout.valid        := s2valid(0)
        meta.io.replaceLogout.bits.setvalid.valid := true.B
        meta.io.replaceLogout.bits.setvalid.bits  := false.B
        
        meta.io.replaceLogout.bits.idx := getIdx(s2req(0).addr)
        meta.io.replaceLogout.bits.pos := s2pos
    } .elsewhen (s2state === s_fence_read) {
        // 对于脏的行，激活wfu在1阶段就做完了,但是对于那些没有脏的，或者无效的行，此处需要由fence_read动用lineClear
        when (s2fence_read_not_dirtyline) {
            meta.io.lineClear.valid        := s2valid(0)
            meta.io.lineClear.bits.isFence := true.B

            meta.io.lineClear.bits.idx := s2fence_read_idx
            meta.io.lineClear.bits.pos := s2fence_read_pos

            meta.io.lineClear.bits.setdirty.valid := true.B
            meta.io.lineClear.bits.setdirty.bits  := false.B

            // 彻底清除这一行
            meta.io.lineClear.bits.setvalid.valid := true.B
            meta.io.lineClear.bits.setvalid.bits  := false.B

            meta.io.lineClear.bits.setreadOnly.valid := true.B
            meta.io.lineClear.bits.setreadOnly.bits  := false.B

            meta.io.lineClear.bits.setfixed.valid := true.B
            meta.io.lineClear.bits.setfixed.bits  := false.B

            meta.io.lineClear.bits.setTag.valid := true.B
            meta.io.lineClear.bits.setTag.bits  := 0.U
        }
    } .elsewhen (s2state === s_wb) {  // write back to memory
        // 将读到的data给wfu
        wfu.io.wfu_read_resp.valid     :=  data.io.read(0).resp.valid
        wfu.io.wfu_read_resp.bits.data :=  data.io.read(0).resp.bits.data

    } .elsewhen (s2state === s_refill) {
        // 去写data,这里不是st请求，而是refill的内部事务，因此不需要做llbit的判断
        data.io.write.req.valid       := s2valid(0)
        data.io.write.req.bits.idx    := getIdx(s2req(0).addr)
        data.io.write.req.bits.pos    := s2pos
        data.io.write.req.bits.offset := getWordOffset(s2req(0).addr)
        data.io.write.req.bits.data   := s2req(0).data

        // s2fetchReady 的时候,所有data将要写完了,这个时候可以将新的meta写入
        when (s2fetchReady) {
            // 告诉meta.io.wfuWrite要写入的行号
            meta.io.wfuWrite.valid    := s2valid(0)
            meta.io.wfuWrite.bits.idx := getIdx(s2req(0).addr)
            meta.io.wfuWrite.bits.pos := s2pos

            meta.io.wfuWrite.bits.setvalid.valid := true.B
            meta.io.wfuWrite.bits.setvalid.bits  := s2newMeta.valid

            meta.io.wfuWrite.bits.setdirty.valid := true.B
            meta.io.wfuWrite.bits.setdirty.bits  := s2newMeta.dirty

            meta.io.wfuWrite.bits.setreadOnly.valid := true.B
            meta.io.wfuWrite.bits.setreadOnly.bits  := s2newMeta.readOnly

            // 解除fixed
            meta.io.wfuWrite.bits.setfixed.valid := true.B
            meta.io.wfuWrite.bits.setfixed.bits  := s2newMeta.fixed

            // 将新的tag写入
            meta.io.wfuWrite.bits.setTag.valid := true.B
            meta.io.wfuWrite.bits.setTag.bits  := s2newMeta.tag
        }
    } .elsewhen (s2state === s_mmio_req) {

        for(w <- 0 until memWidth){
            io.lsu.resp(w).bits.data := io.lsu.llbit.asUInt
        }

        when(s2req.map(x => isSC(x)).reduce(_||_) && !io.lsu.llbit){
            for(w <- 0 until memWidth){
                sendResp(w) := s2valid(w)
                sendNack(w) := false.B
            }
        }.otherwise {
            // 查pipeline，要求lsu发送的时候0号或1号是mmio请求，不会同时发两个
            val mmio_req = Mux(s2valid(0), s2req(0), s2req(1))
            mmiou.io.mmioReq.valid := s2valid(0) || s2valid(1)
            // 存入请求本身
            mmiou.io.mmioReq.bits  := mmio_req

            when (!mmiou.io.mmioReq.fire) {
                assert(!(mmiou.io.mmioReq.valid && !isStore(mmio_req)),
                    "a mmio load request is sent but there is another store in mmio unit")
                // 当前不接受，说明busy，这种情况一定是store，发nack，然后拉高storeFailed
                // 0号发nack
                sendResp(0) := false.B
                sendNack(0) := s2valid(0)
                sendResp(1) := false.B
                sendNack(1) := s2valid(1)
            }
            s2StoreFailed := mmiou.io.mmioReq.valid && !mmiou.io.mmioReq.ready
        }
    } .elsewhen(s2state === s_mmio_resp){
        // 准备返回resp(0号)
        sendResp(0) := true.B // 因为mmio不可能被branch kill
        sendNack(0) := false.B
        sendResp(1) := false.B
        sendNack(1) := false.B
        // data 以 s2req(0) 为载体
        io.lsu.resp(0).bits.data := s2req(0).data
    }.elsewhen (s2state === s_fence_clear) {
        // 清除对应的meta行的dirty位,以及valid 位
        meta.io.lineClear.valid        := s2valid(0)
        meta.io.lineClear.bits.isFence := true.B

        meta.io.lineClear.bits.idx := getIdx(s2req(0).addr)
        meta.io.lineClear.bits.pos := s2pos

        meta.io.lineClear.bits.setdirty.valid := true.B
        meta.io.lineClear.bits.setdirty.bits  := false.B

        // 彻底清除这一行
        meta.io.lineClear.bits.setvalid.valid := true.B
        meta.io.lineClear.bits.setvalid.bits  := false.B

        meta.io.lineClear.bits.setreadOnly.valid := true.B
        meta.io.lineClear.bits.setreadOnly.bits  := false.B

        meta.io.lineClear.bits.setfixed.valid := true.B
        meta.io.lineClear.bits.setfixed.bits  := false.B

        meta.io.lineClear.bits.setTag.valid := true.B
        meta.io.lineClear.bits.setTag.bits  := 0.U
    }.elsewhen(s2state === s_nil) {
        //
    }

    if(!FPGAPlatform){
        val difftest = Module(new DifftestStoreEvent)
        // difftest
        val isRealStoreState = (s2state === s_lsu || s2state === s_replay || mmiou.io.mmioResp.fire)
        //{4'b0, llbit && sc_w, st_w, st_h, st_b}
        val sc_w =  isRealStoreState && io.lsu.resp(0).valid && io.lsu.resp(0).bits.uop.is_sc
        val st_w =  isRealStoreState && io.lsu.resp(0).valid && io.lsu.resp(0).bits.uop.use_stq &&  io.lsu.resp(0).bits.uop.mem_size === 2.U
        val st_h =  isRealStoreState && io.lsu.resp(0).valid && io.lsu.resp(0).bits.uop.use_stq &&  io.lsu.resp(0).bits.uop.mem_size === 1.U
        val st_b =  isRealStoreState && io.lsu.resp(0).valid && io.lsu.resp(0).bits.uop.use_stq &&  io.lsu.resp(0).bits.uop.mem_size === 0.U
        // disable now
        difftest.io.valid := 0.U & VecInit(Cat((0.U(4.W)), io.lsu.llbit && sc_w, st_w, st_h, st_b)).asUInt
        difftest.io.clock := clock
        difftest.io.coreid := 0.U // only support 1 core now
        difftest.io.index := 0.U
        difftest.io.storePAddr := Mux(mmiou.io.mmioResp.fire, mmiou.io.mmioResp.bits.addr , s2req(0).addr)
        difftest.io.storeVAddr := 0.U
        difftest.io.storeData := Mux(mmiou.io.mmioResp.fire,WordWrite(mmiou.io.mmioResp.bits, 0.U(32.W))
                                                                                        ,WordWrite(s2req(0), 0.U(32.W)))
    }

    wfuReady  := wfu.io.ready &&
                 (s1state =/= s_fence_read && s1state =/= s_fence_clear && s1state =/= s_replace_find && s1state =/= s_wb && s1state =/= s_refill) &&
                 (s2state =/= s_fence_read && s2state =/= s_fence_clear && s2state =/= s_replace_find && s2state =/= s_wb && s2state =/= s_refill)
    // TODO enable continuous hit store forwarding(DONE)
    // TODO simplify the IO channel of DCacheData and DCacheMeta(DONE)

    // TODO mmio(DONE)
    // TODO reconstrcut the DCache(DONE)
    // TODO lr/sc
}
