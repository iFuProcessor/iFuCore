package iFu.frontend

import chisel3._
import chisel3.util._
import iFu.sma._
import iFu.common._
import iFu.common.Consts._
import iFu.util._
import iFu.tlb._
import iFu.backend.{PreDecode, PreDecodeSignals}
import iFu.frontend.FrontendUtils._

class F3Pack extends CoreBundle {
    // parameters
    val fetchWidth = frontendParams.fetchWidth

    val pc         = UInt(vaddrBits.W)
    val instrs     = Vec(fetchWidth, UInt(coreInstrBits.W))
    val mask       = UInt(fetchWidth.W)
    val exception  = Valid(new ITLBException)
    val rasPtr     = new RASPtr
    val predecode  = Vec(fetchWidth, new PreDecodeSignals)
}

class FetchBundle extends CoreBundle {
    // parameters
    val fetchWidth         = frontendParams.fetchWidth
    val fetchBytes         = frontendParams.fetchBytes
    val numFTQEntries      = frontendParams.numFTQEntries


    val pc              = UInt(vaddrBits.W) // fetch PC, possibly unaligned.
    val instrs          = Vec(fetchWidth, Bits(coreInstrBits.W))

    val cfiIdx    = Valid(UInt(log2Ceil(fetchWidth).W))
    val cfiType   = UInt(CFI_SZ.W)
    val cfiIsCall = Bool()
    val cfiIsRet  = Bool()

    val targetSz  = frontendParams.targetSz
    val rasTop    = UInt(targetSz.W)
    val ftqIdx    = UInt(log2Ceil(numFTQEntries).W)
    val mask      = UInt(fetchWidth.W)    // the purpose and specific details of the mask need more information, check later
    val brMask    = UInt(fetchWidth.W)
    val rasPtr    = new RASPtr
    val exception = Valid(new ITLBException)
    val bpdMeta   = Vec(fetchWidth, new PredictionMeta)
}

class FrontendTLBDataIO extends CoreBundle {
    val r_req   = Valid(new TLBDataRReq)
    val r_resp  = Flipped(Valid(new TLBDataRResp))
}

class FrontendCsrIO extends CoreBundle {
    val itlb_csr_cxt = Input(new TLBCsrContext)
}

class FrontendToCoreIO extends CoreBundle {
    val numFTQEntries   = frontendParams.numFTQEntries

    val tlb_data        = new FrontendTLBDataIO
    val csr             = new FrontendCsrIO

    val fetchPacket     = new DecoupledIO(new FetchBufferResp)

    // 1 for xcpt/jalr/auipc/flush
    val getFtqPc        = Vec(2, new GetPCFromFtqIO)

    val brupdate        = Input(new BrUpdateInfo)

    // Redirects change the PC
    val redirect_flush  = Input(Bool())
    val redirect_val    = Input(Bool())
    val redirect_pc     = Input(UInt())    //分支指令的结果
    val redirect_ftq_idx= Input(UInt())
    val redirect_ras_ptr= Input(new RASPtr)

    val commit          = Flipped(Valid(UInt(numFTQEntries.W)))
    val flush_icache    = Input(Bool())
}

class FrontendIO extends CoreBundle {
    val core = new FrontendToCoreIO

    val smar = new SMAR
}

class Frontend extends CoreModule {
// --------------------------------------------------------
// Parameters and constants
    val fetchWidth    = frontendParams.fetchWidth
    val numRasEntries = frontendParams.bpdParams.numRasEntries
    val fetchBytes    = frontendParams.fetchBytes
    val instrBytes    = frontendParams.instrBytes

    def getTargetPC(pc: UInt , target : UInt): UInt = {
        frontendParams.getTargetPC(pc, target)
    }
    def getTarget(tgtpc : UInt): UInt = frontendParams.getTarget(tgtpc)
// --------------------------------------------------------
    val io = IO(new FrontendIO)

    // Module definition
    val bpd          = Module(new BranchPredictor)
    val ras          = Module(new RAS)  // TODO: should ras be a part of bpd?
    val icache       = Module(new ICache(frontendParams.iCacheParams))
    val itlb          = Module(new ITLB)
    val fetch_buffer = Module(new FetchBuffer)
    val ftq          = Module(new FetchTargetQueue)

    io.core.tlb_data.r_req := itlb.io.r_req
    itlb.io.r_resp         := io.core.tlb_data.r_resp
    itlb.io.itlb_csr_cxt   := io.core.csr.itlb_csr_cxt

    io.smar <> icache.io.smar

    icache.io.invalidate := io.core.flush_icache
// --------------------------------------------------------
// Stage 0 -> select next pc, send request to icache, bpd
    // s0 is not a real stage, it is hidden in s1, so below variables are wire
    val s0_valid   = WireInit(false.B)
    val s0_vpc     = WireInit(0.U(vaddrBits.W))
    val s0_ras_ptr = WireInit(0.U.asTypeOf(new RASPtr))

    if(!FPGAPlatform)dontTouch(s0_valid)
    if(!FPGAPlatform)dontTouch(s0_vpc)
    if(!FPGAPlatform)dontTouch(s0_ras_ptr)

    // the first cycle after reset, the frontend will fetch from resetPC
    when (RegNext(reset.asBool) && !reset.asBool) {
        s0_valid   := true.B
        s0_vpc     := resetPC.U(vaddrBits.W)
        s0_ras_ptr := 0.U.asTypeOf(new RASPtr)
    }

    icache.io.req.valid     := s0_valid
    icache.io.req.bits.addr := s0_vpc

    bpd.io.f0req.valid      := s0_valid
    bpd.io.f0req.bits.pc    := s0_vpc
// --------------------------------------------------------
    val f1_clear   = WireInit(false.B)
    val s1_valid   = RegNext(s0_valid, false.B)
    val s1_vpc     = RegNext(s0_vpc)
    val s1_ras_ptr = RegNext(s0_ras_ptr)

    if(!FPGAPlatform)dontTouch(f1_clear)
    if(!FPGAPlatform)dontTouch(s1_valid)
    if(!FPGAPlatform)dontTouch(s1_vpc)
    if(!FPGAPlatform)dontTouch(s1_ras_ptr)
// --------------------------------------------------------
// Stage 1 -> send paddr to icache, and use bpd.f1 to predict next pc
    itlb.io.req.valid      := s1_valid
    itlb.io.req.bits.vaddr := s1_vpc
    val f1_tlb_resp = itlb.io.resp

    // send paddr to icache
    icache.io.s1_paddr := f1_tlb_resp.paddr
    icache.io.s1_kill  := itlb.io.resp.exception.valid || f1_clear

    // branch prediction
    val f1_bpd_resp = bpd.io.resp.f1
    val f1_do_redirect = f1_bpd_resp.predInfos.takens.asUInt.orR
    val f1_tgt = f1_bpd_resp.predInfos.predicted_target.bits

    if(!FPGAPlatform)dontTouch(f1_tgt)

    // if current cycle is valid, use the predicted target as the next fetch pc
    when (s1_valid) {
        s0_valid   := !f1_tlb_resp.exception.valid
        s0_vpc     := Mux(
            f1_do_redirect,
            getTargetPC(s1_vpc, f1_tgt),
            nextFetch(s1_vpc)
        )
        s0_ras_ptr := s1_ras_ptr
    }
// --------------------------------------------------------
    val f2_clear    = WireInit(false.B)
    val s2_valid    = RegNext(s1_valid && !f1_clear, false.B)
    val s2_vpc      = RegNext(s1_vpc)
    val s2_ras_ptr  = Reg(new RASPtr)
    val s2_tlb_resp = RegNext(f1_tlb_resp)

    if(!FPGAPlatform)dontTouch(f2_clear)
    if(!FPGAPlatform)dontTouch(s2_valid)
    if(!FPGAPlatform)dontTouch(s2_vpc)
    if(!FPGAPlatform)dontTouch(s2_ras_ptr)
    if(!FPGAPlatform)dontTouch(s2_tlb_resp)
// --------------------------------------------------------
// Stage 2 -> use bpd.f2 to redirect(if needed)
    // handle tlb exceptions
    icache.io.s2_kill := s2_valid && s2_tlb_resp.exception.valid

    // set the default value
    s2_ras_ptr := s1_ras_ptr

    // branch prediction
    val f2_bpd_resp = bpd.io.resp.f2
    val f2_do_redirect = f2_bpd_resp.predInfos.takens.asUInt.orR
    val f2_tgt = f2_bpd_resp.predInfos.predicted_target.bits
    // f2综合得出的预测目标地址
    val f2_predicted_target_pc = Mux(
        f2_do_redirect,
        getTargetPC(s2_vpc, f2_tgt),
        nextFetch(s2_vpc)
    )
    if(!FPGAPlatform)dontTouch(f2_tgt)

    val f2_correct_f1_tgt = !IsEqual(getTarget(s1_vpc), getTarget(f2_predicted_target_pc))
    // note: s0 is not a real stage
    val f3_ready = Wire(Bool())
    when (
        (s2_valid && !icache.io.resp.valid) ||  // cache miss
        (s2_valid && icache.io.resp.valid && !f3_ready) // f3 full
    ) {
        s0_valid     := !s2_tlb_resp.exception.valid
        s0_vpc       := s2_vpc
        s0_ras_ptr   := s2_ras_ptr
        f1_clear     := true.B
    } .elsewhen (s2_valid && f3_ready) {
        when (s1_valid && !f2_correct_f1_tgt) {
            s2_ras_ptr := s2_ras_ptr
        }
        when ((s1_valid && f2_correct_f1_tgt) || !s1_valid) {
            // redirect, next cycle, s2_ras_ptr is meaningless, so we don't care
            s0_valid   := !s2_tlb_resp.exception.valid
            s0_vpc     := f2_predicted_target_pc
            s0_ras_ptr := s2_ras_ptr
            f1_clear   := true.B
        }
    }
// --------------------------------------------------------
    val f3_clear = WireInit(false.B)
    val f3_ifu_resp = withReset(reset.asBool || f3_clear) {
        Module(new Queue(new F3Pack, 1, pipe = true, flow = false))
    }
    val f3_bpd_resp = withReset(reset.asBool || f3_clear) {
        Module(new Queue(new BranchPredictionBundle, 1, pipe = true, flow = true))
    }
    val ras_read_idx = RegInit(0.U(log2Ceil(numRasEntries).W))
// --------------------------------------------------------
// Stage 3 ->
    // predecode
    val predecoders = Seq.fill(fetchWidth) { Module(new PreDecode) }
    val instrs = VecInit((0 until fetchWidth).map(i => icache.io.resp.bits.data(i * coreInstrBits + coreInstrBits - 1, i * coreInstrBits)))
    (0 until fetchWidth).foreach(i => {
        val pc = getPc(s2_vpc, i.U)
        predecoders(i).io.pc := pc
        predecoders(i).io.instr := instrs(i)
    })
    // do enqueue
    f3_ifu_resp.io.enq.valid := (
        s2_valid && !f2_clear && (          // stage 2 is valid and (
            icache.io.resp.valid ||         //     icache return instrctions or
            s2_tlb_resp.exception.valid     //     tlb exception happens
        )                                   // )
    )
    f3_ifu_resp.io.enq.bits.pc        := s2_vpc
    f3_ifu_resp.io.enq.bits.instrs    := instrs
    f3_ifu_resp.io.enq.bits.rasPtr    := s2_ras_ptr
    f3_ifu_resp.io.enq.bits.mask      := fetchMask(s2_vpc)
    f3_ifu_resp.io.enq.bits.exception := s2_tlb_resp.exception
    f3_ifu_resp.io.enq.bits.predecode := VecInit(predecoders.map(_.io.out))

    f3_ready := f3_ifu_resp.io.enq.ready

    // send request to ras, use ras_read_idx for replay(when f3 is full)
    ras.io.read_idx := ras_read_idx
    when (f3_ifu_resp.io.enq.fire) {
        ras_read_idx    := f3_ifu_resp.io.enq.bits.rasPtr.bits
        ras.io.read_idx := f3_ifu_resp.io.enq.bits.rasPtr.bits
    }

    f3_bpd_resp.io.enq.valid := (
        RegNext(f3_ifu_resp.io.enq.ready) &&    // f3 is not full
        f3_ifu_resp.io.deq.valid                // f4 is not full
    )
    f3_bpd_resp.io.enq.bits  := bpd.io.resp.f3

    val f4_ready = Wire(Bool())
    f3_ifu_resp.io.deq.ready := f4_ready
    f3_bpd_resp.io.deq.ready := f4_ready

    val f3_fetchResp    = f3_ifu_resp.io.deq.bits
    val f3_aligned_pc   = fetchAlign(f3_fetchResp.pc)
    val f3_redirects    = Wire(Vec(fetchWidth, Bool()))
    val f3_tgt_pcs      = Wire(Vec(fetchWidth, UInt(vaddrBits.W)))
    val f3_cfi_types    = Wire(Vec(fetchWidth, UInt(CFI_SZ.W)))
    val f3_fetch_bundle = Wire(new FetchBundle)
    val f3_mask         = Wire(Vec(fetchWidth, Bool()))
    val f3_br_mask      = Wire(Vec(fetchWidth, Bool()))
    val f3_call_mask    = Wire(Vec(fetchWidth, Bool()))
    val f3_ret_mask     = Wire(Vec(fetchWidth, Bool()))

    if(!FPGAPlatform)dontTouch(f3_tgt_pcs)

    var redirect_found = false.B
    for (i <- 0 until fetchWidth) {
        val brsigs = f3_fetchResp.predecode(i)

        f3_fetch_bundle.instrs(i) := f3_fetchResp.instrs(i)
        f3_mask(i) := f3_ifu_resp.io.deq.valid && f3_fetchResp.mask(i) && !redirect_found
        f3_tgt_pcs(i) := Mux(
            brsigs.cfiType === CFI_JIRL,
            getTargetPC(f3_fetchResp.pc, f3_bpd_resp.io.deq.bits.predInfos.tgts(i).bits),
            brsigs.target   // surely right
        )

        /**
            s3阶段重定向的条件
            1. 是jal/jalr指令
            2. 是条件分支指令并被预测跳转
         */
        f3_redirects(i) := f3_mask(i) && (
            brsigs.cfiType === CFI_BL || brsigs.cfiType === CFI_JIRL ||
            (brsigs.cfiType === CFI_BR && f3_bpd_resp.io.deq.bits.predInfos.takens(i))
        )
        f3_br_mask(i)   := f3_mask(i) && brsigs.cfiType === CFI_BR
        f3_cfi_types(i) := brsigs.cfiType
        f3_call_mask(i) := brsigs.isCall
        f3_ret_mask(i)  := brsigs.isRet

        redirect_found = redirect_found || f3_redirects(i)
    }

    f3_fetch_bundle.mask         := f3_mask.asUInt
    f3_fetch_bundle.brMask       := f3_br_mask.asUInt
    f3_fetch_bundle.pc           := f3_fetchResp.pc
    f3_fetch_bundle.ftqIdx       := 0.U
    f3_fetch_bundle.exception    := f3_fetchResp.exception
    f3_fetch_bundle.cfiIdx.valid := f3_redirects.reduce(_||_)
    f3_fetch_bundle.cfiIdx.bits  := PriorityEncoder(f3_redirects)
    f3_fetch_bundle.cfiType      := f3_cfi_types(f3_fetch_bundle.cfiIdx.bits)
    f3_fetch_bundle.cfiIsCall    := f3_call_mask(f3_fetch_bundle.cfiIdx.bits)
    f3_fetch_bundle.cfiIsRet     := f3_ret_mask(f3_fetch_bundle.cfiIdx.bits)
    f3_fetch_bundle.rasPtr       := f3_ifu_resp.io.deq.bits.rasPtr
    f3_fetch_bundle.rasTop       := ras.io.read_tgt
    f3_fetch_bundle.bpdMeta      := f3_bpd_resp.io.deq.bits.meta


    val f3_ras_tgt_pc = getTargetPC(f3_fetchResp.pc, ras.io.read_tgt)

    // s3综合得出的预测目标地址
    val f3_predicted_target_pc = Mux(
        f3_redirects.reduce(_||_),
            Mux(f3_fetch_bundle.cfiIsRet,
                f3_ras_tgt_pc,
                f3_tgt_pcs(PriorityEncoder(f3_redirects))
            ),
            nextFetch(f3_fetch_bundle.pc)
    )

    if(!FPGAPlatform)dontTouch(f3_predicted_target_pc)

    val f3_predicted_ras_ptr = f3_fetch_bundle.rasPtr.update(
        f3_fetch_bundle.cfiIdx.valid,
        f3_fetch_bundle.cfiIsCall,
        f3_fetch_bundle.cfiIsRet
    )

    ras.io.write_valid := false.B
    ras.io.write_tgt  := getTarget(getPc(f3_aligned_pc, f3_fetch_bundle.cfiIdx.bits) + 4.U)
    ras.io.write_idx  := WrapInc(f3_fetch_bundle.rasPtr.bits, numRasEntries)

    // 三阶段必须保证译出的地址完全正确
    val f3_correct_f1_tgt = !IsEqual((s1_vpc), (f3_predicted_target_pc))
    val f3_correct_f2_tgt = !IsEqual((s2_vpc), (f3_predicted_target_pc))

    when (f3_ifu_resp.io.deq.valid && f4_ready) {
        when (f3_fetch_bundle.cfiIsCall && f3_fetch_bundle.cfiIdx.valid){
            ras.io.write_valid := true.B
        }

        when (s2_valid && !f3_correct_f2_tgt) {
            f3_ifu_resp.io.enq.bits.rasPtr := f3_predicted_ras_ptr
        } .elsewhen (!s2_valid && s1_valid && !f3_correct_f1_tgt) {
            s2_ras_ptr := f3_predicted_ras_ptr
        } .elsewhen (
            (s2_valid && f3_correct_f2_tgt)              ||
            (!s2_valid && s1_valid && f3_correct_f1_tgt) ||
            (!s2_valid && !s1_valid)
        ) {
            f2_clear     := true.B
            f1_clear     := true.B
            s0_valid     := !f3_fetch_bundle.exception.valid
            s0_vpc       := f3_predicted_target_pc
            s0_ras_ptr   := f3_predicted_ras_ptr
        }
    }

    if(!FPGAPlatform)dontTouch(f2_correct_f1_tgt)
    if(!FPGAPlatform)dontTouch(f3_correct_f1_tgt)
    if(!FPGAPlatform)dontTouch(f3_correct_f2_tgt)
    if(!FPGAPlatform)dontTouch(f1_tgt)
    if(!FPGAPlatform)dontTouch(f2_tgt)
    if(!FPGAPlatform)dontTouch(f3_predicted_target_pc)
// -------------------------------------------------------
    val f4_clear = WireInit(false.B)
    val f4 = withReset(reset.asBool || f4_clear) {
        Module(new Queue(new FetchBundle, 1, pipe = true, flow = false))
    }
// -------------------------------------------------------
    f4_ready        := f4.io.enq.ready
    f4.io.enq.valid := f3_ifu_resp.io.deq.valid && !f3_clear
    f4.io.enq.bits  := f3_fetch_bundle
    f4.io.deq.ready := fetch_buffer.io.enq.ready && ftq.io.enq.ready

    fetch_buffer.io.enq.valid             := f4.io.deq.valid && ftq.io.enq.ready
    fetch_buffer.io.enq.bits              := f4.io.deq.bits
    fetch_buffer.io.enq.bits.ftqIdx       := ftq.io.enqIdx

    ftq.io.enq.valid := f4.io.deq.valid && fetch_buffer.io.enq.ready
    ftq.io.enq.bits  := f4.io.deq.bits

    // bpd update information
    bpd.io.update := ftq.io.bpdUpdate

    // ras update(from ftq, redirect happened, will override the ras update in stage 3)
    when (ftq.io.rasUpdate) {
        ras.io.write_valid := true.B
        ras.io.write_idx   := ftq.io.rasUpdateIdx
        ras.io.write_tgt  := ftq.io.rasUpdate_tgt
    }
// -------------------------------------------------------

// -------------------------------------------------------
// Stage 5 -> no a real stage, connect to core
    io.core.fetchPacket <> fetch_buffer.io.deq
    io.core.getFtqPc    <> ftq.io.getFtqpc

    ftq.io.deq            := io.core.commit
    ftq.io.brUpdate       := io.core.brupdate
    ftq.io.redirect.valid := io.core.redirect_val
    ftq.io.redirect.bits  := io.core.redirect_ftq_idx

    fetch_buffer.io.clear := false.B

    // handle redirect from core
    when (io.core.redirect_flush) {
        fetch_buffer.io.clear := true.B

        f4_clear := true.B
        f3_clear := true.B
        f2_clear := true.B
        f1_clear := true.B

        s0_valid     := io.core.redirect_val
        s0_vpc       := io.core.redirect_pc
        s0_ras_ptr     := io.core.redirect_ras_ptr
    }
// --------------------------------------------------------
}
