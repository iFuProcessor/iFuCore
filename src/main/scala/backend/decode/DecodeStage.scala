package iFu.backend

import chisel3._
import chisel3.util._

import iFu.common._

import iFu.frontend.FetchBufferResp

class DecodeStageIO extends CoreBundle {
    val enq         = Flipped(Decoupled(new FetchBufferResp))
    val deq         = Vec(coreWidth, Valid(new MicroOp))
    val intrpt      = Input(Bool())
    val flush       = Input(Bool())
    val clear       = Input(Bool())
    val rollback    = Input(Bool())
    val dis_ready   = Input(Bool())
    val b1_mispred  = Input(Bool())
    val br_update   = Input(new BrUpdateInfo)
    val xcpt_ftqIdx = Decoupled(UInt(log2Ceil(frontendParams.numFTQEntries).W))
}

class DecodeStage extends CoreModule {
    val io = IO(new DecodeStageIO)

    private val finished = RegInit(0.U(coreWidth.W))

    private val valids = Wire(Vec(coreWidth, Bool()))
    private val uops   = Wire(Vec(coreWidth, new MicroOp))
    for (w <- 0 until coreWidth) {
        val dec_unit = Module(new DecodeUnit)
        dec_unit.io.enq.uop   := io.enq.bits.uops(w).bits
        dec_unit.io.interrupt := io.intrpt

        valids(w) := io.enq.valid && io.enq.bits.uops(w).valid && !finished(w)
        uops(w)   := dec_unit.io.deq.uop
    }

    private val brmask_unit = Module(new BrMaskUnit)
    brmask_unit.io.flush     := io.flush
    brmask_unit.io.br_update := io.br_update

    private val deq_valid = Wire(Vec(coreWidth, Bool()))
    for (w <- 0 until coreWidth) {
        brmask_unit.io.is_branch(w) := uops(w).allocate_brtag && !finished(w)
        brmask_unit.io.will_fire(w) := uops(w).allocate_brtag && deq_valid(w)

        uops(w).brTag  := brmask_unit.io.br_tag(w)
        uops(w).brMask := brmask_unit.io.br_mask(w)
    }

    private val dec_xcpts = uops zip valids map { case (u, v) => u.xcpt_valid && v }
    private val xcpt_idx  = PriorityEncoder(dec_xcpts)
    io.xcpt_ftqIdx.valid := dec_xcpts.reduce(_||_)
    io.xcpt_ftqIdx.bits  := uops(xcpt_idx).ftqIdx

    private val brmask_full = brmask_unit.io.is_full
    private val xcpt_stall  = dec_xcpts.reduce(_||_) && !io.xcpt_ftqIdx.ready
    private val hazards     = (0 until coreWidth).map(w =>
        valids(w) && (
            !io.dis_ready              ||
            io.rollback                ||
            brmask_full(w)             ||
            xcpt_stall                 ||
            io.b1_mispred              ||
            io.br_update.b2.mispredict ||
            io.clear
        )
    )
    private val stalls = hazards.scanLeft(false.B)((s, h) => s || h).takeRight(coreWidth)
    deq_valid := (0 until coreWidth).map { w => valids(w) && !stalls(w) }

    io.enq.ready := deq_valid.last
    (deq_valid zip uops).zipWithIndex foreach { case((v, u), i) =>
        io.deq(i).valid := v
        io.deq(i).bits  := u
    }

    when (deq_valid.last || io.clear) {
        finished := 0.U
    } .otherwise {
        finished := deq_valid.asUInt | finished
    }
}
