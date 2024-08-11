package iFu.frontend

import chisel3._
import chisel3.util._

import iFu.util._
import iFu.frontend.FrontendUtils._

class BIMPredictMeta extends Bundle with HasBimParameters {
    val bim = UInt(2.W)
}

class BIMIO extends Bundle with HasBimParameters {
    val s0valid  = Input(Bool())
    val s0pc     = Input(UInt(vaddrBits.W))

    val s2taken  = Output(Vec(fetchWidth, Bool()))

    val s3meta   = Output(Vec(fetchWidth, new BIMPredictMeta))

    val s1update = Input(Valid(new BranchPredictionUpdate))
}

class BimPredictor extends Module with HasBimParameters {
    val io = IO(new BIMIO)

    val bim_ram = SyncReadMem(nSets, Vec(fetchWidth, UInt(2.W)))

// ---------------------------------------------
//      Reset Logic
    val reset_en  = RegInit(false.B)
    val reset_idx = RegInit(0.U(log2Ceil(nSets).W))
    when (reset_en) {
        reset_idx := reset_idx + 1.U
    }
    when (reset_idx === (nSets - 1).U) {
        reset_en := false.B
    }
// ---------------------------------------------

// ---------------------------------------------
//      Predict Logic
    val s0_valid = io.s0valid
    val s0_idx   = fetchIdx(io.s0pc)

    val s2_bim = RegNext(VecInit(
        bim_ram.read(s0_idx.asUInt, s0_valid).map(_.asTypeOf(UInt(2.W)))
    ))

    // val s2_valid = RegNext(RegNext(io.s0valid))

    for (w <- 0 until fetchWidth) {
        // val resp_valid = !reset_en && s2_valid
        // io.s2taken(w) := resp_valid && s2_bim(w)(1)
        io.s2taken(w) := s2_bim(w)(1)
    }
// ---------------------------------------------

// ---------------------------------------------
//      Prepare Meta for Update
    val s2_meta = Wire(Vec(fetchWidth, new BIMPredictMeta))
    for (w <- 0 until fetchWidth) {
        s2_meta(w).bim := s2_bim(w)
    }
    io.s3meta := RegNext(s2_meta)
// ---------------------------------------------

// ---------------------------------------------
//      Update Logic
    val s1_update      = io.s1update
    val s1_update_idx  = fetchIdx(io.s1update.bits.pc)
    val s1_update_meta = VecInit(s1_update.bits.meta.map(_.bimMeta))

    val s1_update_mask = Wire(Vec(fetchWidth,Bool()))
    val s1_update_data = Wire(Vec(fetchWidth, UInt(2.W)))

    for (w <- 0 until fetchWidth) {
        s1_update_mask(w) := false.B
        s1_update_data(w) := s1_update_meta(w).asUInt

        when (s1_update.valid && (s1_update.bits.brMask(w) ||(s1_update.bits.cfiIdx.valid && s1_update.bits.cfiIdx.bits === w.U))) {
            val was_taken = (
                (s1_update.bits.cfiIdx.valid) &&
                (s1_update.bits.cfiIdx.bits === w.U) &&
                (
                    (s1_update.bits.cfiIsBr && s1_update.bits.brMask(w) && s1_update.bits.cfiTaken) ||
                    s1_update.bits.cfiIsJal
                )
            )
            val old_bim = s1_update_meta(w).asUInt

            s1_update_mask(w) := true.B
            s1_update_data(w) := bimWrite(old_bim, was_taken)
        }
    }

    bim_ram.write(
        Mux(reset_en, reset_idx, s1_update_idx),
        Mux(reset_en, VecInit(Seq.fill(fetchWidth){ 2.U(2.W) }), s1_update_data),
        Mux(reset_en, (~(0.U(fetchWidth.W))), s1_update_mask.asUInt).asBools
    )

// ---------------------------------------------
}
