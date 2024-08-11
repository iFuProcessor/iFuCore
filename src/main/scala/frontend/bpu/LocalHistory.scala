package iFu.frontend

import chisel3._
import chisel3.util.{Cat, Valid}
import iFu.frontend.FrontendUtils.{fetchIdx, getPc}
import iFu.frontend._
import ram.SDPRam

class LocalHistoryPredictMeta extends Bundle with HasLocalHistoryParameters {
    val cntIdx = UInt(nCounterBits.W)
    val counter = UInt(2.W)
}

class LocalHistoryIO extends Bundle with HasLocalHistoryParameters {
    val s0pc = Input(UInt(vaddrBits.W))

    /* val s2taken = Output(Vec(fetchWidth, Valid(Bool()))) */
    val s2_high_taken = Output(Vec(fetchWidth, Valid(Bool())))

    val s3meta = Output(Vec(fetchWidth, new LocalHistoryPredictMeta))

    val s1update = Input(Valid(new BranchPredictionUpdate))
}

class LocalHistoryPredictor extends Module with HasLocalHistoryParameters {
    val io = IO(new LocalHistoryIO)

    val localHistories = SyncReadMem(nLHRs, Vec(fetchWidth, UInt(localHistoryLength.W)))
    val counters = Seq.fill(fetchWidth) {SyncReadMem(nCounters, UInt(2.W))}
    /* val cacheCounters = Seq.fill(fetchWidth) {Module(new SDPRam(nCacheCounters, UInt(2.W)))} */

    // ---------------------------------------------
    // Reset
    val reset_en = RegInit(false.B)
    val reset_idx = RegInit(0.U(nCounterBits.W))
    when(reset_en) {
        reset_idx := reset_idx + 1.U
    }
    when(reset_idx.andR) {
        reset_en := false.B
    }
    // ---------------------------------------------
    // Predict
    val s1pc = RegNext(io.s0pc)
    val s1hist = localHistories.read(fetchIdx(io.s0pc)(nLHRBits - 1, 0))
    val s1idx = VecInit(s1hist.zipWithIndex.map({case (hist, w) => idxHash(getPc(s1pc, w.U), hist)}))
    val s2cnt = VecInit(counters.zip(s1idx).map({case (ram, idx) => ram.read(idx)}))
    /* io.s2taken := s1hist.zip(cacheCounters).map({ case (hist, cnt) =>
        val idx = cacheIdxHash(hist)
        cnt.io.raddr := idx
        val cnt_val = cnt.io.rdata.head
        val taken = Wire(Valid(Bool()))
        taken.valid := !(cnt_val(0) ^ cnt_val(1))
        taken.bits := cnt_val(1)
        taken
    }) */

    io.s2_high_taken := VecInit(s2cnt.map(cnt => {
        val taken = Wire(Valid(Bool()))
        taken.valid := cnt =/= 2.U && !reset_en
        taken.bits := cnt(1)
        taken
    }))
    /* val s3hist = RegNext(RegNext(s1hist))
    val s3cnt = RegNext(s2cnt)
    s3hist.zip(cacheCounters).zip(s3cnt).foreach({
        case ((hist, cnt), cnt_val) =>
        val idx = cacheIdxHash(hist)
        cnt.io.wen := true.B
        cnt.io.waddr := idx
        cnt.io.wdata.head := cnt_val
        cnt.io.wstrobe := 1.U
    }) */
    // ---------------------------------------------
    // Meta
    val s2idx = RegNext(s1idx)
    val s2meta = VecInit(s2idx.zip(s2cnt).map({case (idx, cnt) =>
        val meta = Wire(new LocalHistoryPredictMeta)
        meta.cntIdx := idx
        meta.counter := cnt
        meta
    }))
    io.s3meta := RegNext(s2meta)
    // ---------------------------------------------
    // Update at s2
    val s1update = io.s1update.bits
    val s1updatepc = io.s1update.bits.pc
    val s2update = RegNext(s1update)
    val s2oldHist = localHistories.read(fetchIdx(s1updatepc)(nLHRBits - 1, 0))
    val s2newHist = Wire(Vec(fetchWidth, UInt(localHistoryLength.W)))
    val s2newCounter = Wire(Vec(fetchWidth, UInt(2.W)))
    for (w <- 0 until fetchWidth) {
        val s1taken = Mux(s1update.cfiIdx.valid && s1update.cfiIdx.bits === w.U, s1update.cfiTaken, false.B)
        val s2taken = RegNext(s1taken)
        val s2oldCounter = counters(w).read(s1update.meta(w).localHistoryMeta.cntIdx)
        s2newHist(w) := Mux(s2update.brMask(w), Cat(s2oldHist(w)(localHistoryLength - 2, 0), s2taken.asUInt), s2oldHist(w))
        s2newCounter(w) := Mux(s2update.brMask(w), update(s2oldCounter, s2taken), s2oldCounter)
    }
    when (RegNext(io.s1update.valid)) {
        localHistories.write(fetchIdx(s2update.pc)(nLHRBits - 1, 0), s2newHist)
    }
    when (RegNext(io.s1update.valid) || reset_en) { // only counter needs resetting
        for (w <- 0 until fetchWidth) {
            counters(w).write(Mux(reset_en, reset_idx, s2update.meta(w).localHistoryMeta.cntIdx),
                Mux(reset_en, 2.U(2.W), s2newCounter(w)))
        }
    }
    // ---------------------------------------------
}
