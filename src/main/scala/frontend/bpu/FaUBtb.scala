package iFu.frontend

import scala.math.min

import chisel3._
import chisel3.util._
import chisel3.util.random.LFSR

import iFu.util._

import iFu.frontend.FrontendUtils._

class UBTBEntry extends Bundle with HasUbtbParameters {
    // val offset = SInt(offsetSz.W)   // signed offset
    val target = UInt(targetSz.W)   // target address
}

class PredState extends Bundle {
    // 00 <-> 10 <-> 11 <-> 01
    val state = UInt(2.W)

    def isTaken = state(0)
    def init(taken: Bool) = {
        state := Mux(taken, 1.U, 0.U)
    }
    def update(taken: Bool) = {
        val next = Cat(
            taken ^ state(0),
            (!taken & !state(1) & state(0)) | (taken & (state(1) | state(0)))
        )
        this.state := next
    }
}

// 用于BTB里面存储的信息，进行判断命中等信息
class UBTBMeta extends Bundle with HasUbtbParameters {
    val tag   = UInt(tagSz.W)
    val is_br = Bool()
    val state = new PredState
}

// 返回给外面预测器的信息
class UBTBPredictMeta extends Bundle with HasUbtbParameters {
    val hit       = Bool()
    val write_way = UInt(log2Ceil(nWays).W)
}

class UBTBIO extends Bundle with HasUbtbParameters {
    val s1valid  = Input(Bool())
    val s1pc     = Input(UInt(vaddrBits.W))
    val s1_mixed_pc = Input(UInt(vaddrBits.W))

    val s1targs  = Output(Vec(fetchWidth, Valid(UInt(targetSz.W))))
    // val s1targspc = Output(Vec(fetchWidth, Valid(UInt(vaddrBits.W))))
    val s1taken  = Output(Vec(fetchWidth, Bool()))
    val s1br     = Output(Vec(fetchWidth, Bool()))
    val s1jal    = Output(Vec(fetchWidth, Bool()))

    val s3meta   = Output(Vec(fetchWidth, new UBTBPredictMeta))

    val s1update = Input(Valid(new BranchPredictionUpdate))
}

class FaUBtbPredictior extends Module with HasUbtbParameters {
    val io = IO(new UBTBIO)

    val valid = Reg(Vec(nWays * fetchWidth, Bool()))
    val meta  = Reg(Vec(nWays, Vec(fetchWidth, new UBTBMeta)))
    val btb   = Reg(Vec(nWays, Vec(fetchWidth, new UBTBEntry)))

// ---------------------------------------------
//      Predict Logic
    // val s1_tag = fetchIdx(io.s1pc)
    // val s1_tag = io.s1pc(tagSz + log2Ceil(fetchBytes) - 1, log2Ceil(fetchBytes))
    val s1_tag = getTag(io.s1_mixed_pc)
    val s1_hit_OHs = VecInit((0 until fetchWidth) map { i =>
        VecInit((0 until nWays) map { w =>
            /* (meta(w)(i).tag === s1_tag) && valid(Cat(w.U(log2Ceil(nWays).W), i.U(log2Ceil(fetchWidth).W))) */
            IsEqual(meta(w)(i).tag, s1_tag) && valid(Cat(w.U(log2Ceil(nWays).W), i.U(log2Ceil(fetchWidth).W)))
        })
    })
    val s1_hits = s1_hit_OHs.map(_.asUInt.orR)
    val s1_hit_ways = s1_hit_OHs.map(oh => OHToUInt(oh))

    for (w <- 0 until fetchWidth) {
        val resp_valid = io.s1valid && s1_hits(w)
        val entry_meta = meta(s1_hit_ways(w))(w)

        io.s1targs(w).valid := resp_valid
        io.s1targs(w).bits  := (
            // (fetchAlign(io.s1pc) | (w << 2).U).asSInt + btb(s1_hit_ways(w))(w).offset
            btb(s1_hit_ways(w))(w).target
        ).asUInt

        // io.s1targspc(w).valid := resp_valid
        // io.s1targspc(w).bits  := (
        //     getTargetPC(io.s1pc, btb(s1_hit_ways(w))(w).target)
        // ).asUInt

        io.s1br(w)  := resp_valid &&  entry_meta.is_br
        io.s1jal(w) := resp_valid && !entry_meta.is_br
        // taken:
        //     1. br and we predict taken
        //     2. jal
        io.s1taken(w) := entry_meta.state.isTaken || !entry_meta.is_br
    }
// ---------------------------------------------

// ---------------------------------------------
//      Prepare Meta for Update
    val repl_way_update_en = io.s1valid && !s1_hits.reduce(_||_)
    val repl_way = LFSR(nWays, repl_way_update_en)(log2Ceil(nWays) - 1, 0)

    val s1_meta = Wire(Vec(fetchWidth, new UBTBPredictMeta)) // 这个是返回给外面的信息 和meta不是同一个类型
    for (w <- 0 until fetchWidth) {
        s1_meta(w).hit := s1_hits(w)
        s1_meta(w).write_way := Mux(
            s1_hits(w),
            s1_hit_ways(w),
            repl_way
        )
    }
    io.s3meta := RegNext(RegNext(s1_meta))
// ---------------------------------------------

// ---------------------------------------------
//      Update Logic
    val s1_update         = io.s1update
    val s1_update_cfi_idx = s1_update.bits.cfiIdx.bits
    val s1_update_meta    = VecInit(s1_update.bits.meta.map(_.uBTBMeta))
    val s1_update_ways    = VecInit(s1_update_meta.map(_.write_way))
    val s1_update_way     = s1_update_ways(s1_update_cfi_idx)

    // we don't care if offset > 2^13, BTB will handle it
    val new_offset = s1_update.bits.target.asSInt -
    (fetchAlign(s1_update.bits.pc) | (s1_update_cfi_idx << 2).asUInt).asSInt

    // update target offset
    val wen = s1_update.valid && s1_update.bits.cfiTaken && s1_update.bits.cfiIdx.valid
    
    // for (w <- 0 until fetchWidth) {
    //     when (wen) {
    //         btb(s1_update_way(w))(s1_update_cfi_idx).offset := new_offset
    //     } 
    // }

    // among all the ways that are updated, we only care about the hit one
    when(wen){
        // btb(s1_update_way)(s1_update_cfi_idx).offset := new_offset
        // update里面存的是32位，这里取出低位做target
        btb(s1_update_way)(s1_update_cfi_idx).target := getTarget(s1_update.bits.target)
    }

    // update predictor state\
    val wastaken = WireInit(VecInit(Seq.fill(fetchWidth)(false.B)))
    for (w <- 0 until fetchWidth) {
        val branch_taken = (
            s1_update.valid && (
                s1_update.bits.brMask(w) ||
                (
                    s1_update.bits.cfiIdx.valid &&
                    s1_update.bits.cfiTaken &&
                    s1_update_cfi_idx === w.U
                )
            )
        )

        when (branch_taken) {
            wastaken(w)      :=  (s1_update_cfi_idx === w.U && s1_update.bits.cfiIdx.valid &&
                                (s1_update.bits.cfiTaken || s1_update.bits.cfiIsJal))
            /* val s1_update_tag = fetchIdx(s1_update.bits.pc) */
            // val s1_update_tag = s1_update.bits.pc(tagSz + log2Ceil(fetchBytes) - 1, log2Ceil(fetchBytes))
            val s1_update_tag = getTag(mixHILO(s1_update.bits.pc))
            valid(Cat(s1_update_way, w.U(log2Ceil(fetchWidth).W))) := true.B
            meta(s1_update_way)(w).is_br := s1_update.bits.brMask(w)
            meta(s1_update_way)(w).tag   := s1_update_tag

            when (s1_update_meta(w).hit) {
                meta(s1_update_way)(w).state.update(wastaken(w))
            } .otherwise {
                meta(s1_update_way)(w).state.init(wastaken(w))
            }
        }
    }
// ---------------------------------------------

// ---------------------------------------------
//      Performance Counter
    // TODO
// ---------------------------------------------
}

