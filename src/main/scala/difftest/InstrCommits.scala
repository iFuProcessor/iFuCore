
package iFu.difftest

import chisel3._
import chisel3.util._

import iFu.common._
import iFu.common.Consts._

import iFu.difftest._

class InstrCommit extends CoreBundle {
    val debug_uopc  = Vec(robParameters.retireWidth, UInt(UOPC_SZ.W))
    val debug_insts = Vec(robParameters.retireWidth, UInt(32.W))
    val debug_wdata = Vec(robParameters.retireWidth, UInt(xLen.W))
    val debug_ldst  = Vec(robParameters.retireWidth, UInt(lregSz.W))
    val debug_pc    = Vec(robParameters.retireWidth, UInt(32.W))
    val debug_wen   = Vec(robParameters.retireWidth, Bool())
    val debug_load_uncacheable = Vec(robParameters.retireWidth, Bool())

    val valids      = Vec(robParameters.retireWidth,Bool())
}

class InstrCommits extends CoreModule {
    val io = IO(new Bundle{
        val rawCommit = Input(new InstrCommit)
        val exception = Input(Bool())
        val fill_idx  = if (!FPGAPlatform) Input(UInt(5.W)) else null
    })

    //-------------------------------------
    val cmtsz = robParameters.retireWidth
    //-------------------------------------

    val rawCommit = RegNext(io.rawCommit)

    val idxs = Wire(Vec(cmtsz, Valid(UInt(log2Ceil(cmtsz).W))))
    idxs := 0.U.asTypeOf(Vec(cmtsz, Valid(UInt(log2Ceil(cmtsz).W))))

    when (rawCommit.valids.reduce(_|_)) {
        // 初始化头元素
        idxs(0).valid := rawCommit.valids(0) && !io.exception
        idxs(0).bits  := Mux(rawCommit.valids(0), 0.U, 3.U/*2'b11*/)

        for (i <- 1 until cmtsz) {
            idxs(i).valid := rawCommit.valids(i) && !io.exception
            idxs(i).bits  := Mux(rawCommit.valids(i), idxs(i-1).bits + 1.U, idxs(i-1).bits)
        }
    }

    val zippedCommit = WireInit(0.U.asTypeOf(new InstrCommit))

    for (i <- 0 until cmtsz) {
        val valid = idxs(i).valid
        val idx = idxs(i).bits

        when (valid) {
            zippedCommit.valids(idx) := true.B
            zippedCommit.debug_pc(idx)    := rawCommit.debug_pc(i)
            zippedCommit.debug_ldst(idx)  := rawCommit.debug_ldst(i)
            zippedCommit.debug_insts(idx) := rawCommit.debug_insts(i)
            zippedCommit.debug_wdata(idx) := rawCommit.debug_wdata(i)
            zippedCommit.debug_wen(idx)   := rawCommit.debug_wen(i)
            zippedCommit.debug_uopc(idx)  := rawCommit.debug_uopc(i)
            zippedCommit.debug_load_uncacheable(idx) := rawCommit.debug_load_uncacheable(i)
        }
    }

    for (i <- 0 until cmtsz) {
        val dic = Module(new DifftestInstrCommit)
        dic.io.clock  := clock
        dic.io.coreid := 0.U   // only support 1 core now

        dic.io.index          := i.U
        dic.io.valid          := zippedCommit.valids(i)
        dic.io.pc             := zippedCommit.debug_pc(i)
        dic.io.instr          := zippedCommit.debug_insts(i)
        dic.io.skip           := false.B
        dic.io.is_TLBFILL     := zippedCommit.debug_uopc(i) === uopTLBFILL
        dic.io.TLBFILL_index  := Cat(0.U(3.W), io.fill_idx - 1.U)
        dic.io.is_CNTinst     := false.B
        dic.io.timer_64_value := 0.U
        dic.io.wen            := zippedCommit.debug_wen(i)
        dic.io.wdest          := zippedCommit.debug_ldst(i)
        dic.io.wdata          := zippedCommit.debug_wdata(i)
        dic.io.csr_rstat      := false.B
        dic.io.csr_data       := 0.U

        val dle = Module(new DifftestLoadEvent)
        dle.io.clock          := clock
        dle.io.coreid         := 0.U
        dle.io.vaddr          := 0.U

        dle.io.valid          := zippedCommit.debug_load_uncacheable(i)
        dle.io.index          := i.U
        dle.io.paddr          := 0xf8000000L.asUInt
    }
}
