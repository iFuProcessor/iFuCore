package iFu.difftest

import chisel3._
import chisel3.util._

import iFu.common._
import iFu.common.Consts._

import iFu.difftest._

class LogicRegisters extends CoreModule {
    val io = IO(new Bundle {
        val commit = Input(new CommitSignals())
    })

    val debug_reg = RegInit(VecInit(Seq.fill(numLRegs)(0.U(32.W))))

    for (w <- 0 until coreWidth) {
        when (io.commit.valids(w)) {
            val lreg  = io.commit.uops(w).ldst
            val wdata = io.commit.debug_wdata(w)
            val wen   = io.commit.uops(w).ldst_val

            when (wen && lreg =/= 0.U) {
                debug_reg(lreg) := wdata
            }
        }
    }

    val difftest = Module(new DifftestGRegState)
    difftest.io.clock  := clock
    difftest.io.coreid := 0.U   // only support 1 core now

    for (i <- 0 until numLRegs) {
        difftest.io.gpr(i) := debug_reg(i)
    }
}
