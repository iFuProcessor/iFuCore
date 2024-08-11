package iFu.backend

import chisel3._
import chisel3.util._
import iFu.common._
import iFu.common.Consts._
import iFu.common.CauseCode._
import iFu.tlb._
import iFu.util._

class IndexAllocator extends CoreModule {
    val stqAddrSz       = lsuParameters.stqAddrSz
    val ldqAddrSz       = lsuParameters.ldqAddrSz
    val numStqEntries   = lsuParameters.numSTQEntries
    val numLdqEntries   = lsuParameters.numLDQEntries
    val io = IO(new CoreBundle {
        val dis_uops    = Input(Vec(coreWidth, Valid(new MicroOp)))
        val old_ldq_tail = Input(UInt(ldqAddrSz.W))
        val old_stq_tail = Input(UInt(stqAddrSz.W))
        val ldq_enq_idxs = Output(Vec(coreWidth, UInt(ldqAddrSz.W)))
        val stq_enq_idxs = Output(Vec(coreWidth, UInt(stqAddrSz.W)))
        val new_ldq_tail     = Output(UInt(ldqAddrSz.W))
        val new_stq_tail     = Output(UInt(stqAddrSz.W))
    })
    val dis_ld_valids = VecInit(io.dis_uops.map(x=>x.valid && x.bits.use_ldq && !x.bits.xcpt_valid))
    val dis_st_valids = VecInit(io.dis_uops.map(x=>x.valid && x.bits.use_stq && !x.bits.xcpt_valid))
    // assert(coreWidth == 4 , "coreWidth must be 4")
    io.ldq_enq_idxs := 0.U.asTypeOf(Vec(coreWidth, UInt(ldqAddrSz.W)))
    io.stq_enq_idxs := 0.U.asTypeOf(Vec(coreWidth, UInt(stqAddrSz.W)))
    io.new_ldq_tail := 0.U
    io.new_stq_tail := 0.U

    val ldq_tail_add_1 = WrapAdd(io.old_ldq_tail, 1.U, numLdqEntries)
    val ldq_tail_add_2 = WrapAdd(io.old_ldq_tail, 2.U, numLdqEntries)
    val ldq_tail_add_3 = WrapAdd(io.old_ldq_tail, 3.U, numLdqEntries)

    val stq_tail_add_1 = WrapAdd(io.old_stq_tail, 1.U, numStqEntries)
    val stq_tail_add_2 = WrapAdd(io.old_stq_tail, 2.U, numStqEntries)
    val stq_tail_add_3 = WrapAdd(io.old_stq_tail, 3.U, numStqEntries)

    assert(coreWidth == 3 , "coreWidth must be 3")
    switch(dis_ld_valids.asUInt){
        is("b000".U){
            io.ldq_enq_idxs(0) := io.old_ldq_tail
            io.ldq_enq_idxs(1) := io.old_ldq_tail
            io.ldq_enq_idxs(2) := io.old_ldq_tail
            io.new_ldq_tail := io.old_ldq_tail
        }
        is("b001".U){
            io.ldq_enq_idxs(0) := io.old_ldq_tail
            io.ldq_enq_idxs(1) := ldq_tail_add_1
            io.ldq_enq_idxs(2) := ldq_tail_add_1
            io.new_ldq_tail := ldq_tail_add_1
        }
        is("b010".U){
            io.ldq_enq_idxs(0) := io.old_ldq_tail
            io.ldq_enq_idxs(1) := io.old_ldq_tail
            io.ldq_enq_idxs(2) := ldq_tail_add_1
            io.new_ldq_tail := ldq_tail_add_1
        }
        is("b011".U){
            io.ldq_enq_idxs(0) := io.old_ldq_tail
            io.ldq_enq_idxs(1) := ldq_tail_add_1
            io.ldq_enq_idxs(2) := ldq_tail_add_2
            io.new_ldq_tail := ldq_tail_add_2
        }
        is("b100".U){
            io.ldq_enq_idxs(0) := io.old_ldq_tail
            io.ldq_enq_idxs(1) := io.old_ldq_tail
            io.ldq_enq_idxs(2) := io.old_ldq_tail
            io.new_ldq_tail := ldq_tail_add_1
        }
        is("b101".U){
            io.ldq_enq_idxs(0) := io.old_ldq_tail
            io.ldq_enq_idxs(1) := ldq_tail_add_1
            io.ldq_enq_idxs(2) := ldq_tail_add_1
            io.new_ldq_tail := ldq_tail_add_2
        }
        is("b110".U){
            io.ldq_enq_idxs(0) := io.old_ldq_tail
            io.ldq_enq_idxs(1) := io.old_ldq_tail
            io.ldq_enq_idxs(2) := ldq_tail_add_1
            io.new_ldq_tail := ldq_tail_add_2
        }
        is("b111".U){
            io.ldq_enq_idxs(0) := io.old_ldq_tail
            io.ldq_enq_idxs(1) := ldq_tail_add_1
            io.ldq_enq_idxs(2) := ldq_tail_add_2
            io.new_ldq_tail := ldq_tail_add_3
        }
    }

    switch(dis_st_valids.asUInt){
        is("b000".U){
            io.stq_enq_idxs(0) := io.old_stq_tail
            io.stq_enq_idxs(1) := io.old_stq_tail
            io.stq_enq_idxs(2) := io.old_stq_tail
            io.new_stq_tail := io.old_stq_tail
        }
        is("b001".U){
            io.stq_enq_idxs(0) := io.old_stq_tail
            io.stq_enq_idxs(1) := stq_tail_add_1
            io.stq_enq_idxs(2) := stq_tail_add_1
            io.new_stq_tail := stq_tail_add_1
        }
        is("b010".U){
            io.stq_enq_idxs(0) := io.old_stq_tail
            io.stq_enq_idxs(1) := io.old_stq_tail
            io.stq_enq_idxs(2) := stq_tail_add_1
            io.new_stq_tail := stq_tail_add_1
        }
        is("b011".U){
            io.stq_enq_idxs(0) := io.old_stq_tail
            io.stq_enq_idxs(1) := stq_tail_add_1
            io.stq_enq_idxs(2) := stq_tail_add_2
            io.new_stq_tail := stq_tail_add_2
        }
        is("b100".U){
            io.stq_enq_idxs(0) := io.old_stq_tail
            io.stq_enq_idxs(1) := io.old_stq_tail
            io.stq_enq_idxs(2) := io.old_stq_tail
            io.new_stq_tail := stq_tail_add_1
        }
        is("b101".U){
            io.stq_enq_idxs(0) := io.old_stq_tail
            io.stq_enq_idxs(1) := stq_tail_add_1
            io.stq_enq_idxs(2) := stq_tail_add_1
            io.new_stq_tail := stq_tail_add_2
        }
        is("b110".U){
            io.stq_enq_idxs(0) := io.old_stq_tail
            io.stq_enq_idxs(1) := io.old_stq_tail
            io.stq_enq_idxs(2) := stq_tail_add_1
            io.new_stq_tail := stq_tail_add_2
        }
        is("b111".U){
            io.stq_enq_idxs(0) := io.old_stq_tail
            io.stq_enq_idxs(1) := stq_tail_add_1
            io.stq_enq_idxs(2) := stq_tail_add_2
            io.new_stq_tail := stq_tail_add_3
        }
    }
}
