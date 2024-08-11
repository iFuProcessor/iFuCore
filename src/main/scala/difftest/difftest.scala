package iFu.difftest

import chisel3._
import chisel3.util._

trait DifftestWithClock {
    val clock = Input(Clock())
}

trait DifftestWithCoreid {
    val coreid = Input(UInt(8.W))
}

trait DifftestWithIndex {
    val index = Input(UInt(8.W))
}

abstract class DifftestBundle extends Bundle
    with DifftestWithClock
    with DifftestWithCoreid

class DiffInstrCommitIO extends DifftestBundle with DifftestWithIndex {
    val valid          = Input(Bool())
    val pc             = Input(UInt(64.W))
    val instr          = Input(UInt(32.W))
    val skip           = Input(Bool())
    val is_TLBFILL     = Input(Bool())
    val TLBFILL_index  = Input(UInt(8.W))
    val is_CNTinst     = Input(Bool())
    val timer_64_value = Input(UInt(64.W))
    val wen            = Input(Bool())
    val wdest          = Input(UInt(8.W))
    val wdata          = Input(UInt(64.W))
    val csr_rstat      = Input(Bool())
    val csr_data       = Input(UInt(32.W))
}

class DiffTrapEventIO extends DifftestBundle {
    val valid    = Input(Bool())
    val code     = Input(UInt(3.W))
    val pc       = Input(UInt(64.W))
    val cycleCnt = Input(UInt(64.W))
    val instrCnt = Input(UInt(64.W))
}

class DiffCSRRegStateIO extends DifftestBundle {
    val crmd      = Input(UInt(64.W))
    val prmd      = Input(UInt(64.W))
    val euen      = Input(UInt(64.W))
    val ecfg      = Input(UInt(64.W))
    val estat     = Input(UInt(64.W))
    val era       = Input(UInt(64.W))
    val badv      = Input(UInt(64.W))
    val eentry    = Input(UInt(64.W))
    val tlbidx    = Input(UInt(64.W))
    val tlbehi    = Input(UInt(64.W))
    val tlbelo0   = Input(UInt(64.W))
    val tlbelo1   = Input(UInt(64.W))
    val asid      = Input(UInt(64.W))
    val pgdl      = Input(UInt(64.W))
    val pgdh      = Input(UInt(64.W))
    val save0     = Input(UInt(64.W))
    val save1     = Input(UInt(64.W))
    val save2     = Input(UInt(64.W))
    val save3     = Input(UInt(64.W))
    val tid       = Input(UInt(64.W))
    val tcfg      = Input(UInt(64.W))
    val tval      = Input(UInt(64.W))
    val ticlr     = Input(UInt(64.W))
    val llbctl    = Input(UInt(64.W))
    val tlbrentry = Input(UInt(64.W))
    val dmw0      = Input(UInt(64.W))
    val dmw1      = Input(UInt(64.W))
}

class DifftestExcpEventIO extends DifftestBundle {
    val excp_valid    = Input(Bool())
    val eret          = Input(Bool())
    val intrNo        = Input(UInt(11.W))
    val cause         = Input(UInt(15.W))
    val exceptionPC   = Input(UInt(32.W))
    val exceptionInst = Input(UInt(32.W))
}

class DiffGRegStateIO extends DifftestBundle {
  val gpr = Input(Vec(32, UInt(64.W)))
}

class DiffStoreEventIO extends DifftestBundle with DifftestWithIndex {
    val valid      = Input(UInt(8.W))//{4'b0, llbit && sc_w, st_w, st_h, st_b}
    val storePAddr = Input(UInt(64.W))
    val storeVAddr = Input(UInt(64.W))
    val storeData  = Input(UInt(64.W))
}

class DiffLoadEventIO extends DifftestBundle with DifftestWithIndex {
    val valid  = Input(UInt(8.W)) //{2'b0, ll_w, ld_w, ld_hu, ld_h, ld_bu, ld_b}
    val paddr  = Input(UInt(64.W))
    val vaddr  = Input(UInt(64.W))
}

class DifftestInstrCommit extends BlackBox {
  val io = IO(new DiffInstrCommitIO)
}

class DifftestTrapEvent extends BlackBox {
  val io = IO(new DiffTrapEventIO)
}

class DifftestCSRRegState extends BlackBox {
  val io = IO(new DiffCSRRegStateIO)
}

class DifftestGRegState extends BlackBox {
  val io = IO(new DiffGRegStateIO)
}

class DifftestStoreEvent extends BlackBox {
    val io = IO(new DiffStoreEventIO)
}

class DifftestLoadEvent extends BlackBox {
    val io = IO(new DiffLoadEventIO)
}

class DifftestExcpEvent extends BlackBox {
    val io = IO(new DifftestExcpEventIO)
}
