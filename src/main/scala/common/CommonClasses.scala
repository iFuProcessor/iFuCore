package iFu.common

import chisel3._
import chisel3.util._

import iFu.common.Consts._

import iFu.common.CauseCode
class BrUpdateMasks extends CoreBundle {
    val resolveMask = UInt(maxBrCount.W)
    val mispredictMask = UInt(maxBrCount.W)
}

class BrResolutionInfo extends CoreBundle {
    val uop = new MicroOp()
    val valid = Bool()
    val mispredict = Bool()
    val taken = Bool()
    val cfiType = UInt(CFI_SZ.W)
    val pcSel = UInt(2.W)
    val jalrTarget = UInt(xLen.W)
    val targetOffset = SInt(xLen.W) // ???
}

class BrUpdateInfo extends CoreBundle  {
    val b1 = new BrUpdateMasks()
    val b2 = new BrResolutionInfo()
}

class FuncUnitReq extends CoreBundle {
    val kill = Bool()
    val uop = new MicroOp()
    val rs1Data = UInt(xLen.W)
    val rs2Data = UInt(xLen.W)
}

class FuncUnitResp extends CoreBundle {
    val uop = new MicroOp()
    val data = UInt(xLen.W)
    val addr = UInt(xLen.W)
    val r1   = UInt(xLen.W)
    val r2   = UInt(xLen.W)
}

class ExeUnitResp(val len: Int = 32) extends CoreBundle {
    val uop        = new MicroOp
    val data       = Bits(len.W)
    val csr_cmd    = UInt(CSR_SZ.W)
    val csr_addr   = UInt(14.W)
    val tlb_op     = UInt(5.W)
    val csr_r2     = UInt(xLen.W)
    val csr_r1     = UInt(xLen.W)
}

class DCacheReq extends CoreBundle {
    val mask            = UInt(4.W)
    val addr            = UInt(vaddrBits.W)
    val data            = Bits(xLen.W)
    val uop             = new MicroOp()
    val is_uncacheable  = Bool()
}

class DCacheResp extends CoreBundle {
    val data = Bits(xLen.W)
    val uop  = new MicroOp()
}

class LSUDMemIO extends CoreBundle {
    /**************************************/
    val robAddrSz = robParameters.robAddrSz
    /***************************************/
    val req         = new DecoupledIO(Vec(memWidth,Valid(new DCacheReq)))
    val s1_kill     = Output(Vec(memWidth, Bool()))
    //val s1_paddr = Output(Vec(memWidth, UInt(paddrBits.W)))
    val s2_hit      = Input(Vec(memWidth, Bool()))
    val resp        = Flipped(Vec(memWidth, new Valid(new DCacheResp)))

    val nack        = Flipped(Vec(memWidth, new Valid(new DCacheReq)))

    val brupdate    = Output(new BrUpdateInfo)
    val exception   = Output(Bool())

    val llbit       = Output(Bool())
    val fence_dmem  = Output(Bool())
    val ordered     = Input(Bool())
}

class CommitSignals extends CoreBundle {
    val valids = Vec(robParameters.retireWidth, Bool())
    val arch_valids = Vec(robParameters.retireWidth,Bool())
    val uops = Vec(robParameters.retireWidth, new MicroOp)

    //maybe use
    //val debug

    val rbk_valids = Vec(robParameters.retireWidth,Bool())
    val rollback = Bool()

    //------------------debug
    val debug_insts = if (!FPGAPlatform) Vec(robParameters.retireWidth, UInt(32.W)) else null
    val debug_wdata = if (!FPGAPlatform) Vec(robParameters.retireWidth, UInt(xLen.W)) else null
    val debug_ldst = if (!FPGAPlatform) Vec(robParameters.retireWidth, UInt(lregSz.W)) else null
    val debug_pc = if (!FPGAPlatform) Vec(robParameters.retireWidth, UInt(32.W)) else null
    val debug_load_uncacheable = if (!FPGAPlatform) Vec(robParameters.retireWidth, Bool()) else null
}

case class SupportedFuncs (
    val alu: Boolean    = false,
    val jmp: Boolean    = false,
    val mem: Boolean    = false,
    val muldiv: Boolean = false,
    val csr: Boolean    = false,
    val cnt: Boolean    = false
    // val tlb: Boolean    = false
)

class Exception extends CoreBundle {
    val uop = new MicroOp()
    //TODO:update cause to loogarch
    val cause = Bits(CauseCode.causeCodeBits.W)
    val badvaddr = UInt(paddrBits.W)
}

class CommitExceptionSignals extends CoreBundle {
    val ftq_idx = UInt(log2Ceil(frontendParams.numFTQEntries).W)
    val pc_lob = UInt(log2Ceil(frontendParams.fetchBytes).W)
    val cause = UInt(CauseCode.microCauseBits.W)
    val badvaddr = UInt(xLen.W)
    val vaddrWriteEnable = Bool()

    val uop = new MicroOp()

    val flush_typ = FlushTypes()
}