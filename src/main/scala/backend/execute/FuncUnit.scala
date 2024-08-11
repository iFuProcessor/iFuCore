package iFu.backend

import chisel3._
import chisel3.util._

import iFu.common._
import iFu.common.Consts._
import iFu.util._
import iFu.frontend.GetPCFromFtqIO

abstract class FuncUnit (
    val isPiplined: Boolean,
    val numStages: Int,
    val isJmpUnit: Boolean,
    val isAluUnit: Boolean,
    val isCSRUnit: Boolean
) extends CoreModule {
    val io = IO(new Bundle {
        val req      = Flipped(Decoupled(new FuncUnitReq))
        val resp     = Decoupled(new FuncUnitResp)
        val brUpdate = Input(new BrUpdateInfo)
        val bypass   = Output(Vec(numStages, Valid(new ExeUnitResp)))

        val brInfo   = if (isAluUnit) Output(new BrResolutionInfo) else null
        val getFtqPC = if (isJmpUnit) Flipped(new GetPCFromFtqIO) else null
        val imm      = if (isCSRUnit) Output(UInt(14.W)) else null
    })
    io <> DontCare
}

abstract class PipelinedFuncUnit (
    numStages: Int,
    isJmpUnit: Boolean = false,
    isAluUnit: Boolean = false,
    isCSRUnit: Boolean = false
) extends FuncUnit (
    isPiplined = true,
    numStages = numStages,
    isJmpUnit = isJmpUnit,
    isAluUnit = isAluUnit,
    isCSRUnit = isCSRUnit
) {
    io.req.ready := true.B
    var rValids: Vec[Bool]  = null
    var rUops: Vec[MicroOp] = null
    var rR1: Vec[UInt]      = null
    var rR2: Vec[UInt]      = null

    if (numStages > 0) {
        rValids = RegInit(VecInit(Seq.fill(numStages){ false.B }))
        rUops = Reg(Vec(numStages, new MicroOp()))
        rR1 = Reg(Vec(numStages, UInt(xLen.W)))
        rR2 = Reg(Vec(numStages, UInt(xLen.W)))

        rValids(0) := io.req.valid && !IsKilledByBranch(io.brUpdate, io.req.bits.uop) && !io.req.bits.kill
        rUops(0) := io.req.bits.uop
        rUops(0).brMask := GetNewBrMask(io.brUpdate, io.req.bits.uop)
        rR1(0) := io.req.bits.rs1Data
        rR2(0) := io.req.bits.rs2Data

        for (i <- 1 until numStages) {
            rValids(i) := rValids(i - 1) && !IsKilledByBranch(io.brUpdate, rUops(i-1)) && !io.req.bits.kill
            rUops(i) := rUops(i - 1)
            rUops(i).brMask := GetNewBrMask(io.brUpdate, rUops(i - 1))
            rR1(i) := rR1(i - 1)
            rR2(i) := rR2(i - 1)
        }

        io.resp.valid := rValids(numStages - 1) && !IsKilledByBranch(io.brUpdate, rUops(numStages - 1)) && !io.req.bits.kill
        io.resp.bits.uop := rUops(numStages - 1)
        io.resp.bits.uop.brMask := GetNewBrMask(io.brUpdate, rUops(numStages - 1))
        io.resp.bits.r1 := rR1(numStages - 1)
        io.resp.bits.r2 := rR2(numStages - 1)

        io.bypass(0).bits.uop := io.req.bits.uop
        for (i <- 1 until numStages) {
            io.bypass(i).bits.uop := rUops(i - 1)
        }
    } else {
        require (numStages == 0)

        io.resp.valid := io.req.valid && !IsKilledByBranch(io.brUpdate, io.req.bits.uop) && !io.req.bits.kill
        io.resp.bits.uop := io.req.bits.uop
        io.resp.bits.uop.brMask := GetNewBrMask(io.brUpdate, io.req.bits.uop)
        io.resp.bits.r1 := io.req.bits.rs1Data
        io.resp.bits.r2 := io.req.bits.rs2Data
    }
}

class ALUUnit(
    isJmpUnit: Boolean = false,
    isCSRUnit: Boolean = false,
    numStages: Int = 1,
) extends PipelinedFuncUnit (
    numStages = numStages,
    isAluUnit = true,
    isJmpUnit = isJmpUnit,
    isCSRUnit = isCSRUnit
) {
    val uop = io.req.bits.uop

    val imm = immGen(uop.immPacked, uop.ctrl.imm_sel)
    if (isCSRUnit) {
        require (numStages == 1, "CSRUnit only supports 1 stage")
        io.imm := Mux(uop.ctrl.imm_sel === immCID, CSR_TID,
                                                   uop.immPacked(23, 10).asUInt)
    }

    var op1Data: UInt = null
    if (isJmpUnit) {
        val block_pc = AlignPCToBoundary(io.getFtqPC.pc, frontendParams.fetchBytes)
        val uop_pc  = block_pc | uop.pcLowBits

        op1Data = Mux(uop.ctrl.op1_sel === OP1_RS1, io.req.bits.rs1Data,
                  Mux(uop.ctrl.op1_sel === OP1_PC,  uop_pc, 0.U))
    } else {
        op1Data = Mux(uop.ctrl.op1_sel === OP1_RS1, io.req.bits.rs1Data, 0.U)
    }
    val op2Data = Mux(uop.ctrl.op2_sel === OP2_IMM, imm.asUInt,
                  Mux(uop.ctrl.op2_sel === OP2_RS2, io.req.bits.rs2Data, 
                  Mux(uop.ctrl.op2_sel === OP2_NEXT, 4.U, 0.U)))

    val alu = Module(new Alu)
    alu.io.fn := uop.ctrl.op_fcn
    alu.io.op1 := op1Data
    alu.io.op2 := op2Data

    val killed = io.req.bits.kill || IsKilledByBranch(io.brUpdate, uop)

    val rs1 = io.req.bits.rs1Data
    val rs2 = io.req.bits.rs2Data
    val brEq = rs1 === rs2
    val brLtu = rs1.asUInt < rs2.asUInt
    val brLt = (!(rs1(xLen - 1) ^ rs2(xLen - 1)) & brLtu | rs1(xLen - 1) & ~rs2(xLen - 1)).asBool

    val pcSel = MuxLookup(uop.ctrl.br_type, PC_PLUS4)(
        Seq(
            BR_N   -> PC_PLUS4,
            BR_NE  -> Mux(!brEq , PC_BRJMP, PC_PLUS4),
            BR_EQ  -> Mux( brEq , PC_BRJMP, PC_PLUS4),
            BR_GE  -> Mux(!brLt , PC_BRJMP, PC_PLUS4),
            BR_GEU -> Mux(!brLtu, PC_BRJMP, PC_PLUS4),
            BR_LT  -> Mux( brLt , PC_BRJMP, PC_PLUS4),
            BR_LTU -> Mux( brLtu, PC_BRJMP, PC_PLUS4),
            BR_J   -> PC_BRJMP,
            BR_JR  -> PC_JIRL
        )
    )

    val isTaken = io.req.valid && !killed &&
                 (uop.isBr || uop.isJalr || uop.isJal) && (pcSel =/= PC_PLUS4)
    val isBr = io.req.valid && !killed && uop.isBr
    val isJalr = io.req.valid && !killed && uop.isJalr

    val mispredict = WireInit(false.B)
    when (isBr || isJalr) {
        if (!isJmpUnit) {
            assert(pcSel =/= PC_JIRL)
        }
        when (pcSel === PC_PLUS4) {
            mispredict := uop.taken
        } .elsewhen (pcSel === PC_BRJMP) {
            mispredict := !uop.taken
        }
    }

    val brInfo = WireInit(0.U.asTypeOf(new BrResolutionInfo))

    brInfo.valid := isBr || isJalr
    brInfo.mispredict := mispredict
    brInfo.uop := uop
    brInfo.cfiType := Mux(isJalr, CFI_JIRL, Mux(isBr, CFI_BR, CFI_X))
    brInfo.taken := isTaken
    brInfo.pcSel := pcSel
    val fetchWidth= frontendParams.fetchWidth
    if (isJmpUnit) {
        val jalrTargetBase = io.req.bits.rs1Data.asSInt
        val jalrTarget = (jalrTargetBase + imm).asUInt
        brInfo.jalrTarget := jalrTarget

        val cfiIdx = uop.pcLowBits(log2Ceil(fetchWidth) + log2Ceil(coreInstrBytes) - 1 , log2Ceil(coreInstrBytes))

        when (pcSel === PC_JIRL) {
            mispredict := !io.getFtqPC.nextVal || (io.getFtqPC.nextpc =/= jalrTarget) ||
                        !io.getFtqPC.entry.cfiIdx.valid || (io.getFtqPC.entry.cfiIdx.bits =/= cfiIdx)
        }
    }
    brInfo.targetOffset := imm
    io.brInfo := brInfo

    val rMispred = Reg(Vec(numStages, Bool()))
    rMispred(0) := mispredict
    for (i <- 1 until numStages) {
        rMispred(i) := rMispred(i - 1)
    }
    if (!FPGAPlatform) io.resp.bits.uop.debug_mispred := rMispred(numStages - 1)

    val rData = Reg(Vec(numStages, UInt(xLen.W)))
    rData(0) := alu.io.out
    for (i <- 1 until numStages) {
        rData(i) := rData(i - 1)
    }
    io.resp.bits.data := rData(numStages - 1)

    require (numStages >= 1)
    io.bypass(0).valid := io.req.valid
    io.bypass(0).bits.data := alu.io.out
    for (i <- 1 until numStages) {
        io.bypass(i).valid := rValids(i - 1)
        io.bypass(i).bits.data := rData(i - 1)
    }
}

class CntUnit(numStages: Int = 1) extends PipelinedFuncUnit (
    numStages = numStages,
) {
    val counter = Module(new Counter64)
    counter.io.fn := io.req.bits.uop.ctrl.op_fcn

    val rData = Reg(Vec(numStages, UInt(xLen.W)))
    rData(0) := counter.io.data
    for (i <- 1 until numStages) {
        rData(i) := rData(i - 1)
    }

    io.resp.bits.data := rData(numStages - 1)

    require (numStages >= 1)
    io.bypass(0).valid := io.req.valid
    io.bypass(0).bits.data := counter.io.data
    for (i <- 1 until numStages) {
        io.bypass(i).valid := rValids(i - 1)
        io.bypass(i).bits.data := rData(i - 1)
    }
}

class PipelinedMulUnit(numStages: Int = 2) extends PipelinedFuncUnit (
    numStages = numStages,
) {
    val imult = Module(new MultDSP48E1)

    imult.io.req.valid := io.req.valid
    imult.io.req.bits.fn := io.req.bits.uop.ctrl.op_fcn
    imult.io.req.bits.op1 := io.req.bits.rs1Data
    imult.io.req.bits.op2 := io.req.bits.rs2Data

    io.resp.bits.data := imult.io.resp.bits.data
}

class AddrGenUnit(numStages: Int = 0) extends PipelinedFuncUnit(
    numStages = numStages
) {
    val uop    = io.req.bits.uop
    val offset = immGen(uop.immPacked, uop.ctrl.imm_sel)
    val addr   = (io.req.bits.rs1Data.asSInt + offset).asUInt

    io.resp.bits.addr := addr
    io.resp.bits.data := io.req.bits.rs1Data
}

abstract class IterativeFuncUnit extends FuncUnit (
    isPiplined = false,
    numStages = 1,
    isJmpUnit = false,
    isAluUnit = false,
    isCSRUnit = false
) {
    val rUop = Reg(new MicroOp)
    val doKill = Wire(Bool())

    when (io.req.fire) {
        doKill := IsKilledByBranch(io.brUpdate, io.req.bits.uop) || io.req.bits.kill
        rUop := io.req.bits.uop
        rUop.brMask := GetNewBrMask(io.brUpdate, io.req.bits.uop)
    } .otherwise {
        doKill := IsKilledByBranch(io.brUpdate, rUop) || io.req.bits.kill
        rUop.brMask := GetNewBrMask(io.brUpdate, rUop)
    }
    io.resp.bits.uop := rUop
}

class DivUnit extends IterativeFuncUnit {
    val div = Module(new SRT16Divider())

    div.io.req.valid := io.req.valid
    div.io.req.bits.fn := io.req.bits.uop.ctrl.op_fcn
    div.io.req.bits.op1 := io.req.bits.rs1Data
    div.io.req.bits.op2 := io.req.bits.rs2Data
    io.req.ready := div.io.req.ready

    div.io.kill := this.doKill

    io.resp.valid := div.io.resp.valid && !this.doKill
    div.io.resp.ready := io.resp.ready
    io.resp.bits.data := div.io.resp.bits.data
}
