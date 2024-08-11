package iFu.backend

import chisel3._
import chisel3.util._

import scala.collection.mutable.ArrayBuffer

import iFu.common._
import iFu.common.Consts._

import iFu.frontend.GetPCFromFtqIO

abstract class ExecutionUnit (
    val writesIrf: Boolean        = false,
    val bypassable: Boolean       = false,
    val alwaysBypassable: Boolean = false,
    val hasMem: Boolean           = false,
    val hasCSR: Boolean           = false,
    val hasJmpUnit: Boolean       = false,
    val hasAlu: Boolean           = false,
    val hasMul: Boolean           = false,
    val hasDiv: Boolean           = false,
    val hasCnt: Boolean           = false,
    val numStages: Int
) extends CoreModule {
    val io = IO(new Bundle{
        val fu_types  = Output(Bits(FUC_SZ.W))

        val req       = Flipped(new DecoupledIO(new FuncUnitReq))

        val iresp     = new DecoupledIO(new ExeUnitResp)

        val bypass    = Output(Vec(numStages, Valid(new ExeUnitResp)))
        val brupdate  = Input(new BrUpdateInfo)

        val brinfo    = if (hasAlu) Output(new BrResolutionInfo) else null
        val getFtqPc  = if (hasJmpUnit) Flipped(new GetPCFromFtqIO) else null

        val lsu_io    = if (hasMem) Flipped(new LSUExeIO) else null
    })
    io <> DontCare
    require (bypassable || !alwaysBypassable, "[execute] an execution unit must be bypassable if it is always bypassable")

    def supportedFuncUnits = {
        SupportedFuncs(
            alu    = hasAlu,
            jmp    = hasJmpUnit,
            mem    = hasMem,
            muldiv = hasMul || hasDiv,  // should we split these up?
            csr    = hasCSR,
            cnt    = hasCnt
        )
    }

    def numReadPorts = {
        if (hasMem) 1 else 2
    }
}

class ALUExeUnit (
    hasJmpUnit: Boolean = false,
    hasCSR: Boolean     = false,
    hasAlu: Boolean     = true ,    // ALU is always supported
    hasMul: Boolean     = false,
    hasDiv: Boolean     = false,
    hasCnt: Boolean     = false,
    hasMem: Boolean     = false,
) extends ExecutionUnit (
    writesIrf        = hasAlu || hasMul || hasDiv || hasCnt,
    bypassable       = hasAlu || hasCnt,
    alwaysBypassable = (hasAlu || hasCnt) && !(hasMem || hasJmpUnit || hasMul || hasDiv /* || hasCSR */),
    hasMem           = hasMem,
    hasCSR           = hasCSR,
    hasJmpUnit       = hasJmpUnit,
    hasAlu           = hasAlu,
    hasMul           = hasMul,
    hasDiv           = hasDiv,
    hasCnt           = hasCnt,
    numStages        = if (hasAlu && hasMul) 2 else if (hasAlu) 1 else 0
) {
    val div_busy  = WireInit(false.B)

    val iresp_fu_units = ArrayBuffer[FuncUnit]()

    io.fu_types := Mux(hasAlu.B,              FU_ALU, 0.U) |
                   Mux(hasMul.B,              FU_MUL, 0.U) |
                   Mux(!div_busy && hasDiv.B, FU_DIV, 0.U) |
                   Mux(hasCnt.B,              FU_CNT, 0.U) |
                   Mux(hasCSR.B,              FU_CSR, 0.U) |
                   Mux(hasJmpUnit.B,          FU_JMP, 0.U) |
                   Mux(hasMem.B,              FU_MEM, 0.U)

    // ALU Unit -------------------------------
    var alu: ALUUnit = null
    if (hasAlu) {
        alu = Module(new ALUUnit(
            isJmpUnit = hasJmpUnit,
            isCSRUnit = hasCSR,
            numStages = numStages
        ))
        alu.io.req.valid := (io.req.valid && (
            io.req.bits.uop.fu_code_is(FU_ALU) ||
            io.req.bits.uop.fu_code_is(FU_JMP) ||
            io.req.bits.uop.fu_code_is(FU_CSR)
        ))
        alu.io.req.bits.uop      := io.req.bits.uop
        alu.io.req.bits.kill     := io.req.bits.kill
        alu.io.req.bits.rs1Data  := io.req.bits.rs1Data
        alu.io.req.bits.rs2Data  := io.req.bits.rs2Data
        alu.io.brUpdate          := io.brupdate
        alu.io.resp.ready        := true.B
        io.bypass                := alu.io.bypass
        io.brinfo                := alu.io.brInfo
        if (hasJmpUnit) { alu.io.getFtqPC <> io.getFtqPc }

        iresp_fu_units += alu
    }

    // Counter Unit ---------------------------
    var cnt: CntUnit = null
    if (hasCnt) {
        cnt = Module(new CntUnit(numStages = numStages))
        cnt.io.req <> DontCare
        cnt.io.req.valid := io.req.valid && io.req.bits.uop.fu_code_is(FU_CNT)
        cnt.io.req.bits.uop     := io.req.bits.uop
        cnt.io.req.bits.kill    := io.req.bits.kill
        cnt.io.req.bits.rs1Data := io.req.bits.rs1Data
        cnt.io.req.bits.rs2Data := io.req.bits.rs2Data
        cnt.io.brUpdate         := io.brupdate
        cnt.io.resp.ready       := true.B

        for (i <- 0 until numStages) {
            when (cnt.io.bypass(i).valid) {
                io.bypass(i) := cnt.io.bypass(i)
            }
        }

        iresp_fu_units += cnt
    }

    // Pipelined, IMul Unit ------------------
    var imul: PipelinedMulUnit = null
    if (hasMul) {
        imul = Module(new PipelinedMulUnit)
        imul.io.req <> DontCare
        imul.io.req.valid        := io.req.valid && io.req.bits.uop.fu_code_is(FU_MUL)
        imul.io.req.bits.uop     := io.req.bits.uop
        imul.io.req.bits.rs1Data := io.req.bits.rs1Data
        imul.io.req.bits.rs2Data := io.req.bits.rs2Data
        imul.io.req.bits.kill    := io.req.bits.kill
        imul.io.brUpdate         := io.brupdate
        imul.io.resp.ready       := true.B

        iresp_fu_units += imul
    }

    // Div/Rem Unit -----------------------
    var div: DivUnit = null
    val div_resp_val = WireInit(false.B)
    if (hasDiv) {
        div = Module(new DivUnit)
        div.io.req <> DontCare
        div.io.req.valid        := io.req.valid && io.req.bits.uop.fu_code_is(FU_DIV)
        div.io.req.bits.uop     := io.req.bits.uop
        div.io.req.bits.rs1Data := io.req.bits.rs1Data
        div.io.req.bits.rs2Data := io.req.bits.rs2Data
        div.io.brUpdate         := io.brupdate
        div.io.req.bits.kill    := io.req.bits.kill
        div.io.resp.ready       := !iresp_fu_units.map(_.io.resp.valid).reduce(_|_)

        div_resp_val := div.io.resp.valid
        div_busy     := !div.io.req.ready || (io.req.valid && io.req.bits.uop.fu_code_is(FU_DIV))

        iresp_fu_units += div
    }

    // Mem Unit --------------------------
    if (hasMem) {
        require(!hasAlu)
        require(numStages == 0)
        val maddrcalc = Module(new AddrGenUnit(numStages = numStages))
        maddrcalc.io.req <> DontCare
        maddrcalc.io.req.valid  := io.req.valid && io.req.bits.uop.fu_code_is(FU_MEM)
        maddrcalc.io.req.bits   := io.req.bits
        maddrcalc.io.brUpdate   := io.brupdate
        maddrcalc.io.resp.ready := true.B

        io.lsu_io.req := maddrcalc.io.resp
        io.iresp <> io.lsu_io.iresp
    }

    // Outputs (Write Port #0)  ---------------
    if (writesIrf) {
        io.iresp.valid     := iresp_fu_units.map(_.io.resp.valid).reduce(_|_)
        io.iresp.bits.uop  := PriorityMux(iresp_fu_units.map(f =>
            (f.io.resp.valid, f.io.resp.bits.uop)).toSeq
        )
        io.iresp.bits.data := PriorityMux(iresp_fu_units.map(f =>
            (f.io.resp.valid, f.io.resp.bits.data)).toSeq
        )

        if (hasCSR) {
            io.iresp.bits.csr_addr := RegNext(alu.io.imm)
            io.iresp.bits.csr_cmd  := alu.io.resp.bits.uop.ctrl.csr_cmd
            io.iresp.bits.tlb_op   := alu.io.resp.bits.uop.tlb_op
            io.iresp.bits.csr_r1   := alu.io.resp.bits.r1
            io.iresp.bits.csr_r2   := alu.io.resp.bits.r2
        }
    }
    assert ((PopCount(iresp_fu_units.map(_.io.resp.valid)) <= 1.U && !div_resp_val) ||
            (PopCount(iresp_fu_units.map(_.io.resp.valid)) <= 2.U &&  div_resp_val),
        "Multiple functional units are fighting over the write port.")
}
