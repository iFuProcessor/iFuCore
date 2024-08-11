package iFu.backend

import chisel3._
import chisel3.util._

import iFu.common._
import iFu.common.Consts._
import iFu.util._

class RegisterRead (
    issueWidth: Int,
    supportedUnitsArray: Seq[SupportedFuncs],
    numReadPortsArray: Seq[Int],
    numTotalBypassPorts: Int,
    registerWidth: Int
) extends CoreModule {
    val io = IO(new Bundle{
        val iss_valids = Input(Vec(issueWidth, Bool()))
        val iss_uops   = Input(Vec(issueWidth, new MicroOp))

        val rf_read_ports  = Flipped(Vec(numReadPortsArray.sum, new RegisterFileReadPortIO(pregSz, registerWidth)))

        val bypass   = Input(Vec(numTotalBypassPorts, Valid(new ExeUnitResp(registerWidth))))

        val exe_reqs = Vec(issueWidth, Decoupled(new FuncUnitReq))

        val kill     = Input(Bool())
        val brupdate = Input(new BrUpdateInfo())
    })
    io.exe_reqs.foreach(_.bits.kill := io.kill)

    var idx = 0
    for (w <- 0 until issueWidth) {
        val numReadPorts = numReadPortsArray(w)

        val rrd_valid = Reg(Bool())
        val rrd_uop   = Reg(new MicroOp)
        // clock 0: func decode
        val dec_unit = Module(new RegisterReadDecode(supportedUnitsArray(w)))
        dec_unit.io.iss_valid := io.iss_valids(w)
        dec_unit.io.iss_uop   := io.iss_uops(w)

        rrd_valid := (
            dec_unit.io.rrd_valid && !IsKilledByBranch(io.brupdate,dec_unit.io.rrd_uop)
        )
        rrd_uop   := GetNewUopAndBrMask(dec_unit.io.rrd_uop, io.brupdate)

        val rrd_rs1  = Wire(UInt(registerWidth.W))
        val rrd_rs2  = if (numReadPorts == 2) Wire(UInt(registerWidth.W)) else null
        val rs1_addr = if (numReadPorts == 2) {
            io.iss_uops(w).prs1
        } else {
            Mux(io.iss_uops(w).lrs2_rtype === RT_FIX, io.iss_uops(w).prs2, io.iss_uops(w).prs1)
        }
        val rs2_addr = if (numReadPorts == 2) io.iss_uops(w).prs2 else null
        // clock 0: send read request to register file
        io.rf_read_ports(idx + 0).addr := rs1_addr
        if (numReadPorts == 2) io.rf_read_ports(idx + 1).addr := rs2_addr
        // clock 1: read data from register file
        rrd_rs1 := Mux(
            RegNext(rs1_addr === 0.U), 0.U, io.rf_read_ports(idx + 0).data
        )
        if (numReadPorts == 2) rrd_rs2 := Mux(
            RegNext(rs2_addr === 0.U), 0.U, io.rf_read_ports(idx + 1).data
        )
        idx += numReadPorts

        // clock 1: do bypass
        val bypassed_rs1 = Wire(UInt(registerWidth.W))
        val bypassed_rs2 = if (numReadPorts == 2) Wire(UInt(registerWidth.W)) else null
        val prs1 = if (numReadPorts == 2) {
            rrd_uop.prs1
        } else {
            Mux(rrd_uop.lrs2_rtype === RT_FIX, rrd_uop.prs2, rrd_uop.prs1)
        }
        val lrs1Rtype = if (numReadPorts == 2) {
            rrd_uop.lrs1_rtype
        } else {
            Mux(rrd_uop.lrs2_rtype === RT_FIX, rrd_uop.lrs2_rtype, rrd_uop.lrs1_rtype)
        }
        val prs2 = if (numReadPorts == 2) rrd_uop.prs2 else null
        val lrs2Rtype = if (numReadPorts == 2) rrd_uop.lrs2_rtype else null
        var rs1_cases = Seq((false.B, 0.U(registerWidth.W)))
        var rs2_cases = if (numReadPorts == 2) Seq((false.B, 0.U(registerWidth.W))) else null
        io.bypass foreach { bypass =>
            rs1_cases ++= Seq((
                bypass.valid && (prs1 === bypass.bits.uop.pdst) && bypass.bits.uop.rf_wen &&
                bypass.bits.uop.dst_rtype === RT_FIX && lrs1Rtype === RT_FIX && (prs1 =/= 0.U),
                bypass.bits.data
            ))
            if (numReadPorts == 2) {
                rs2_cases ++= Seq((
                    bypass.valid && (prs2 === bypass.bits.uop.pdst) && bypass.bits.uop.rf_wen &&
                    bypass.bits.uop.dst_rtype === RT_FIX && lrs2Rtype === RT_FIX && (prs2 =/= 0.U),
                    bypass.bits.data
                ))
            }
        }
        bypassed_rs1 := MuxCase(rrd_rs1, rs1_cases)
        if (numReadPorts == 2) bypassed_rs2 := MuxCase(rrd_rs2, rs2_cases)

        // clock 2: send request to execution units
        val exe_req_valid = RegInit(false.B)
        val exe_req_uop   = Reg(new MicroOp)
        val exe_req_rs1   = Reg(UInt(registerWidth.W))
        val exe_req_rs2   = if (numReadPorts == 2) Reg(UInt(registerWidth.W)) else null

        val killed = io.kill || IsKilledByBranch(io.brupdate, rrd_uop)
        exe_req_valid      := Mux(killed, false.B, rrd_valid)
        exe_req_uop        := rrd_uop
        exe_req_uop.brMask := GetNewBrMask(io.brupdate, rrd_uop)

        exe_req_rs1 := bypassed_rs1
        if (numReadPorts == 2) exe_req_rs2 := bypassed_rs2

        io.exe_reqs(w).valid        := exe_req_valid
        io.exe_reqs(w).bits.uop     := exe_req_uop
        io.exe_reqs(w).bits.rs1Data := exe_req_rs1
        io.exe_reqs(w).bits.rs2Data := (if (numReadPorts == 2) exe_req_rs2 else DontCare)
    }
}
