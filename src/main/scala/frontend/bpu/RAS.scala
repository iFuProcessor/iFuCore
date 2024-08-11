package iFu.frontend

import chisel3._
import chisel3.util._

import iFu.common._
import iFu.util._
import ram.SDPRam


class RAS extends CoreModule {
    val numRasEntries = frontendParams.bpdParams.numRasEntries
    val targetSz      = frontendParams.targetSz
    
    val io = IO(new Bundle {
        val read_idx    = Input(UInt(log2Ceil(numRasEntries).W))
        val read_tgt   = Output(UInt(targetSz.W))

        val write_valid = Input(Bool())
        val write_idx   = Input(UInt(log2Ceil(numRasEntries).W))
        val write_tgt  = Input(UInt(targetSz.W))
    })

    // val ras = Reg(Vec(numRasEntries, UInt(targetSz.W)))
    val ras = Module(new SDPRam(numRasEntries, UInt(targetSz.W)))

    ras.io.raddr := io.read_idx

    // io.read_tgt := Mux(
    //     RegNext(io.write_valid && io.write_idx === io.read_idx), RegNext(io.write_tgt),
    //     RegNext(ras(io.read_idx))
    // )

    io.read_tgt := ras.io.rdata.head


    // when (io.write_valid) {
    //     ras(io.write_idx) := io.write_tgt
    // }

    ras.io.wen := io.write_valid
    ras.io.waddr := io.write_idx
    ras.io.wdata.head := io.write_tgt
    ras.io.wstrobe := 1.U

}

class RASPtr extends CoreBundle {
    /*--------------------------*/
    val numRasEntries = frontendParams.bpdParams.numRasEntries
    /*--------------------------*/
    val bits = UInt(log2Ceil(numRasEntries).W)

    def update(
        en: Bool, is_call: Bool, is_ret: Bool
    ): RASPtr = {
        val new_ptr = Wire(new RASPtr)
        new_ptr.bits := Mux(en && is_call, WrapInc(bits, numRasEntries),
                        Mux(en && is_ret , WrapDec(bits, numRasEntries),
                                           bits))
        new_ptr
    }
}
