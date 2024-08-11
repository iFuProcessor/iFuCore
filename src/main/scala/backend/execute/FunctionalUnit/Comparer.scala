package iFu.backend

import chisel3._
import chisel3.util._

import iFu.common._

class CmpFuncCode {
    val SZ_CMP_FN = 3
    def FN_X    = BitPat("b???")
    def FN_EQ   = 0.U(SZ_CMP_FN.W)  // 0b000
    def FN_NE   = 1.U(SZ_CMP_FN.W)  // 0b001
    def FN_LT   = 2.U(SZ_CMP_FN.W)  // 0b010
    def FN_GE   = 3.U(SZ_CMP_FN.W)  // 0b011
    def FN_LTU  = 6.U(SZ_CMP_FN.W)  // 0b110
    def FN_GEU  = 7.U(SZ_CMP_FN.W)  // 0b111

    def cmpUnsigned(cmd: UInt) = cmd(2)
    def cmpInverted(cmd: UInt) = cmd(0)
    def cmpEq(cmd: UInt) = !cmd(1)
}

object CmpFuncCode {
    def apply() = new CmpFuncCode()
}

abstract class AbstractCmper[T <: CmpFuncCode](val cmpFn: T) extends CoreModule {
    val io = IO(new Bundle {
        val fn = Input(UInt(cmpFn.SZ_CMP_FN.W))
        val op1 = Input(UInt(xLen.W))
        val op2 = Input(UInt(xLen.W))
        val out = Output(Bool())
    })
}

class Comparer(val debug: Boolean = false) extends AbstractCmper(CmpFuncCode()) {
    val lt = Mux(cmpFn.cmpUnsigned(io.fn), io.op1.asUInt < io.op2.asUInt, io.op1.asSInt < io.op2.asSInt)
    io.out := cmpFn.cmpInverted(io.fn) ^ Mux(cmpFn.cmpEq(io.fn), io.op1 === io.op2, lt)

    if (debug) {
        when (io.fn === cmpFn.FN_EQ) {
            printf(p"fn: FN_EQ,  ")
        }.elsewhen (io.fn === cmpFn.FN_NE) {
            printf(p"fn: FN_NE,  ")
        }.elsewhen (io.fn === cmpFn.FN_LT) {
            printf(p"fn: FN_LT,  ")
        }.elsewhen (io.fn === cmpFn.FN_GE) {
            printf(p"fn: FN_GE,  ")
        }.elsewhen (io.fn === cmpFn.FN_LTU) {
            printf(p"fn: FN_LTU, ")
        }.elsewhen (io.fn === cmpFn.FN_GEU) {
            printf(p"fn: FN_GEU, ")
        }.otherwise {
            printf(p"fn: UNKNOWN, ")
        }
        printf("op1: %d, op2: %d, out: %d\n", io.op1.asSInt, io.op2.asSInt, io.out)
    }
}