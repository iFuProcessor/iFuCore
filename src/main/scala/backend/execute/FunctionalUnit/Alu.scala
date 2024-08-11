package iFu.backend

import chisel3._
import chisel3.util._
import iFu.common._


class AluFuncCode {
    val SZ_ALU_FN = 4
    def FN_X    = BitPat("b????")
    def FN_ADD  = 0.U(SZ_ALU_FN.W)  // 0b0000
    def FN_SUB  = 1.U(SZ_ALU_FN.W)  // 0b0001
    def FN_AND  = 2.U(SZ_ALU_FN.W)  // 0b0010
    def FN_NOR  = 3.U(SZ_ALU_FN.W)  // 0b0011
    def FN_OR   = 4.U(SZ_ALU_FN.W)  // 0b0100
    def FN_XOR  = 5.U(SZ_ALU_FN.W)  // 0b0101
    def FN_SL   = 6.U(SZ_ALU_FN.W)  // 0b0110
    def FN_SRA  = 7.U(SZ_ALU_FN.W)  // 0b0111
    def FN_SRL  = 8.U(SZ_ALU_FN.W)  // 0b1000
    def FN_ANDN = 9.U(SZ_ALU_FN.W)  // 0b1011
    def FN_ORN  = 10.U(SZ_ALU_FN.W) // 0b1100
    def FN_SLT  = 11.U(SZ_ALU_FN.W) // 0b1011
    def FN_SLTU = 13.U(SZ_ALU_FN.W) // 0b1101

    def isSub(cmd: UInt) = cmd(0)
    def isCmp(cmd: UInt) = cmd >= FN_SLT
    def cmpUnsigned(cmd: UInt) = cmd(2)
}

object AluFuncCode {
    def apply() = new AluFuncCode()
}

abstract class AbstractAlu[T <: AluFuncCode](val aluFn: T) extends CoreModule {
    val io = IO(new Bundle {
        val fn = Input(UInt(aluFn.SZ_ALU_FN.W))
        val op1 = Input(UInt(xLen.W))
        val op2 = Input(UInt(xLen.W))
        val out = Output(UInt(xLen.W))
    })
}

class Alu(val debug: Boolean = false) extends AbstractAlu(AluFuncCode()) {
    // ADD, SUB
    val op2Inverse = Mux(aluFn.isSub(io.fn), ~io.op2, io.op2)
    val op1XorOp2 = io.op1 ^ io.op2
    val sum = io.op1 + op2Inverse + aluFn.isSub(io.fn)

    // SLT, SLTU
    val slt = Mux(io.op1(xLen - 1) === io.op2(xLen - 1),
        sum(xLen - 1),  // in this case, sum = op1 - op2
        Mux(aluFn.cmpUnsigned(io.fn), io.op2(xLen - 1), io.op1(xLen - 1))
    )

    // SLL, SRL, SRA
    val (shamt, shin_r) = (io.op2(4, 0), io.op1)    // TODO: add support for xLen = 64
    val shin = Mux(io.fn === aluFn.FN_SRL || io.fn === aluFn.FN_SRA, shin_r, Reverse(shin_r))
    val shout_r = (Cat(aluFn.isSub(io.fn) & shin(xLen - 1), shin).asSInt >> shamt)(xLen - 1, 0)
    val shout_l = Reverse(shout_r)
    val shout = Mux(io.fn === aluFn.FN_SRL || io.fn === aluFn.FN_SRA, shout_r, 0.U) |
                Mux(io.fn === aluFn.FN_SL, shout_l, 0.U)

    val logicTmp1 = Mux(io.fn === aluFn.FN_XOR || io.fn === aluFn.FN_OR, op1XorOp2, 0.U) |
                Mux(io.fn === aluFn.FN_OR || io.fn === aluFn.FN_AND, io.op1 & io.op2, 0.U)
    val logicTmp2 = Mux(io.fn === aluFn.FN_ANDN, io.op1 & ~io.op2, 0.U) |
                Mux(io.fn === aluFn.FN_ORN, io.op1 | ~io.op2, 0.U) |
                Mux(io.fn === aluFn.FN_NOR, ~(io.op1 | io.op2), 0.U)
    val logic = logicTmp1 | logicTmp2

    val shift_logic = (aluFn.isCmp(io.fn) && slt) | logic | shout

    io.out := Mux(io.fn === aluFn.FN_ADD || io.fn === aluFn.FN_SUB, sum, shift_logic)

    if (debug) {
        when(io.fn === aluFn.FN_ADD) {
            printf(p"alu: fn=ADD,  ")
        }.elsewhen(io.fn === aluFn.FN_SUB) {
            printf(p"alu: fn=SUB,  ")
        }.elsewhen(io.fn === aluFn.FN_AND) {
            printf(p"alu: fn=AND,  ")
        }.elsewhen(io.fn === aluFn.FN_NOR) {
            printf(p"alu: fn=NOR,  ")
        }.elsewhen(io.fn === aluFn.FN_OR) {
            printf(p"alu: fn=OR,   ")
        }.elsewhen(io.fn === aluFn.FN_XOR) {
            printf(p"alu: fn=XOR,  ")
        }.elsewhen(io.fn === aluFn.FN_SL) {
            printf(p"alu: fn=SL,   ")
        }.elsewhen(io.fn === aluFn.FN_SRA) {
            printf(p"alu: fn=SRA,  ")
        }.elsewhen(io.fn === aluFn.FN_SRL) {
            printf(p"alu: fn=SRL,  ")
        }.elsewhen(io.fn === aluFn.FN_SLT) {
            printf(p"alu: fn=SLT,  ")
        }.elsewhen(io.fn === aluFn.FN_SLTU) {
            printf(p"alu: fn=SLTU, ")
        }.elsewhen(io.fn === aluFn.FN_ANDN) {
            printf(p"alu: fn=ANDN, ")
        }.elsewhen(io.fn === aluFn.FN_ORN) {
            printf(p"alu: fn=ORN,  ")
        }.otherwise {
            printf(p"alu: fn=UNKNOWN, ")
        }
        printf(p"op1=${io.op1.asSInt}, op2=${io.op2.asSInt}, out=${io.out.asSInt}\n")
    }

}