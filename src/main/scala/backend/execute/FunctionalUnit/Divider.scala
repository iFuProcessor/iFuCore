package iFu.backend

import chisel3._
import chisel3.util._
import iFu.common._
import iFu.common.Consts._

class DivFuncCode {
    val SZ_DIV_FN = 4
    def FN_X    = BitPat("b????")
    def FN_DIV  = 1.U(SZ_DIV_FN.W)  // 0b01
    def FN_DIVU = 0.U(SZ_DIV_FN.W)  // 0b00
    def FN_REM  = 3.U(SZ_DIV_FN.W)  // 0b11
    def FN_REMU = 2.U(SZ_DIV_FN.W)  // 0b10

    def SorU(cmd: UInt) = cmd(0)
    def RorD(cmd: UInt) = cmd(1)
}

object DivFuncCode {
    def apply() = new DivFuncCode
}

class DivReq[T <: DivFuncCode](val divFn: T) extends CoreBundle {
    val fn = UInt(divFn.SZ_DIV_FN.W)
    val op1 = UInt(xLen.W)
    val op2 = UInt(xLen.W)
}

class DivResp extends CoreBundle {
    val data = UInt(xLen.W)
}

abstract class AbstractDiv[T <: DivFuncCode](val divFn: T) extends CoreModule {
    val io = IO(new Bundle {
        val kill = Input(Bool())
        val req = Flipped(Decoupled(new DivReq(divFn)))
        val resp = Decoupled(new DivResp)
    })
}

trait SRT16Enums {
    val sIdle :: sPre0 :: sPre1 :: sIter ::sPost0 :: sPost1 :: sFinish :: Nil = Enum(7)
    val quotNeg2 :: quotNeg1 :: quotZero :: quotPos1 :: quotPos2 :: Nil = Enum(5)
}

class SRT16Divider(val debug: Boolean = false) extends AbstractDiv(DivFuncCode()) with SRT16Enums {
    // 常量定义
    val lzcWidth = log2Up(xLen)
    val itnLen = 1 + xLen + 2 + 1

    // state
    val state = RegInit((1 << sIdle.litValue.toInt).U(7.W))
    // io
    io.req.ready := state(sIdle)
    io.resp.valid := state(sFinish)
    // variable definition
    val dIsZero = Wire(Bool())

    val rSumIter = Wire(UInt(itnLen.W))
    val rCarryIter = Wire(UInt(itnLen.W))

    val finalIter = Wire(Bool())

    val qNext2 = Wire(UInt(5.W))

    val quotIter = Wire(UInt(xLen.W))
    val quotIterReg = RegEnable(quotIter, state(sPre1) | state(sIter) | state(sPost0))

    val quotM1Iter = Wire(UInt(xLen.W))
    val quotM1IterReg = RegEnable(quotM1Iter, state(sPre1) | state(sIter) | state(sPost0))
    // ----- sIdle ----------------------------------------------------------
    val (a, d) = (io.req.bits.op1, io.req.bits.op2)

    val aInverter = -Mux(state(sIdle), a, quotIterReg)
    val dInverter = -Mux(state(sIdle), d, quotM1IterReg)

    val aSign = divFn.SorU(io.req.bits.fn) && a(xLen - 1)
    val dSign = divFn.SorU(io.req.bits.fn) && d(xLen - 1)
    val quotSign = Mux(state(sIdle), aSign ^ dSign, true.B) // default is negative
    val rSign = aSign
    // ----- sIdle ----------------------------------------------------------
    // ---------- sIdle - sPre0 Register ------------------------------------
    val opcode = RegEnable(divFn.RorD(io.req.bits.fn), io.req.fire)
    val aReg = RegEnable(a, io.req.fire)

    val dSignReg = RegEnable(dSign, io.req.fire)
    val quotSignReg = RegEnable(quotSign, io.req.fire | (state(sPre1) & dIsZero)) // d = 0时 返回全1
    val rSignReg = RegEnable(rSign, io.req.fire)

    val aAbsReg = RegEnable(Mux(aSign, aInverter, a), io.req.fire)
    val dAbsReg = RegEnable(Mux(dSign, dInverter, d), io.req.fire)
    // ---------- sIdle - sPre0 Register ------------------------------------
    // ----- sPre0 ----------------------------------------------------------
    val aLZC = PriorityEncoder(aAbsReg(xLen - 1, 0).asBools.reverse)
    val dLZC = PriorityEncoder(dAbsReg(xLen - 1, 0).asBools.reverse)
    // ----- sPre0 ----------------------------------------------------------
    // ---------- sPre1 - sPre1 Register ------------------------------------
    val aLZCReg = RegEnable(aLZC, state(sPre0))
    val dLZCReg = RegEnable(dLZC, state(sPre0))

    val aLessThanD = RegEnable((Cat(0.U(1.W), dLZC) - Cat(0.U(1.W), aLZC))(lzcWidth), state(sPre0))

    val aNormReg = RegEnable((aAbsReg(xLen - 1, 0) << aLZC)(xLen - 1, 0), state(sPre0))
    val dNormReg = RegEnable((dAbsReg(xLen - 1, 0) << dLZC)(xLen - 1, 0), state(sPre0))
    // ---------- sPre1 - sPre1 Register ------------------------------------
    // ----- sPre1 ----------------------------------------------------------
    val dIsOne = dLZC.andR  // d 是 0 或者 1
    dIsZero := ~dNormReg.orR // d 是 0

    val special = dIsOne | dIsZero | aLessThanD
    val quotSpecial = Mux(dIsZero,
        (-1.S(xLen.W)).asUInt, Mux(aLessThanD, 0.U, Mux(dSignReg, -aReg, aReg)))
    //       |                                |                    |      |
    //     d = 0                            a < d                d = -1  d = 1
    val remSpecial = Mux(dIsZero || aLessThanD, aReg, 0.U) // (a < d || d = 0) -> return a as rem

    val lzcRegDiff = Cat(0.U(1.W), dLZCReg) - Cat(0.U(1.W), aLZCReg)
    val rShift = lzcRegDiff(0)
    val oddIter = lzcRegDiff(1) ^ lzcRegDiff(0)

    val rSumInit = Cat(0.U(3.W), Mux(rShift, Cat(0.U(1.W), aNormReg), Cat(aNormReg, 0.U(1.W))))
    val rSumInitTrunc = Cat(0.U(1.W), rSumInit(itnLen - 4, itnLen - 4 - 4 + 1))
    val mInitPos1 = MuxLookup(dNormReg(xLen-2, xLen-4), "b00100".U(5.W))(
        Seq(
            0.U -> "b00100".U(5.W),
            1.U -> "b00100".U(5.W),
            2.U -> "b00100".U(5.W),
            3.U -> "b00110".U(5.W),
            4.U -> "b00110".U(5.W),
            5.U -> "b00110".U(5.W),
            6.U -> "b00110".U(5.W),
            7.U -> "b01000".U(5.W),
        )
    )
    val mInitPos2 = MuxLookup(dNormReg(xLen-2, xLen-4), "b01100".U(5.W))(
        Seq(
            0.U -> "b01100".U(5.W),
            1.U -> "b01110".U(5.W),
            2.U -> "b01111".U(5.W),
            3.U -> "b10000".U(5.W),
            4.U -> "b10010".U(5.W),
            5.U -> "b10100".U(5.W),
            6.U -> "b10110".U(5.W),
            7.U -> "b10110".U(5.W),
        )
    )
    val initCmpPos1 = rSumInitTrunc >= mInitPos1
    val initCmpPos2 = rSumInitTrunc >= mInitPos2
    val qInit = Mux(initCmpPos2, UIntToOH(quotPos2, 5), Mux(initCmpPos1, UIntToOH(quotPos1, 5), UIntToOH(quotZero, 5)))

    val dPos = Cat(0.U(1.W), dNormReg)  // +d
    val dNeg = -Cat(0.U(1.W), dNormReg) // -d

    val mNeg = VecInit(
        Cat(SignExt(MuxLookup(dNormReg(xLen-2, xLen-4), 0.U(7.W))(LookUpTable.minus_m(0)), 11), 0.U(1.W)),
        Cat(SignExt(MuxLookup(dNormReg(xLen-2, xLen-4), 0.U(7.W))(LookUpTable.minus_m(1)), 10), 0.U(2.W)),
        Cat(SignExt(MuxLookup(dNormReg(xLen-2, xLen-4), 0.U(7.W))(LookUpTable.minus_m(2)), 10), 0.U(2.W)),
        Cat(SignExt(MuxLookup(dNormReg(xLen-2, xLen-4), 0.U(7.W))(LookUpTable.minus_m(3)), 11), 0.U(1.W))
    )
    val udNeg = VecInit(
        Cat(SignExt(dPos, 66), 0.U(2.W)),
        Cat(SignExt(dPos, 67), 0.U(1.W)),
        0.U,
        Cat(SignExt(dNeg, 67), 0.U(1.W)),
        Cat(SignExt(dNeg, 66), 0.U(2.W))
    )

    val rudNeg = VecInit(Seq.tabulate(5){i => udNeg(i)(itnLen-2, itnLen-11)})
    val r2udNeg = VecInit(Seq.tabulate(5){i => udNeg(i)(itnLen-2, itnLen-13)})

    val rudPmNeg = VecInit(Seq.tabulate(5){i => VecInit(Seq.tabulate(4){ j => SignExt(rudNeg(i)(9, 1), 10) + mNeg(j)(10, 1)})})
    val r2udPmNeg = VecInit(Seq.tabulate(5){i => VecInit(Seq.tabulate(4){ j => SignExt(r2udNeg(i), 13) + SignExt(mNeg(j), 13)})})
    // ----- sPre1 ----------------------------------------------------------
    // ---------- sPre1 - sIter Register ------------------------------------
    val specialReg = RegEnable(special, state(sPre1))
    val quotSpecialReg = RegEnable(quotSpecial, state(sPre1))
    val remSpecialReg = RegEnable(remSpecial, state(sPre1))

    val udNegReg = RegEnable(udNeg, state(sPre1))
    val rudPmNegReg = RegEnable(rudPmNeg, state(sPre1))
    val r2udPmNegReg = RegEnable(r2udPmNeg, state(sPre1))

    val rSumReg = RegEnable(Mux(state(sPre1), rSumInit, rSumIter), state(sPre1) | state(sIter))
    val rCarryReg = RegEnable(Mux(state(sPre1), 0.U(itnLen.W), rCarryIter), state(sPre1) | state(sIter))
    val qPrevReg = RegEnable(Mux(state(sPre1), qInit, qNext2), state(sPre1) | state(sIter))
    // ---------- sPre1 - sIter Register ------------------------------------
    // ----- sIter ----------------------------------------------------------
    def GetSigns(signs: UInt): UInt = {
        val qVec = Wire(Vec(5, Bool()))
        qVec(quotNeg2) := signs(0) && signs(1) && signs(2)
        qVec(quotNeg1) := ~signs(0) && signs(1) && signs(2)
        qVec(quotZero) := signs(2) && ~signs(1)
        qVec(quotPos1) := signs(3) && ~signs(2) && ~signs(1)
        qVec(quotPos2) := ~signs(3) && ~signs(2) && ~signs(1)
        qVec.asUInt
    }
    def OTFC(q: UInt, quot: UInt, quotM1: UInt): (UInt, UInt) = {
        val quotNext = Mux1H(Seq(
            q(quotPos2) -> (quot << 2 | "b10".U),
            q(quotPos1) -> (quot << 2 | "b01".U),
            q(quotZero) -> (quot << 2 | "b00".U),
            q(quotNeg1) -> (quotM1 << 2 | "b11".U),
            q(quotNeg2) -> (quotM1 << 2 | "b10".U)
        ))
        val quotM1Next = Mux1H(Seq(
            q(quotPos2) -> (quot << 2 | "b01".U),
            q(quotPos1) -> (quot << 2 | "b00".U),
            q(quotZero) -> (quotM1 << 2 | "b11".U),
            q(quotNeg1) -> (quotM1 << 2 | "b10".U),
            q(quotNeg2) -> (quotM1 << 2 | "b01".U)
        ))
        (quotNext(xLen-1, 0), quotM1Next(xLen-1, 0))
    }

    val r2ws = rSumReg(itnLen - 1, itnLen - 10)
    val r2wc = rCarryReg(itnLen - 1, itnLen - 10)
    val r3ws = rSumReg(itnLen - 1, itnLen - 13)
    val r3wc = rCarryReg(itnLen - 1, itnLen - 13)

    val signs = VecInit(Seq.tabulate(4){ i => {
    val csa = Module(new CSA3_2(10))
    csa.io.in(0) := r2ws
    csa.io.in(1) := r2wc
    csa.io.in(2) := Mux1H(qPrevReg, rudPmNegReg.toSeq)(i)
        (csa.io.out(0) + (csa.io.out(1)(8, 0) << 1))(9)
    }})
    val qNext = GetSigns(signs.asUInt)

    val csaWide1 = Module(new CSA3_2(itnLen))
    val csaWide2 = Module(new CSA3_2(itnLen))
    csaWide1.io.in(0) := rSumReg << 2
    csaWide1.io.in(1) := rCarryReg << 2
    csaWide1.io.in(2) := Mux1H(qPrevReg, udNegReg.toSeq) << 2
    csaWide2.io.in(0) := csaWide1.io.out(0) << 2
    csaWide2.io.in(1) := (csaWide1.io.out(1) << 1)(itnLen - 1, 0) << 2
    csaWide2.io.in(2) := Mux1H(qNext, udNegReg.toSeq) << 2

    rSumIter := Mux(~oddIter & finalIter, csaWide1.io.out(0), csaWide2.io.out(0))
    rCarryIter := Mux(~oddIter & finalIter, (csaWide1.io.out(1) << 1)(itnLen-1, 0), (csaWide2.io.out(1) << 1)(itnLen - 1, 0))

    val qSpec = VecInit(Seq.tabulate(5){ q_spec => {
        val csa1 = Module(new CSA3_2(13))
        csa1.io.in(0) := r3ws
        csa1.io.in(1) := r3wc
        csa1.io.in(2) := SignExt(udNegReg(q_spec)(itnLen-2, itnLen-11), 13)
        val signs = VecInit(Seq.tabulate(4){ i => {
        val csa2 = Module(new CSA3_2(13))
        csa2.io.in(0) := csa1.io.out(0)
        csa2.io.in(1) := (csa1.io.out(1) << 1)(12, 0)
        csa2.io.in(2) := Mux1H(qPrevReg, r2udPmNegReg.toSeq)(i) // r2udPmNeg(OHToUInt(qPrevReg))(i)
        (csa2.io.out(0) + (csa2.io.out(1)(11, 0) << 1))(12)
        }})
        val qVec = GetSigns(signs.asUInt)
        qVec
    }})
    qNext2 := Mux1H(qNext, qSpec.toSeq)

    val quotHalfIter = OTFC(qPrevReg, quotIterReg, quotM1IterReg)._1
    val quotM1HalfIter = OTFC(qPrevReg, quotIterReg, quotM1IterReg)._2
    val quotIterNext = Mux(~oddIter && finalIter, quotHalfIter, OTFC(qNext, quotHalfIter, quotM1HalfIter)._1)
    val quotM1IterNext = Mux(~oddIter && finalIter, quotM1HalfIter, OTFC(qNext, quotHalfIter, quotM1HalfIter)._2)

    quotIter := Mux(state(sIter), quotIterNext,
                Mux(state(sPre1), 0.U(xLen.W),
                Mux(quotSignReg, aInverter, quotIterReg)))
    quotM1Iter := Mux(state(sIter), quotM1IterNext,
                  Mux(state(sPre1), 0.U(xLen.W),
                  Mux(quotSignReg, dInverter, quotM1IterReg)))
    // ----- sIter ----------------------------------------------------------
    // ---------- sIter - sPost0 Register -----------------------------------
    // ---------- sIter - sPost0 Register -----------------------------------
    // ----- sPost0 ----------------------------------------------------------
    val rNext = Wire(UInt(itnLen.W))
    val rNextPd = Wire(UInt(itnLen.W))
    when(rSignReg) {
        rNext := ~rSumReg + ~rCarryReg + 2.U
        rNextPd := ~rSumReg + ~rCarryReg + ~Cat(0.U(1.W), dNormReg, 0.U(3.W)) + 3.U
    } .otherwise {
        rNext := rSumReg + rCarryReg
        rNextPd := rSumReg + rCarryReg + Cat(0.U(1.W), dNormReg, 0.U(3.W))
    }
    // ----- sPost0 ----------------------------------------------------------
    // ---------- sPost0 - sPost1 Register -----------------------------------
    val rNextReg = RegEnable(rNext(xLen + 3, 3), state(sPost0))
    val rNextPdReg = RegEnable(rNextPd(xLen + 3, 3), state(sPost0))
    if(!FPGAPlatform)dontTouch(rNextReg)
    // ---------- sPost0 - sPost1 Register -----------------------------------
    // ----- sPost1 ----------------------------------------------------------
    val r = rNextReg
    val rPd = rNextPdReg
    val needCorr = Mux(rSignReg, ~r(xLen) & r.orR, r(xLen)) // r 的符号不对

    val rPreShifted = Mux(needCorr, rPd, r)
    val rightShifter = Module(new RightShifter(xLen, lzcWidth))
    rightShifter.io.in := rPreShifted
    rightShifter.io.shiftNum := dLZCReg
    rightShifter.io.msb := Mux(~(rPreShifted.orR), 0.U, rSignReg)
    val rShifted = rightShifter.io.out
    // ----- sPost1 ----------------------------------------------------------
    // ---------- sPost1 - sFinish Register ----------------------------------
    val rFinal = RegEnable(Mux(specialReg, remSpecialReg, rShifted), state(sPost1))
    val qFinal = RegEnable(Mux(specialReg, quotSpecialReg, Mux(needCorr, quotM1IterReg, quotIterReg)), state(sPost1))
    // ---------- sPost1 - sFinish Register ----------------------------------
    // ----- sFinish ---------------------------------------------------------
    io.resp.bits.data := Mux(opcode, rFinal, qFinal)
    // ----- sFinish ---------------------------------------------------------

    // ----------------- Loop Ctrl -----------------
    val iterNum = Wire(UInt((lzcWidth - 2).W))
    val iterNumReg = RegEnable(iterNum, state(sPre1) | state(sIter))
    iterNum := Mux(state(sPre1), (lzcRegDiff + 1.U) >> 2, iterNumReg -% 1.U) // -% : 不保留进位减法
    finalIter := iterNumReg === 0.U
    // ---------------------------------------------
    // ----------------- ---FSM--- -----------------
    when(io.kill) {
        state := UIntToOH(sIdle, 7)
    } .elsewhen(state(sIdle) && io.req.fire) {
        state := UIntToOH(sPre0, 7)
    } .elsewhen(state(sPre0)) {
        state := UIntToOH(sPre1, 7)
    } .elsewhen(state(sPre1)) {
        state := Mux(special, UIntToOH(sPost1, 7), UIntToOH(sIter, 7))
    } .elsewhen(state(sIter)) {
        state := Mux(finalIter, UIntToOH(sPost0, 7), UIntToOH(sIter, 7))
    } .elsewhen(state(sPost0)) {
        state := UIntToOH(sPost1, 7)
    } .elsewhen(state(sPost1)) {
        state := UIntToOH(sFinish, 7)
    } .elsewhen(state(sFinish) && io.resp.fire) {
        state := UIntToOH(sIdle, 7)
    } .otherwise {
        state := state
    }
    // ---------------------------------------------
    if (debug) {
        val cnt = RegInit(0.U(64.W))
        cnt := cnt + 1.U
        printf("========== cyc: %d ==========\n", cnt)
        printf("\tstate: ")
        when (OHToUInt(state) === sIdle) {
            printf("sIdle\n")
        } .elsewhen (OHToUInt(state) === sPre0) {
            printf("sPre0\n")
        } .elsewhen (OHToUInt(state) === sPre1) {
            printf("sPre1\n")
        } .elsewhen (OHToUInt(state) === sIter) {
            printf("sIter\n")
        } .elsewhen (OHToUInt(state) === sPost0) {
            printf("sPost0\n")
        } .elsewhen (OHToUInt(state) === sPost1) {
            printf("sPost1\n")
        } .elsewhen (OHToUInt(state) === sFinish) {
            printf("sFinish\n")
        } .otherwise {
            printf("unknown\n")
        }
        printf(p"\toddIter: $oddIter, finalIter: $finalIter, csaWide1.io.out(0): ${csaWide1.io.out(0)}\n")
        printf(p"\tcsaWide2.io: ${csaWide2.io}\n")
        printf(p"\trSumInit: $rSumInit, \n")
        printf(p"\trSignReg: $rSignReg, rSumReg: $rSumReg, rCarryReg: $rCarryReg\n")
        printf(p"\t${rightShifter.io}\n")
        when (io.resp.fire) {
            printf(p"\tresp(U) = ${io.resp.bits.data}, resp(S) = ${io.resp.bits.data.asSInt}\n")
        }
    }
}

object LookUpTable {
    val minus_m = Seq(
        Seq( // -m[-1]
            0.U -> "b00_11010".U(7.W),
            1.U -> "b00_11110".U(7.W),
            2.U -> "b01_00000".U(7.W),
            3.U -> "b01_00100".U(7.W),
            4.U -> "b01_00110".U(7.W),
            5.U -> "b01_01010".U(7.W),
            6.U -> "b01_01100".U(7.W),
            7.U -> "b01_10000".U(7.W)
        ),
        Seq( // -m[0]
            0.U -> "b000_0100".U(7.W),
            1.U -> "b000_0110".U(7.W),
            2.U -> "b000_0110".U(7.W),
            3.U -> "b000_0110".U(7.W),
            4.U -> "b000_1000".U(7.W),
            5.U -> "b000_1000".U(7.W),
            6.U -> "b000_1000".U(7.W),
            7.U -> "b000_1000".U(7.W)
        ),
        Seq( //-m[1]
            0.U -> "b111_1101".U(7.W),
            1.U -> "b111_1100".U(7.W),
            2.U -> "b111_1100".U(7.W),
            3.U -> "b111_1100".U(7.W),
            4.U -> "b111_1011".U(7.W),
            5.U -> "b111_1010".U(7.W),
            6.U -> "b111_1010".U(7.W),
            7.U -> "b111_1010".U(7.W)
        ),
        Seq( //-m[2]
            0.U -> "b11_01000".U(7.W),
            1.U -> "b11_00100".U(7.W),
            2.U -> "b11_00010".U(7.W),
            3.U -> "b10_11110".U(7.W),
            4.U -> "b10_11100".U(7.W),
            5.U -> "b10_11000".U(7.W),
            6.U -> "b10_10110".U(7.W),
            7.U -> "b10_10010".U(7.W)
        )
    )
}

class RightShifter(len: Int, lzcWidth: Int) extends Module {
    val io = IO(new Bundle {
        val shiftNum = Input(UInt(lzcWidth.W))
        val in = Input(UInt(len.W))
        val msb = Input(Bool())
        val out = Output(UInt(len.W))
    })
    require(len == 64 || len == 32)
    val shift = io.shiftNum
    val msb = io.msb
    val s0 = Mux(shift(0), Cat(VecInit(Seq.fill(1)(msb)).asUInt, io.in(len - 1, 1)), io.in)
    val s1 = Mux(shift(1), Cat(VecInit(Seq.fill(2)(msb)).asUInt, s0(len - 1, 2)), s0)
    val s2 = Mux(shift(2), Cat(VecInit(Seq.fill(4)(msb)).asUInt, s1(len - 1, 4)), s1)
    val s3 = Mux(shift(3), Cat(VecInit(Seq.fill(8)(msb)).asUInt, s2(len - 1, 8)), s2)
    val s4 = Mux(shift(4), Cat(VecInit(Seq.fill(16)(msb)).asUInt, s3(len - 1, 16)), s3)
    val s5 = Wire(UInt(len.W))
    if (len == 64) {
        s5 := Mux(shift(5), Cat(VecInit(Seq.fill(32)(msb)).asUInt, s4(len - 1, 32)), s4)
    } else if (len == 32) {
        s5 := s4
    }
    io.out := s5
}

object SignExt {
    def apply(a: UInt, len: Int): UInt = {
        val aLen = a.getWidth
        val signBit = a(aLen-1)
        if (aLen >= len) a(len-1,0) else Cat(Fill(len - aLen, signBit), a)
    }
}