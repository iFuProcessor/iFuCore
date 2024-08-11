package iFu.backend

import chisel3._
import chisel3.util._
import iFu.common._

class CntFuncCode {
    val SZ_CNT_FN = 4
    def FN_X    = BitPat("b????")
    def FN_VL   = BitPat("b1000")
    def FN_VH   = BitPat("b1001")

    def isLsb(cmd: UInt) = !cmd(0)
}

object CntFuncCode {
    def apply() = new CntFuncCode
}

abstract class AbstractCnt[T <: CntFuncCode](val cntFn: T) extends CoreModule {
    val io = IO(new Bundle {
        val fn = Input(UInt(cntFn.SZ_CNT_FN.W))
        val data = UInt(xLen.W)
    })
}

class Counter64 extends AbstractCnt(CntFuncCode()) {
    val cnt = RegInit(0.U((xLen * 2).W))
    cnt := cnt + 1.U

    io.data := Mux(cntFn.isLsb(io.fn), cnt(xLen - 1, 0), cnt(xLen * 2 - 1, xLen))
}
