package iFu.backend

import chisel3._
import chisel3.util._

import iFu.isa.Instructions._
import iFu.common._   
import iFu.common.Consts._
import iFu.util.ImplicitCast.uintToBitPat

trait PreDecodeTable {
    val default = List[BitPat](N, N, N)
    val table:Array[(BitPat, List[BitPat])] = Array[(BitPat, List[BitPat])](
        ////                      is br?
        ////                      |  is bl?
        ////                      |  |  is jirl?
        ////                      |  |  |
        ////                      |  |  |
        JIRL              -> List(N, N, Y),
        B                 -> List(N, Y, N),
        BL                -> List(N, Y, N),
        BEQ               -> List(Y, N, N),
        BNE               -> List(Y, N, N),
        BLT               -> List(Y, N, N),
        BLTU              -> List(Y, N, N),
        BGE               -> List(Y, N, N),
        BGEU              -> List(Y, N, N)
    )
}

class PreDecodeSignals extends CoreBundle {
    val isRet       = Bool()
    val isCall      = Bool()
    val target      = UInt(vaddrBits.W)
    val cfiType     = UInt(CFI_SZ.W)
}

class PreDecode extends CoreModule with PreDecodeTable {
    val io = IO(new Bundle{
        val instr   = Input(UInt(coreInstrBits.W))
        val pc      = Input(UInt(vaddrBits.W))
        val out     = Output(new PreDecodeSignals)
    })

    //TODO 换成asBool
    val bpdSignals = DecodeLogic(io.instr, default, table)

    val isBr          = bpdSignals(0)(0)
    val isBl         = bpdSignals(1)(0)
    val isJirl        = bpdSignals(2)(0)

    /**
     * isRet的情况：
     *      1. 为JIRL指令
     *      2. rd=0 rj=1
     *      3. 立即数值为0
     */
    io.out.isRet := (isJirl && io.instr(4,0) === 0.U && io.instr(9,5) === 1.U && io.instr(25,10) === 0.U)
    /**
     * isCall的情况：为BL指令或link到ra的JIRL指令
     */
    io.out.isCall := (isBl && io.instr(26)) || (isJirl && io.instr(4, 0) === 1.U)

    // target输出一个32位的地址
    io.out.target := (
        Mux(isBr,
            Cat(Fill(14, io.instr(25)), io.instr(25, 10), 0.U(2.W)),
            Cat(Fill(4, io.instr(9)), io.instr(9, 0), io.instr(25, 10), 0.U(2.W))
        ).asSInt + io.pc.asSInt).asUInt

    io.out.cfiType := Mux(isBr,   CFI_BR,
                      Mux(isBl,   CFI_BL,
                      Mux(isJirl, CFI_JIRL,
                                  CFI_X)))
}
