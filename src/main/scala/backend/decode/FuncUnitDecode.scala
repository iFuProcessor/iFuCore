package iFu.backend

import chisel3._
import chisel3.util._
import iFu.common.Consts._
import iFu.common._
import iFu.util.ImplicitCast.uintToBitPat

class FuncCtrlSigs extends CoreBundle {
    val br_type        = UInt(BR_N.getWidth.W)
    val op_fcn         = UInt((new AluFuncCode).SZ_ALU_FN.W)
    val op1_sel        = UInt(OP1_X.getWidth.W)
    val op2_sel        = UInt(OP2_X.getWidth.W)
    val imm_sel        = UInt(immX.getWidth.W)
    val rf_wen         = Bool()
    val csr_cmd        = UInt(CSR_SZ.W)

    def decode(uopc: UInt, table: Iterable[(BitPat, List[BitPat])]) = {
        val decoder = DecodeLogic(uopc, AluRRdDecode.default, table)
        val sigs = Seq(
            br_type, op_fcn, op1_sel, op2_sel, imm_sel, rf_wen, csr_cmd
        )
        sigs zip decoder foreach {case(s,d) => s := d}
        this
    }
}

trait RRdDecodeConstants {
    val aluFn = new AluFuncCode
    val mulFn = new MultFuncCode
    val divFn = new DivFuncCode
    val cntFn = new CntFuncCode
    val default: List[BitPat] =
        List[BitPat](BR_N, aluFn.FN_ADD, OP1_X, OP2_X, immX, N, CSR_N)
    val table: Array[(BitPat, List[BitPat])]
}

object AluRRdDecode extends RRdDecodeConstants {
    val table: Array[(BitPat, List[BitPat])] =
        Array[(BitPat, List[BitPat])](
                //                      br type
                //                        |                    op1 sel     op2 sel
                //                        |                       |           |      immsel    csr_cmd
                //                        |                       |           |        | rf wen |
                //                        |       alu fcn         |           |        |   |    |
                //                        |          |            |           |        |   |    |
            BitPat(uopSRLIW)    -> List(BR_N  , aluFn.FN_SRL  , OP1_RS1, OP2_IMM , immU5 , Y, CSR_N),
            BitPat(uopSRAIW)    -> List(BR_N  , aluFn.FN_SRA  , OP1_RS1, OP2_IMM , immU5 , Y, CSR_N),
            BitPat(uopSRLW)     -> List(BR_N  , aluFn.FN_SRL  , OP1_RS1, OP2_RS2 , immX  , Y, CSR_N),
            BitPat(uopSRAW)     -> List(BR_N  , aluFn.FN_SRA  , OP1_RS1, OP2_RS2 , immX  , Y, CSR_N),
            BitPat(uopSLLIW)    -> List(BR_N  , aluFn.FN_SL   , OP1_RS1, OP2_IMM , immU5 , Y, CSR_N),
            BitPat(uopORN)      -> List(BR_N  , aluFn.FN_ORN  , OP1_RS1, OP2_RS2 , immX  , Y, CSR_N),
            BitPat(uopANDN)     -> List(BR_N  , aluFn.FN_ANDN , OP1_RS1, OP2_RS2 , immX  , Y, CSR_N),
            BitPat(uopLU12IW)   -> List(BR_N  , aluFn.FN_ADD  , OP1_ZERO,OP2_IMM , immU20, Y, CSR_N),
            BitPat(uopOR)       -> List(BR_N  , aluFn.FN_OR   , OP1_RS1, OP2_RS2 , immX  , Y, CSR_N),
            BitPat(uopNOR)      -> List(BR_N  , aluFn.FN_NOR  , OP1_RS1, OP2_RS2 , immX  , Y, CSR_N),
            BitPat(uopADDIW)    -> List(BR_N  , aluFn.FN_ADD  , OP1_RS1, OP2_IMM , immS12, Y, CSR_N),
            BitPat(uopANDI)     -> List(BR_N  , aluFn.FN_AND  , OP1_RS1, OP2_IMM , immU12, Y, CSR_N),
            BitPat(uopORI)      -> List(BR_N  , aluFn.FN_OR   , OP1_RS1, OP2_IMM , immU12, Y, CSR_N),
            BitPat(uopXORI)     -> List(BR_N  , aluFn.FN_XOR  , OP1_RS1, OP2_IMM , immU12, Y, CSR_N),
            BitPat(uopSLTI)     -> List(BR_N  , aluFn.FN_SLT  , OP1_RS1, OP2_IMM , immS12, Y, CSR_N),
            BitPat(uopSLTUI)    -> List(BR_N  , aluFn.FN_SLTU , OP1_RS1, OP2_IMM , immS12, Y, CSR_N),
            BitPat(uopSLLW)     -> List(BR_N  , aluFn.FN_SL   , OP1_RS1, OP2_RS2 , immX  , Y, CSR_N),
            BitPat(uopADD)      -> List(BR_N  , aluFn.FN_ADD  , OP1_RS1, OP2_RS2 , immX  , Y, CSR_N),
            BitPat(uopSUB)      -> List(BR_N  , aluFn.FN_SUB  , OP1_RS1, OP2_RS2 , immX  , Y, CSR_N),
            BitPat(uopSLT)      -> List(BR_N  , aluFn.FN_SLT  , OP1_RS1, OP2_RS2 , immX  , Y, CSR_N),
            BitPat(uopSLTU)     -> List(BR_N  , aluFn.FN_SLTU , OP1_RS1, OP2_RS2 , immX  , Y, CSR_N),
            BitPat(uopAND)      -> List(BR_N  , aluFn.FN_AND  , OP1_RS1, OP2_RS2 , immX  , Y, CSR_N),
            BitPat(uopXOR)      -> List(BR_N  , aluFn.FN_XOR  , OP1_RS1, OP2_RS2 , immX  , Y, CSR_N),
            BitPat(uopBEQ)      -> List(BR_EQ , aluFn.FN_SUB  , OP1_X  , OP2_X   , immS16, N, CSR_N),
            BitPat(uopBNE)      -> List(BR_NE , aluFn.FN_SUB  , OP1_X  , OP2_X   , immS16, N, CSR_N),
            BitPat(uopBGE)      -> List(BR_GE , aluFn.FN_SLT  , OP1_X  , OP2_X   , immS16, N, CSR_N),
            BitPat(uopBGEU)     -> List(BR_GEU, aluFn.FN_SLTU , OP1_X  , OP2_X   , immS16, N, CSR_N),
            BitPat(uopBLT)      -> List(BR_LT , aluFn.FN_SLT  , OP1_X  , OP2_X   , immS16, N, CSR_N),
            BitPat(uopBLTU)     -> List(BR_LTU, aluFn.FN_SLTU , OP1_X  , OP2_X   , immS16, N, CSR_N)
        )
}

object JmpRRdDecode extends RRdDecodeConstants {
    val table: Array[(BitPat, List[BitPat])] =
        Array[(BitPat, List[BitPat])](
                //                      br type
                //                        |                    op1 sel     op2 sel
                //                        |                       |           |      immsel  csr_cmd
                //                        |                       |           |        | rf wen |
                //                        |       alu fcn         |           |        |   |    |
                //                        |          |            |           |        |   |    |
            BitPat(uopJIRL)     -> List(BR_JR , aluFn.FN_ADD  , OP1_PC , OP2_NEXT, immS16, Y, CSR_N),
            BitPat(uopBL)       -> List(BR_J  , aluFn.FN_ADD  , OP1_PC , OP2_NEXT, immS26, Y, CSR_N),
            BitPat(uopPCADDU12I)-> List(BR_N  , aluFn.FN_ADD  , OP1_PC , OP2_IMM , immU20, Y, CSR_N),
            BitPat(uopPCADDI)   -> List(BR_N  , aluFn.FN_ADD  , OP1_PC , OP2_IMM , immS20, Y, CSR_N)
        )
}

object MulDivRRdDecode extends RRdDecodeConstants {
    val table: Array[(BitPat, List[BitPat])] =
        Array[(BitPat, List[BitPat])](
                //                      br type
                //                        |                    op1 sel     op2 sel
                //                        |                       |           |      immsel    csr_cmd
                //                        |                       |           |        | rf wen |
                //                        |       alu fcn         |           |        |   |    |
                //                        |          |            |           |        |   |    |
            BitPat(uopMODWU)    -> List(BR_N  , divFn.FN_REMU , OP1_RS1, OP2_RS2 , immX  , Y, CSR_N),
            BitPat(uopDIVWU)    -> List(BR_N  , divFn.FN_DIVU , OP1_RS1, OP2_RS2 , immX  , Y, CSR_N),
            BitPat(uopDIVW)     -> List(BR_N  , divFn.FN_DIV  , OP1_RS1, OP2_RS2 , immX  , Y, CSR_N),
            BitPat(uopMODW)     -> List(BR_N  , divFn.FN_REM  , OP1_RS1, OP2_RS2 , immX  , Y, CSR_N),
            BitPat(uopMULHW)    -> List(BR_N  , mulFn.FN_MULH , OP1_RS1, OP2_RS2 , immX  , Y, CSR_N),
            BitPat(uopMULHWU)   -> List(BR_N  , mulFn.FN_MULHU, OP1_RS1, OP2_RS2 , immX  , Y, CSR_N),
            BitPat(uopMULW)     -> List(BR_N  , mulFn.FN_MUL  , OP1_RS1, OP2_RS2 , immX  , Y, CSR_N)
        )
}

object MemRRdDecode extends RRdDecodeConstants {
    val table: Array[(BitPat, List[BitPat])] =
        Array[(BitPat, List[BitPat])](
                //                      br type
                //                        |                    op1 sel     op2 sel
                //                        |                       |           |     immsel   csr_cmd
                //                        |                       |           |       |  rf wen |
                //                        |       alu fcn         |           |       |    |    |
                //                        |          |            |           |       |    |    |
            BitPat(uopLD)       -> List(BR_N  , aluFn.FN_ADD  , OP1_RS1, OP2_IMM , immS12, N, CSR_N),
            BitPat(uopSTA)      -> List(BR_N  , aluFn.FN_ADD  , OP1_RS1, OP2_IMM , immS12, N, CSR_N),
            BitPat(uopSTD)      -> List(BR_N  , aluFn.FN_X    , OP1_RS1, OP2_RS2 , immS12, N, CSR_N),
            BitPat(uopLLW)      -> List(BR_N  , aluFn.FN_ADD  , OP1_RS1, OP2_IMM , immS14, N, CSR_N),
            BitPat(uopSC_AG)    -> List(BR_N  , aluFn.FN_ADD  , OP1_RS1, OP2_ZERO, immS14, N, CSR_N)
        )
}

object CsrRRdDecode extends RRdDecodeConstants {
    val table: Array[(BitPat, List[BitPat])] =
        Array[(BitPat, List[BitPat])](
                //                    br type
                //                      |                    op1 sel     op2 sel
                //                      |                       |           |      immsel   csr_cmd
                //                      |                       |           |        |  rf wen |
                //                      |       alu fcn         |           |        |    |    |
                //                      |          |            |           |        |    |    |
            BitPat(uopCSRWR)   -> List(BR_N , aluFn.FN_ADD  , OP1_RS1 , OP2_ZERO, immCSR, Y, CSR_W),
            BitPat(uopCSRRD)   -> List(BR_N , aluFn.FN_ADD  , OP1_ZERO, OP2_ZERO, immCSR, Y, CSR_R),
            BitPat(uopCSRXCHG) -> List(BR_N , aluFn.FN_ADD  , OP1_RS1 , OP2_RS2 , immCSR, Y, CSR_M),
            BitPat(uopERET)    -> List(BR_N , aluFn.FN_ADD  , OP1_X   , OP2_X   , immX  , N, CSR_E),
            BitPat(uopIDLE)    -> List(BR_N , aluFn.FN_ADD  , OP1_X   , OP2_X   , immX  , N, CSR_I),
        )
}

object CntRRdDecode extends RRdDecodeConstants {
    val table: Array[(BitPat, List[BitPat])] =
        Array[(BitPat, List[BitPat])](
                //                    br type
                //                      |                    op1 sel     op2 sel
                //                      |                       |           |      immsel     csr_cmd
                //                      |                       |           |        |  rf wen |
                //                      |       alu fcn         |           |        |    |    |
                //                      |          |            |           |        |    |    |
            BitPat(uopRDCNTIDW)-> List(BR_N , aluFn.FN_ADD  , OP1_ZERO, OP2_ZERO, immCID, Y, CSR_R),
            BitPat(uopRDCNTVLW)-> List(BR_N , cntFn.FN_VL   , OP1_X   , OP2_X   , immX  , Y, CSR_N),
            BitPat(uopRDCNTVHW)-> List(BR_N , cntFn.FN_VH   , OP1_X   , OP2_X   , immX  , Y, CSR_N)
        )
}

object TLBRRdDecode extends RRdDecodeConstants {
    val table: Array[(BitPat, List[BitPat])] =
        Array[(BitPat, List[BitPat])](
                //                    br type
                //                      |                    op1 sel     op2 sel
                //                      |                       |           |      immsel   csr_cmd
                //                      |                       |           |        |  rf wen |
                //                      |       alu fcn         |           |        |    |    |
                //                      |          |            |           |        |    |    |
            BitPat(uopTLBSRCH) -> List(BR_N , aluFn.FN_ADD  , OP1_X   , OP2_X   , immX  , N, TLB_S),
            BitPat(uopTLBFILL) -> List(BR_N , aluFn.FN_ADD  , OP1_X   , OP2_X   , immX  , N, TLB_F),
            BitPat(uopTLBRD)   -> List(BR_N , aluFn.FN_ADD  , OP1_X   , OP2_X   , immX  , N, TLB_R),
            BitPat(uopTLBWR)   -> List(BR_N , aluFn.FN_ADD  , OP1_X   , OP2_X   , immX  , N, TLB_W),
            BitPat(uopINVTLB)  -> List(BR_N , aluFn.FN_ADD  , OP1_RS1 , OP2_RS2 , immX  , N, TLB_I),
        )
}

class RegisterReadDecode(supportedUnits: SupportedFuncs) extends CoreModule {
    val io = IO(new CoreBundle {
        val iss_valid = Input(Bool())
        val iss_uop   = Input(new MicroOp)

        val rrd_valid = Output(Bool())
        val rrd_uop   = Output(new MicroOp)
    })

    // Issued Instruction
    io.rrd_uop   := io.iss_uop

    var dec_table = AluRRdDecode.table
    if (supportedUnits.jmp)    dec_table ++= JmpRRdDecode.table
    if (supportedUnits.mem)    dec_table ++= MemRRdDecode.table
    if (supportedUnits.muldiv) dec_table ++= MulDivRRdDecode.table
    if (supportedUnits.csr) {
        dec_table ++= CsrRRdDecode.table
        dec_table ++= TLBRRdDecode.table
    }
    if (supportedUnits.cnt)    dec_table ++= CntRRdDecode.table
    val rrd_cs = Wire(new FuncCtrlSigs).decode(io.rrd_uop.uopc, dec_table)

    io.rrd_valid := io.iss_valid

    io.rrd_uop.ctrl.br_type := rrd_cs.br_type
    io.rrd_uop.ctrl.op1_sel := rrd_cs.op1_sel
    io.rrd_uop.ctrl.op2_sel := rrd_cs.op2_sel
    io.rrd_uop.ctrl.imm_sel := rrd_cs.imm_sel
    io.rrd_uop.ctrl.op_fcn  := rrd_cs.op_fcn
    io.rrd_uop.ctrl.is_load := io.rrd_uop.uopc === uopLD  || io.rrd_uop.uopc === uopLLW
    io.rrd_uop.ctrl.is_sta  := io.rrd_uop.uopc === uopSTA || io.rrd_uop.uopc === uopSC_AG
    io.rrd_uop.ctrl.is_std  := io.rrd_uop.uopc === uopSTD
    io.rrd_uop.ctrl.csr_cmd := rrd_cs.csr_cmd
}
