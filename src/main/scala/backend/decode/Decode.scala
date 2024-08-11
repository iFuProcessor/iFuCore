package iFu.backend

import chisel3._
import chisel3.util._

import iFu.isa.Instructions._
import iFu.common.Consts._
import iFu.common.CauseCode._
import iFu.common._
import iFu.util._
import iFu.util.ImplicitCast.uintToBitPat

trait DecodeTable {
    val DC1 = BitPat.dontCare(1)
    def decode_default: List[BitPat] =
    //
    //       is val inst?                                             bypassable
    //       |    micro-code                       uses_ldg               |  is_br
    //       |    |  iq-type                       |  uses_stq            |  |
    //       |    |     |     func unit            |  |  is_ll            |  |  is_unique(clear pipeline for it)
    //       |    |     |      |     dst_type      |  |  |     is_dbar    |  |  |  flush_on_commit
    //       |    |     |      |     |    rs1_typ  |  |  | is_sc| is_ibar |  |  |  |
    //       |    |     |      |     |    |        |  |  |  |   |  |      |  |  |  |
    //       |    |     |      |     |    | rs2_typ|  |  |  |   |  |      |  |  |  |
    //       |    |     |      |     |    |    |   |  |  |  |   |  |      |  |  |  |
        List(N, uopX, IQT_X, FU_X, RT_X, DC1, DC1, X, X, X, N,  X, N,     X, X, N, X)

    val table: Array[(BitPat, List[BitPat])]
}

class CtrlSigs extends Bundle {
    val legal           = Bool() //合法指令
    val uopc            = UInt(UOPC_SZ.W) //指令对应的uop操作
    val iq_type         = UInt(IQT_SZ.W) //issue queue type
    val fu_code         = UInt(FUC_SZ.W) //function code
    val dst_type        = UInt(1.W) //
    val rs1_type        = UInt(1.W) //
    val rs2_type        = UInt(1.W) //
    val uses_ldq        = Bool() //是否使用load
    val uses_stq        = Bool() //是否使用store
    val is_ll           = Bool()
    val is_sc           = Bool()
    val is_dbar         = Bool() //栅障指令
    val is_ibar         = Bool()
    val bypassable      = Bool() //rename中需要
    val is_br           = Bool() //
    val inst_unique     = Bool() //
    val flush_on_commit = Bool()
    def decode(instr: UInt, table: Iterable[(BitPat, List[BitPat])]) = {
        val decoder = DecodeLogic(instr, XDecode.decode_default, table)
        val sigs = Seq(
            legal, uopc, iq_type, fu_code, dst_type, rs1_type, rs2_type,
            uses_ldq, uses_stq, is_ll, is_sc, is_dbar, is_ibar,
            bypassable, is_br, inst_unique, flush_on_commit
        )
        sigs zip decoder map { case (s, d) => s := d }
        this
    }
}

object XDecode extends DecodeTable {
    val table: Array[(BitPat, List[BitPat])] = Array(
                //
                //    is val inst?                                                                     bypassable
                //        | micro-code                                           uses_ldg                  | is_br
                //        |    |           iq-type                                  | uses_stq             |  |
                //        |    |              |   func unit                         |  |  is_ll            |  | is_unique(clear pipeline for it)
                //        |    |              |       |    dst_type                 |  |  |     is_dbar    |  |  | flush_on_commit
                //        |    |              |       |       |    rs1_type         |  |  | is_sc| is_ibar |  |  |  |
                //        |    |              |       |       |       |             |  |  |  |   |  |      |  |  |  |
                //        |    |              |       |       |       |    rs2_type |  |  |  |   |  |      |  |  |  |
                //        |    |              |       |       |       |       |     |  |  |  |   |  |      |  |  |  |
                //        |    |              |       |       |       |       |     |  |  |  |   |  |      |  |  |  |
        NEMU_TRAP -> List(Y, uopNOP      , IQT_INT, FU_X  , RT_X  , RT_X  , RT_X  , N, N, N, N,  N, N,     N, N, N, N),

        LLW       -> List(Y, uopLLW      , IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_X  , Y, N, Y, N,  N, N,     N, N, Y, Y),
        SCW       -> List(Y, uopSC_AG    , IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_FIX, N, Y, N, Y,  N, N,     N, N, Y, Y),
        PRELD     -> List(Y, uopNOP      , IQT_MEM, FU_MEM, RT_X  , RT_FIX, RT_X  , N, N, N, N,  N, N,     N, N, N, N),
        DBAR      -> List(Y, uopNOP      , IQT_INT, FU_X  , RT_X  , RT_X  , RT_X  , N, N, N, N,  Y, N,     N, N, N, N),
        IBAR      -> List(Y, uopNOP      , IQT_INT, FU_X  , RT_X  , RT_X  , RT_X  , N, N, N, N,  N, Y,     N, N, Y, Y),
        SRLIW     -> List(Y, uopSRLIW    , IQT_INT, FU_ALU, RT_FIX, RT_FIX, RT_X  , N, N, N, N,  N, N,     Y, N, N, N),
        SRAIW     -> List(Y, uopSRAIW    , IQT_INT, FU_ALU, RT_FIX, RT_FIX, RT_X  , N, N, N, N,  N, N,     Y, N, N, N),
        SRLW      -> List(Y, uopSRLW     , IQT_INT, FU_ALU, RT_FIX, RT_FIX, RT_FIX, N, N, N, N,  N, N,     Y, N, N, N),
        SRAW      -> List(Y, uopSRAW     , IQT_INT, FU_ALU, RT_FIX, RT_FIX, RT_FIX, N, N, N, N,  N, N,     Y, N, N, N),
        SLLIW     -> List(Y, uopSLLIW    , IQT_INT, FU_ALU, RT_FIX, RT_FIX, RT_X  , N, N, N, N,  N, N,     Y, N, N, N),
        MODWU     -> List(Y, uopMODWU    , IQT_INT, FU_DIV, RT_FIX, RT_FIX, RT_FIX, N, N, N, N,  N, N,     N, N, N, N),
        DIVWU     -> List(Y, uopDIVWU    , IQT_INT, FU_DIV, RT_FIX, RT_FIX, RT_FIX, N, N, N, N,  N, N,     N, N, N, N),
        DIVW      -> List(Y, uopDIVW     , IQT_INT, FU_DIV, RT_FIX, RT_FIX, RT_FIX, N, N, N, N,  N, N,     N, N, N, N),
        MODW      -> List(Y, uopMODW     , IQT_INT, FU_DIV, RT_FIX, RT_FIX, RT_FIX, N, N, N, N,  N, N,     N, N, N, N),
        MULHWU    -> List(Y, uopMULHWU   , IQT_INT, FU_MUL, RT_FIX, RT_FIX, RT_FIX, N, N, N, N,  N, N,     N, N, N, N),
        MULHW     -> List(Y, uopMULHW    , IQT_INT, FU_MUL, RT_FIX, RT_FIX, RT_FIX, N, N, N, N,  N, N,     N, N, N, N),
        MULW      -> List(Y, uopMULW     , IQT_INT, FU_MUL, RT_FIX, RT_FIX, RT_FIX, N, N, N, N,  N, N,     N, N, N, N),
        ORN       -> List(Y, uopORN      , IQT_INT, FU_ALU, RT_FIX, RT_FIX, RT_FIX, N, N, N, N,  N, N,     Y, N, N, N),
        ANDN      -> List(Y, uopANDN     , IQT_INT, FU_ALU, RT_FIX, RT_FIX, RT_FIX, N, N, N, N,  N, N,     Y, N, N, N),
        PCADDU12I -> List(Y, uopPCADDU12I, IQT_INT, FU_JMP, RT_FIX, RT_X  , RT_X  , N, N, N, N,  N, N,     Y, N, N, N),
        PCADDI    -> List(Y, uopPCADDI   , IQT_INT, FU_JMP, RT_FIX, RT_X  , RT_X  , N, N, N, N,  N, N,     Y, N, N, N),
        LU12IW    -> List(Y, uopLU12IW   , IQT_INT, FU_ALU, RT_FIX, RT_X  , RT_X  , N, N, N, N,  N, N,     Y, N, N, N),
        OR        -> List(Y, uopOR       , IQT_INT, FU_ALU, RT_FIX, RT_FIX, RT_FIX, N, N, N, N,  N, N,     Y, N, N, N),
        NOR       -> List(Y, uopNOR      , IQT_INT, FU_ALU, RT_FIX, RT_FIX, RT_FIX, N, N, N, N,  N, N,     Y, N, N, N),
        LDW       -> List(Y, uopLD       , IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_X  , Y, N, N, N,  N, N,     N, N, N, N),
        LDH       -> List(Y, uopLD       , IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_X  , Y, N, N, N,  N, N,     N, N, N, N),
        LDHU      -> List(Y, uopLD       , IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_X  , Y, N, N, N,  N, N,     N, N, N, N),
        LDB       -> List(Y, uopLD       , IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_X  , Y, N, N, N,  N, N,     N, N, N, N),
        LDBU      -> List(Y, uopLD       , IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_X  , Y, N, N, N,  N, N,     N, N, N, N),
        STW       -> List(Y, uopSTA      , IQT_MEM, FU_MEM, RT_X  , RT_FIX, RT_FIX, N, Y, N, N,  N, N,     N, N, N, N),
        STH       -> List(Y, uopSTA      , IQT_MEM, FU_MEM, RT_X  , RT_FIX, RT_FIX, N, Y, N, N,  N, N,     N, N, N, N),
        STB       -> List(Y, uopSTA      , IQT_MEM, FU_MEM, RT_X  , RT_FIX, RT_FIX, N, Y, N, N,  N, N,     N, N, N, N),
        ADDIW     -> List(Y, uopADDIW    , IQT_INT, FU_ALU, RT_FIX, RT_FIX, RT_X  , N, N, N, N,  N, N,     Y, N, N, N),
        ANDI      -> List(Y, uopANDI     , IQT_INT, FU_ALU, RT_FIX, RT_FIX, RT_X  , N, N, N, N,  N, N,     Y, N, N, N),
        ORI       -> List(Y, uopORI      , IQT_INT, FU_ALU, RT_FIX, RT_FIX, RT_X  , N, N, N, N,  N, N,     Y, N, N, N),
        XORI      -> List(Y, uopXORI     , IQT_INT, FU_ALU, RT_FIX, RT_FIX, RT_X  , N, N, N, N,  N, N,     Y, N, N, N),
        SLTI      -> List(Y, uopSLTI     , IQT_INT, FU_ALU, RT_FIX, RT_FIX, RT_X  , N, N, N, N,  N, N,     Y, N, N, N),
        SLTUI     -> List(Y, uopSLTUI    , IQT_INT, FU_ALU, RT_FIX, RT_FIX, RT_X  , N, N, N, N,  N, N,     Y, N, N, N),
        SLLW      -> List(Y, uopSLLW     , IQT_INT, FU_ALU, RT_FIX, RT_FIX, RT_FIX, N, N, N, N,  N, N,     Y, N, N, N),
        ADDW      -> List(Y, uopADD      , IQT_INT, FU_ALU, RT_FIX, RT_FIX, RT_FIX, N, N, N, N,  N, N,     Y, N, N, N),
        SUBW      -> List(Y, uopSUB      , IQT_INT, FU_ALU, RT_FIX, RT_FIX, RT_FIX, N, N, N, N,  N, N,     Y, N, N, N),
        SLT       -> List(Y, uopSLT      , IQT_INT, FU_ALU, RT_FIX, RT_FIX, RT_FIX, N, N, N, N,  N, N,     Y, N, N, N),
        SLTU      -> List(Y, uopSLTU     , IQT_INT, FU_ALU, RT_FIX, RT_FIX, RT_FIX, N, N, N, N,  N, N,     Y, N, N, N),
        AND       -> List(Y, uopAND      , IQT_INT, FU_ALU, RT_FIX, RT_FIX, RT_FIX, N, N, N, N,  N, N,     Y, N, N, N),
        XOR       -> List(Y, uopXOR      , IQT_INT, FU_ALU, RT_FIX, RT_FIX, RT_FIX, N, N, N, N,  N, N,     Y, N, N, N),
        JIRL      -> List(Y, uopJIRL     , IQT_INT, FU_JMP, RT_FIX, RT_FIX, RT_X  , N, N, N, N,  N, N,     N, N, N, N),
        B         -> List(Y, uopBL       , IQT_INT, FU_JMP, RT_X  , RT_X  , RT_X  , N, N, N, N,  N, N,     N, N, N, N),
        BL        -> List(Y, uopBL       , IQT_INT, FU_JMP, RT_FIX, RT_X  , RT_X  , N, N, N, N,  N, N,     N, N, N, N),
        BEQ       -> List(Y, uopBEQ      , IQT_INT, FU_ALU, RT_X  , RT_FIX, RT_FIX, N, N, N, N,  N, N,     N, Y, N, N),
        BNE       -> List(Y, uopBNE      , IQT_INT, FU_ALU, RT_X  , RT_FIX, RT_FIX, N, N, N, N,  N, N,     N, Y, N, N),
        BGE       -> List(Y, uopBGE      , IQT_INT, FU_ALU, RT_X  , RT_FIX, RT_FIX, N, N, N, N,  N, N,     N, Y, N, N),
        BGEU      -> List(Y, uopBGEU     , IQT_INT, FU_ALU, RT_X  , RT_FIX, RT_FIX, N, N, N, N,  N, N,     N, Y, N, N),
        BLT       -> List(Y, uopBLT      , IQT_INT, FU_ALU, RT_X  , RT_FIX, RT_FIX, N, N, N, N,  N, N,     N, Y, N, N),
        BLTU      -> List(Y, uopBLTU     , IQT_INT, FU_ALU, RT_X  , RT_FIX, RT_FIX, N, N, N, N,  N, N,     N, Y, N, N),
    )
}

object TLBDeocde extends DecodeTable {
   val table: Array[(BitPat, List[BitPat])] = Array(
               //
               //    is val inst?                                                                      bypassable
               //        | micro-code                                             uses_ldg                 | is_br
               //        |    |           iq-type                                   | uses_stq             |  |
               //        |    |              |   func unit                          |  |  is_ll            |  | is_unique(clear pipeline for it)
               //        |    |              |       |    dst_type                  |  |  |     is_dbar    |  |  | flush_on_commit
               //        |    |              |       |       |    rs1_type          |  |  | is_sc| is_ibar |  |  |  |
               //        |    |              |       |       |       |              |  |  |  |   |  |      |  |  |  |
               //        |    |              |       |       |       |    rs2_type  |  |  |  |   |  |      |  |  |  |
               //        |    |              |       |       |       |       |      |  |  |  |   |  |      |  |  |  |
               //        |    |              |       |       |       |       |      |  |  |  |   |  |      |  |  |  |
        TLBSRCH  -> List(Y, uopTLBSRCH   , IQT_INT, FU_CSR, RT_X  , RT_X  , RT_X  , N, N, N, N,  N, N,     N, N, Y, Y),
        TLBFILL  -> List(Y, uopTLBFILL   , IQT_INT, FU_CSR, RT_X  , RT_X  , RT_X  , N, N, N, N,  N, N,     N, N, Y, Y),
        TLBRD    -> List(Y, uopTLBRD     , IQT_INT, FU_CSR, RT_X  , RT_X  , RT_X  , N, N, N, N,  N, N,     N, N, Y, Y),
        TLBWR    -> List(Y, uopTLBWR     , IQT_INT, FU_CSR, RT_X  , RT_X  , RT_X  , N, N, N, N,  N, N,     N, N, Y, Y),
        INVTLB0  -> List(Y, uopINVTLB    , IQT_INT, FU_CSR, RT_X  , RT_FIX, RT_FIX, N, N, N, N,  N, N,     N, N, Y, Y),
        INVTLB1  -> List(Y, uopINVTLB    , IQT_INT, FU_CSR, RT_X  , RT_FIX, RT_FIX, N, N, N, N,  N, N,     N, N, Y, Y),
        INVTLB2  -> List(Y, uopINVTLB    , IQT_INT, FU_CSR, RT_X  , RT_FIX, RT_FIX, N, N, N, N,  N, N,     N, N, Y, Y),
        INVTLB3  -> List(Y, uopINVTLB    , IQT_INT, FU_CSR, RT_X  , RT_FIX, RT_FIX, N, N, N, N,  N, N,     N, N, Y, Y),
        INVTLB4  -> List(Y, uopINVTLB    , IQT_INT, FU_CSR, RT_X  , RT_FIX, RT_FIX, N, N, N, N,  N, N,     N, N, Y, Y),
        INVTLB5  -> List(Y, uopINVTLB    , IQT_INT, FU_CSR, RT_X  , RT_FIX, RT_FIX, N, N, N, N,  N, N,     N, N, Y, Y),
        INVTLB6  -> List(Y, uopINVTLB    , IQT_INT, FU_CSR, RT_X  , RT_FIX, RT_FIX, N, N, N, N,  N, N,     N, N, Y, Y),
    )
}

object CSRDecode extends DecodeTable {
    val table: Array[(BitPat, List[BitPat])] = Array(
                //
                //    is val inst?                                                                     bypassable
                //        | micro-code                                           uses_ldg                  | is_br
                //        |    |           iq-type                                  | uses_stq             |  |
                //        |    |              |   func unit                         |  |  is_ll            |  | is_unique(clear pipeline for it)
                //        |    |              |       |    dst_type                 |  |  |     is_dbar    |  |  | flush_on_commit
                //        |    |              |       |       |    rs1_type         |  |  | is_sc| is_ibar |  |  |  |
                //        |    |              |       |       |       |             |  |  |  |   |  |      |  |  |  |
                //        |    |              |       |       |       |    rs2_type |  |  |  |   |  |      |  |  |  |
                //        |    |              |       |       |       |       |     |  |  |  |   |  |      |  |  |  |
                //        |    |              |       |       |       |       |     |  |  |  |   |  |      |  |  |  |
        CSRRD     -> List(Y, uopCSRRD    , IQT_INT, FU_CSR, RT_FIX, RT_X  , RT_X  , N, N, N, N,  N, N,     Y, N, Y, Y),
        CSRWR     -> List(Y, uopCSRWR    , IQT_INT, FU_CSR, RT_FIX, RT_FIX, RT_X  , N, N, N, N,  N, N,     Y, N, Y, Y),
        CSRXCHG1  -> List(Y, uopCSRXCHG  , IQT_INT, FU_CSR, RT_FIX, RT_FIX, RT_FIX, N, N, N, N,  N, N,     Y, N, Y, Y),
        CSRXCHG2  -> List(Y, uopCSRXCHG  , IQT_INT, FU_CSR, RT_FIX, RT_FIX, RT_FIX, N, N, N, N,  N, N,     Y, N, Y, Y),
        CSRXCHG3  -> List(Y, uopCSRXCHG  , IQT_INT, FU_CSR, RT_FIX, RT_FIX, RT_FIX, N, N, N, N,  N, N,     Y, N, Y, Y),
        CSRXCHG4  -> List(Y, uopCSRXCHG  , IQT_INT, FU_CSR, RT_FIX, RT_FIX, RT_FIX, N, N, N, N,  N, N,     Y, N, Y, Y),
        CSRXCHG5  -> List(Y, uopCSRXCHG  , IQT_INT, FU_CSR, RT_FIX, RT_FIX, RT_FIX, N, N, N, N,  N, N,     Y, N, Y, Y),
        CSRXCHG6  -> List(Y, uopCSRXCHG  , IQT_INT, FU_CSR, RT_FIX, RT_FIX, RT_FIX, N, N, N, N,  N, N,     Y, N, Y, Y),
        CSRXCHG7  -> List(Y, uopCSRXCHG  , IQT_INT, FU_CSR, RT_FIX, RT_FIX, RT_FIX, N, N, N, N,  N, N,     Y, N, Y, Y),
        CSRXCHG8  -> List(Y, uopCSRXCHG  , IQT_INT, FU_CSR, RT_FIX, RT_FIX, RT_FIX, N, N, N, N,  N, N,     Y, N, Y, Y),
        CSRXCHG9  -> List(Y, uopCSRXCHG  , IQT_INT, FU_CSR, RT_FIX, RT_FIX, RT_FIX, N, N, N, N,  N, N,     Y, N, Y, Y),
        CSRXCHG10 -> List(Y, uopCSRXCHG  , IQT_INT, FU_CSR, RT_FIX, RT_FIX, RT_FIX, N, N, N, N,  N, N,     Y, N, Y, Y),
        CSRXCHG11 -> List(Y, uopCSRXCHG  , IQT_INT, FU_CSR, RT_FIX, RT_FIX, RT_FIX, N, N, N, N,  N, N,     Y, N, Y, Y),
        CSRXCHG12 -> List(Y, uopCSRXCHG  , IQT_INT, FU_CSR, RT_FIX, RT_FIX, RT_FIX, N, N, N, N,  N, N,     Y, N, Y, Y),
        CSRXCHG13 -> List(Y, uopCSRXCHG  , IQT_INT, FU_CSR, RT_FIX, RT_FIX, RT_FIX, N, N, N, N,  N, N,     Y, N, Y, Y),
        CSRXCHG14 -> List(Y, uopCSRXCHG  , IQT_INT, FU_CSR, RT_FIX, RT_FIX, RT_FIX, N, N, N, N,  N, N,     Y, N, Y, Y),
        CSRXCHG15 -> List(Y, uopCSRXCHG  , IQT_INT, FU_CSR, RT_FIX, RT_FIX, RT_FIX, N, N, N, N,  N, N,     Y, N, Y, Y),
        ERTN      -> List(Y, uopERET     , IQT_INT, FU_CSR, RT_X  , RT_X  , RT_X  , N, N, N, N,  N, N,     N, N, Y, Y),
        SYSCALL   -> List(Y, uopERET     , IQT_INT, FU_CSR, RT_X  , RT_X  , RT_X  , N, N, N, N,  N, N,     N, N, Y, Y),
        BREAK     -> List(Y, uopERET     , IQT_INT, FU_CSR, RT_X  , RT_X  , RT_X  , N, N, N, N,  N, N,     N, N, Y, Y),
    )
}

object CntDecode extends DecodeTable {
   val table: Array[(BitPat, List[BitPat])] = Array(
               //
               //    is val inst?                                                                      bypassable
               //        | micro-code                                              uses_ldg                | is_br
               //        |    |           iq-type                                   | uses_stq             |  |
               //        |    |              |   func unit                          |  |  is_ll            |  | is_unique(clear pipeline for it)
               //        |    |              |       |    dst_type                  |  |  |     is_dbar    |  |  | flush_on_commit
               //        |    |              |       |       |    rs1_type          |  |  | is_sc| is_ibar |  |  |  |
               //        |    |              |       |       |       |              |  |  |  |   |  |      |  |  |  |
               //        |    |              |       |       |       |    rs2_type  |  |  |  |   |  |      |  |  |  |
               //        |    |              |       |       |       |       |      |  |  |  |   |  |      |  |  |  |
               //        |    |              |       |       |       |       |      |  |  |  |   |  |      |  |  |  |
       RDTIMELW  -> List(Y, uopRDTIMELW  , IQT_INT, FU_CNT, RT_FIX, RT_X  , RT_X  , N ,N, N, N,  N, N,     Y, N, N, N),
       RDCNTVHW  -> List(Y, uopRDCNTVHW  , IQT_INT, FU_CNT, RT_FIX, RT_X  , RT_X  , N, N, N, N,  N, N,     Y, N, N, N),
   )
}

object WeirdDecode extends DecodeTable {
   val table: Array[(BitPat, List[BitPat])] = Array(
               //
               //    is val inst?                                                                      bypassable
               //        | micro-code                                            uses_ldg                  | is_br
               //        |    |           iq-type                                   | uses_stq             |  |
               //        |    |              |   func unit                          |  |  is_ll            |  | is_unique(clear pipeline for it)
               //        |    |              |       |    dst_type                  |  |  |     is_dbar    |  |  | flush_on_commit
               //        |    |              |       |       |    rs1_type          |  |  | is_sc| is_ibar |  |  |  |
               //        |    |              |       |       |       |              |  |  |  |   |  |      |  |  |  |
               //        |    |              |       |       |       |    rs2_type  |  |  |  |   |  |      |  |  |  |
               //        |    |              |       |       |       |       |      |  |  |  |   |  |      |  |  |  |
               //        |    |              |       |       |       |       |      |  |  |  |   |  |      |  |  |  |
        IDLE     -> List(Y, uopIDLE      , IQT_INT, FU_CSR, RT_X  , RT_X  , RT_X  , N, N, N, N,  N, N,     N, N, Y, Y),
        // now cacop is as same as ibar
        CACOP    -> List(Y, uopNOP       , IQT_INT, FU_X  , RT_X  , RT_X  , RT_X  , N, N, N, N,  N, Y,     N, N, Y, Y),
   )
}

class DecodeUnitIO() extends CoreBundle {
    val enq = new Bundle { val uop = Input(new MicroOp()) }
    val deq = new Bundle { val uop = Output(new MicroOp()) }
    val interrupt = Input(Bool())
}

//TODO 添加对CSR环境下异常指令的检测
class DecodeUnit extends CoreModule {
    val io = IO(new DecodeUnitIO)

    val uop = Wire(new MicroOp)
    uop := io.enq.uop

    var decode_table = XDecode.table
    decode_table ++= CSRDecode.table
    decode_table ++= CntDecode.table
    decode_table ++= TLBDeocde.table
    decode_table ++= WeirdDecode.table

    val inst = uop.instr
    val cs = Wire(new CtrlSigs).decode(inst, decode_table)

    val id_illegal_insn = !cs.legal
    val xcpt_valid = WireInit(false.B)
    val xcpt_cause = WireInit(0.U(CauseCode.microCauseBits.W))

    when (io.interrupt) {
        xcpt_valid := true.B
        xcpt_cause := INT
    } .elsewhen (io.enq.uop.xcpt_valid) {
        xcpt_valid := true.B
        xcpt_cause := io.enq.uop.xcpt_cause
    } .elsewhen (inst === SYSCALL || inst === BREAK) {
        when (inst === SYSCALL) {
            xcpt_valid := true.B
            xcpt_cause := SYS
        } .otherwise {
            xcpt_valid := true.B
            xcpt_cause := BRK
        }
    } .elsewhen (id_illegal_insn) {
        xcpt_valid := true.B
        xcpt_cause := INE
    }

    uop.mem_size := inst(23, 22)
    uop.mem_signed := !inst(25)
    uop.tlb_op := inst(4, 0)
    uop.use_ldq := cs.uses_ldq
    uop.use_stq := cs.uses_stq
    uop.is_ll := cs.is_ll
    uop.is_sc := cs.is_sc
    uop.is_dbar := cs.is_dbar
    uop.is_ibar := cs.is_ibar
    uop.is_unique := cs.inst_unique
    uop.flush_on_commit := cs.flush_on_commit
    uop.bypassable := cs.bypassable
    uop.immPacked := inst(25, 0)
    uop.isBr := cs.is_br
    uop.isJal := cs.uopc === uopBL
    uop.isJalr := cs.uopc === uopJIRL
    uop.uopc := cs.uopc
    uop.iqType := cs.iq_type
    uop.fuCode := cs.fu_code
    uop.ldst := inst(4, 0)
    uop.lrs1 := inst(9, 5)
    uop.lrs2 := inst(14, 10)
    uop.vaddrWriteEnable := false.B

    when(xcpt_valid && (xcpt_cause === PIF || xcpt_cause === PPI || xcpt_cause === ADEF || xcpt_cause === TLBR)){
        uop.vaddrWriteEnable := true.B
    }
    uop.xcpt_valid := xcpt_valid
    uop.xcpt_cause := xcpt_cause
    //-------------------------------------------------------------

    when (cs.uopc === uopBL) {
        uop.ldst := 1.U
    }
    when (cs.uopc === uopRDTIMELW && inst(4,0) === 0.U) {
        uop.uopc            := uopRDCNTIDW
        uop.fuCode          := FU_CSR
        uop.ldst            := inst(9, 5)
        uop.is_unique       := true.B
        uop.flush_on_commit := true.B
    } .elsewhen (cs.uopc === uopRDTIMELW) {
        uop.uopc := uopRDCNTVLW
    }
    when (cs.uopc === uopLLW || cs.uopc === uopSC_AG) {
        uop.mem_size := 2.asUInt
    }
    when (
        (uop.uopc === uopBEQ || uop.uopc === uopBNE || uop.uopc === uopBGE || uop.uopc === uopBGEU || uop.uopc === uopBLT || uop.uopc === uopBLTU) ||
        (uop.uopc === uopSTA || uop.uopc === uopSC_AG)
    ) {
        uop.lrs2 := inst(4, 0)
    }
    when (cs.uopc === uopCSRWR || cs.uopc === uopCSRXCHG) {
        uop.lrs1 := inst(4, 0)
        uop.lrs2 := inst(9, 5)
    }
    uop.ldst_val   := cs.dst_type =/= RT_X && uop.ldst =/= 0.U
    uop.dst_rtype  := cs.dst_type
    uop.lrs1_rtype := cs.rs1_type
    uop.lrs2_rtype := cs.rs2_type

    /* when (cs.uopc === uopORI && inst(21,10) === 0.U) {
        uop.uopc := uopMOV
    }*/
    when (cs.uopc === uopADDIW && inst(21, 0) === 0.U) {
        uop.uopc := uopNOP
    }

    io.deq.uop  := uop
}

class BrMaskUnit extends CoreModule {
    val io = IO(new Bundle {
        val is_branch = Input(Vec(coreWidth, Bool()))
        val will_fire = Input(Vec(coreWidth, Bool()))
        val br_tag    = Output(Vec(coreWidth, UInt(log2Ceil(maxBrCount).W)))
        val br_mask   = Output(Vec(coreWidth, UInt(maxBrCount.W)))
        val is_full   = Output(Vec(coreWidth, Bool()))
        val br_update = Input(new BrUpdateInfo())
        val flush     = Input(Bool())
    })

    val branch_mask = RegInit(0.U(maxBrCount.W))

    var allocate_mask = branch_mask
    val tag_masks = Wire(Vec(coreWidth, UInt(maxBrCount.W)))
    for (w <- 0 until coreWidth) {
        io.is_full(w) := (allocate_mask === (~0.U(maxBrCount.W)).asUInt) && io.is_branch(w)

        // find br_tag and compute next br_mask
        val new_br_tag = Wire(UInt(log2Ceil(maxBrCount).W))
        new_br_tag   := 0.U
        tag_masks(w) := 0.U

        for (i <- maxBrCount - 1 to 0 by -1) {
            when (!allocate_mask(i)) {
                new_br_tag   := i.U
                tag_masks(w) := (1.U << i.U)
            }
        }

        io.br_tag(w) := new_br_tag
        allocate_mask = Mux(io.is_branch(w), tag_masks(w) | allocate_mask, allocate_mask)
    }

    var curr_mask = branch_mask
    for (w <- 0 until coreWidth) {
        io.br_mask(w) := GetNewBrMask(io.br_update, curr_mask)
        curr_mask = Mux(io.will_fire(w), tag_masks(w) | curr_mask, curr_mask)
    }

    when (io.flush) {
        branch_mask := 0.U
    } .otherwise {
        val mask = Mux(
            io.br_update.b2.mispredict, io.br_update.b2.uop.brMask,
                                        (~0.U(maxBrCount.W)).asUInt
        )
        branch_mask := GetNewBrMask(io.br_update, curr_mask) & mask
    }
}
