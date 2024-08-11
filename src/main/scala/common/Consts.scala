package iFu.common

import chisel3._
import chisel3.util._

object CauseCode {
    val ecodeBits = 6
    val subcodeBits = 9
    val causeCodeBits = ecodeBits + subcodeBits
    val microCauseBits = 6

    def INT = 0x0.U(ecodeBits.W)
    def PIL = 0x1.U(ecodeBits.W)
    def PIS = 0x2.U(ecodeBits.W)
    def PIF = 0x3.U(ecodeBits.W)
    def PME = 0x4.U(ecodeBits.W)
    def PPI = 0x7.U(ecodeBits.W)
    def ADEF = 0x8.U(ecodeBits.W)
    def ALE = 0x9.U(ecodeBits.W)
    def SYS = 0xb.U(ecodeBits.W)
    def BRK = 0xc.U(ecodeBits.W)
    def INE = 0xd.U(ecodeBits.W)
    def TLBR = 0x3f.U(ecodeBits.W)

    def MINI_EXCEPTION_MEM_ORDERING = 0x1a.U(ecodeBits.W)
    def MINI_EXCEPTION_L0TLB_MISS   = 0x1b.U(ecodeBits.W)

    def microCause2ecode(uCause: UInt): UInt = uCause
    def microCause2esubcode(uCause: UInt): UInt = 0.U(subcodeBits.W)
}

object Consts {
    val X = BitPat("b?")
    val Y = BitPat("b1")
    val N = BitPat("b0")

    val CSR_SZ = 4
    val CSR_N  =  0.U(CSR_SZ.W)  // 非CSR指令
    val CSR_R  =  1.U(CSR_SZ.W)  // csrrd
    val CSR_W  =  2.U(CSR_SZ.W)  // csrwr
    val CSR_M  =  3.U(CSR_SZ.W)  // csrxchg
    val CSR_E  =  4.U(CSR_SZ.W)  // ertn
    val CSR_I  =  5.U(CSR_SZ.W)  // idle
    val TLB_S  =  6.U(CSR_SZ.W)  // tlbsrch
    val TLB_R  =  7.U(CSR_SZ.W)  // tlbrd
    val TLB_W  =  8.U(CSR_SZ.W)  // tlbwr
    val TLB_F  =  9.U(CSR_SZ.W)  // tlbfill
    val TLB_I  = 10.U(CSR_SZ.W)  // invtlb

    val CFI_SZ   = 2
    val CFI_X    = 0.U(CFI_SZ.W) // Not a CFI instruction
    val CFI_BR   = 1.U(CFI_SZ.W) // Branch
    val CFI_BL   = 2.U(CFI_SZ.W) // BL
    val CFI_JIRL = 3.U(CFI_SZ.W) // JIRL

    val IQT_SZ  = 2
    val IQT_X   = BitPat.dontCare(IQT_SZ)
    val IQT_INT = 1.U(IQT_SZ.W)
    val IQT_MEM = 2.U(IQT_SZ.W)

    val FUC_SZ = 8 // TODO
    val FU_X = BitPat.dontCare(FUC_SZ)
    val FU_ALU = 1.U(FUC_SZ.W)
    val FU_JMP = 2.U(FUC_SZ.W)
    val FU_MEM = 4.U(FUC_SZ.W)
    val FU_MUL = 8.U(FUC_SZ.W)
    val FU_DIV = 16.U(FUC_SZ.W)
    val FU_CSR = 32.U(FUC_SZ.W)
    val FU_CNT = 64.U(FUC_SZ.W)

    val RT_X   = 0.U(1.W)
    val RT_FIX = 1.U(1.W)

    val immSz  = 4
    val immX   = BitPat("b????")
    val immU5  = 0.U(immSz.W)  // Cat(Fill(27,0.U),inst(14,10))
    val immU12 = 1.U(immSz.W) // Cat(Fill(20,0.U),inst(21,10))
    val immS12 = 2.U(immSz.W) //Cat(Fill(20,inst(21)),inst(21,10))
    val immS14 = 3.U(immSz.W) // Cat(Fill(16,inst(23)),inst(23,10),Fill(2,0.U))
    val immS16 = 4.U(immSz.W) // Cat(Fill(14,inst(25)),inst(25,10),Fill(2,0.U))
    val immU20 = 5.U(immSz.W) //Cat(inst(24,5),Fill(12,0.U))
    val immS20 = 6.U(immSz.W) //Cat(Fill(10,inst(24)),inst(24,5),Fill(2,0.U))
    val immS26 = 7.U(immSz.W) // Cat(Fill(4,inst(9)),inst(9,0),inst(25,10),Fill(2,0.U))
    val immCSR = 8.U(immSz.W)
    val immCID = 9.U(immSz.W)


    val BR_N   = 0.U(4.W) // Next
    val BR_NE  = 1.U(4.W) // Branch on NotEqual
    val BR_EQ  = 2.U(4.W) // Branch on Equal
    val BR_GE  = 3.U(4.W) // Branch on Greater/Equal
    val BR_GEU = 4.U(4.W) // Branch on Greater/Equal Unsigned
    val BR_LT  = 5.U(4.W) // Branch on Less Than
    val BR_LTU = 6.U(4.W) // Branch on Less Than Unsigned
    val BR_J   = 7.U(4.W) // Jump
    val BR_JR  = 8.U(4.W) // Jump Register

    val PC_PLUS4 = 0.U(2.W)
    val PC_BRJMP = 1.U(2.W)
    val PC_JIRL  = 2.U(2.W)

    val OP1_X    = BitPat("b??")
    val OP1_RS1  = 0.U(2.W) // Register Source #1
    val OP1_ZERO = 1.U(2.W)
    val OP1_PC   = 2.U(2.W)

    val OP2_X    = BitPat("b???")
    val OP2_RS2  = 0.U(3.W) // Register Source #2
    val OP2_IMM  = 1.U(3.W) // immediate
    val OP2_ZERO = 2.U(3.W) // constant 0
    val OP2_NEXT = 3.U(3.W) // constant 4 (for PC + 4)

    val BUBBLE = 0.U(32.W)

    val UOPC_SZ = 7
    val uopX = BitPat.dontCare(UOPC_SZ)

    val uopNOP = 0.U(UOPC_SZ.W)
    val uopADD = 1.U(UOPC_SZ.W)
    val uopSUB = 2.U(UOPC_SZ.W)
    val uopSLT = 3.U(UOPC_SZ.W)
    val uopSLTU = 4.U(UOPC_SZ.W)
    val uopNOR = 5.U(UOPC_SZ.W)
    val uopAND = 6.U(UOPC_SZ.W)
    val uopOR = 7.U(UOPC_SZ.W)
    val uopXOR = 8.U(UOPC_SZ.W)
    val uopLU12IW = 9.U(UOPC_SZ.W)
    val uopADDIW = 10.U(UOPC_SZ.W)
    val uopSLTI = 11.U(UOPC_SZ.W)
    val uopSLTUI = 12.U(UOPC_SZ.W)
    val uopPCADDI = 13.U(UOPC_SZ.W)
    val uopPCADDU12I = 14.U(UOPC_SZ.W)
    val uopANDN = 15.U(UOPC_SZ.W) // TODO: should alu do this?
    val uopORN = 16.U(UOPC_SZ.W) // TODO: should alu do this?
    val uopANDI = 17.U(UOPC_SZ.W)
    val uopORI = 18.U(UOPC_SZ.W)
    val uopXORI = 19.U(UOPC_SZ.W)
    val uopMULW = 20.U(UOPC_SZ.W)
    val uopMULHW = 21.U(UOPC_SZ.W)
    val uopMULHWU = 22.U(UOPC_SZ.W)
    val uopDIVW = 23.U(UOPC_SZ.W)
    val uopMODW = 24.U(UOPC_SZ.W)
    val uopDIVWU = 25.U(UOPC_SZ.W)
    val uopMODWU = 26.U(UOPC_SZ.W)
    val uopSLLIW = 27.U(UOPC_SZ.W)
    val uopSRLIW = 28.U(UOPC_SZ.W)
    val uopSRAIW = 29.U(UOPC_SZ.W)
    val uopSLLW = 30.U(UOPC_SZ.W)
    val uopSRLW = 31.U(UOPC_SZ.W)
    val uopSRAW = 32.U(UOPC_SZ.W)

    val uopJIRL = 33.U(UOPC_SZ.W)
    val uopBL = 34.U(UOPC_SZ.W)
    val uopBEQ = 36.U(UOPC_SZ.W)
    val uopBNE = 37.U(UOPC_SZ.W)
    val uopBLT = 38.U(UOPC_SZ.W)
    val uopBGE = 39.U(UOPC_SZ.W)
    val uopBLTU = 40.U(UOPC_SZ.W)
    val uopBGEU = 41.U(UOPC_SZ.W)

    val uopERET = 42.U(UOPC_SZ.W)

    val uopCSRRD = 43.U(UOPC_SZ.W)
    val uopCSRWR = 44.U(UOPC_SZ.W)
    val uopCSRXCHG = 45.U(UOPC_SZ.W)

    val uopIDLE = 46.U(UOPC_SZ.W)

    val uopTLBSRCH = 47.U(UOPC_SZ.W)
    val uopTLBRD = 48.U(UOPC_SZ.W)
    val uopTLBWR = 49.U(UOPC_SZ.W)
    val uopTLBFILL = 50.U(UOPC_SZ.W)
    val uopINVTLB = 51.U(UOPC_SZ.W)

    val uopCACOP = 52.U(UOPC_SZ.W)
    val uopPRELD = 53.U(UOPC_SZ.W)
    val uopDBAR = 54.U(UOPC_SZ.W)

    val uopLD = 55.U(UOPC_SZ.W)
    val uopSTA = 56.U(UOPC_SZ.W)
    val uopSTD = 57.U(UOPC_SZ.W)

    val uopLLW = 58.U(UOPC_SZ.W)
    val uopSC_AG = 59.U(UOPC_SZ.W)

    val uopRDCNTIDW = 60.U(UOPC_SZ.W)
    val uopRDCNTVLW = 61.U(UOPC_SZ.W)
    val uopRDCNTVHW = 62.U(UOPC_SZ.W)
    val uopRDTIMELW = 63.U(UOPC_SZ.W)

    val uopMOV = 64.U(UOPC_SZ.W)
    def NullMicroOp: MicroOp = {
        val uop = Wire(new MicroOp)
        uop := DontCare
        uop.uopc       := uopNOP
        uop.bypassable := false.B
        uop.use_stq    := false.B
        uop.use_ldq    := false.B
        uop.pdst       := 0.U
        uop.dst_rtype  := RT_X

        val cs = Wire(new CtrlSignals())
        cs := DontCare
        cs.br_type := BR_N
        cs.csr_cmd := CSR_N
        cs.is_load := false.B
        cs.is_sta  := false.B
        cs.is_std  := false.B

        uop.ctrl := cs
        uop
    }

    val TLB_NUM: Int = 32
    val TLB_INDEX_LENGTH: Int = 5
    val TLBIDX_r: Int = 16 - TLB_INDEX_LENGTH
    val TLBIDX_INDEX: Int = TLB_INDEX_LENGTH

    val PALEN: Int = 32
    val TLBELO_r: Int = 36 - PALEN
    val TLBELO_ppn: Int = PALEN - 12

    val TIMER_LENGTH: Int = 32
    val TCFG_initval: Int = TIMER_LENGTH - 2

    val CSR_CRMD = 0.U(14.W)
    val CSR_PRMD = 1.U(14.W)
    val CSR_EUEN = 2.U(14.W)
    val CSR_ECFG = 4.U(14.W)
    val CSR_ESTAT = 5.U(14.W)
    val CSR_ERA = 6.U(14.W)
    val CSR_BADV = 7.U(14.W)
    val CSR_EENTRY = 12.U(14.W)
    val CSR_TLBIDX = 16.U(14.W)
    val CSR_TLBEHI = 17.U(14.W)
    val CSR_TLBELO0 = 18.U(14.W)
    val CSR_TLBELO1 = 19.U(14.W)
    val CSR_ASID = 24.U(14.W)
    val CSR_PGDL = 25.U(14.W)
    val CSR_PGDH = 26.U(14.W)
    val CSR_PGD = 27.U(14.W)
    val CSR_CPUID = 32.U(14.W)
    val CSR_SAVE0 = 48.U(14.W)
    val CSR_SAVE1 = 49.U(14.W)
    val CSR_SAVE2 = 50.U(14.W)
    val CSR_SAVE3 = 51.U(14.W)
    val CSR_TID = 64.U(14.W)
    val CSR_TCFG = 65.U(14.W)
    val CSR_TVAL = 66.U(14.W)
    val CSR_TICLR = 68.U(14.W)
    val CSR_LLBCTL = 96.U(14.W)
    val CSR_TLBRENTRY = 136.U(14.W)
    val CSR_CTAG = 152.U(14.W)
    val CSR_DMW0 = 384.U(14.W)
    val CSR_DMW1 = 385.U(14.W) 

    val FPGAPlatform = true
}

object FlushTypes {
    def SZ =3
    def apply() = UInt(SZ.W)
    def none = 0.U
    def xcpt = 1.U
    def eret = 2.U
    def refetch = 3.U
    def next = 4.U

    def useCsrEvec(typ: UInt): Bool = typ === eret || typ === xcpt
    def useSamePC(typ: UInt): Bool  = typ === refetch

    def getType(valid: Bool, i_xcpt: Bool, i_eret: Bool, i_refetch: Bool): UInt = {
        val ret = Mux(!valid, none,
                  Mux(i_xcpt, xcpt,
                  Mux(i_eret, eret,
                  Mux(i_refetch,refetch,
                  next))))
        ret
    }
}