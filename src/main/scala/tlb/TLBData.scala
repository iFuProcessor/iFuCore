package iFu.tlb

import chisel3._
import chisel3.util._
import iFu.common.CauseCode.ecodeBits
import iFu.common.Consts._
import iFu.common._

class TLBMeta extends CoreBundle {
    val vppn = UInt((vaddrBits - 13).W)
    val ps   = UInt(6.W)
    val g    = Bool()
    val asid = UInt(10.W)
    val e    = Bool()
}

class TLBSingleData extends CoreBundle {
    val ppn = UInt((paddrBits - 12).W)
    val plv = UInt(2.W)
    val mat = UInt(2.W)
    val d   = Bool()
    val v   = Bool()
}

class TLBEntry extends CoreBundle {
    val meta = new TLBMeta
    val data = Vec(2, new TLBSingleData)

    def matches(vppn: UInt, asid: UInt): Bool = {
        meta.e &&
            (meta.g || meta.asid === asid) &&
            Mux(meta.ps === 12.U,
                meta.vppn === vppn,
                meta.vppn(18, 9) === vppn(18, 9)
            )
    }
}

class TLBDataRReq extends CoreBundle {
    val vaddr       = UInt(vaddrBits.W)
}

class TLBDataRResp extends CoreBundle {
    val found = Bool()
    val entry = new TLBEntry
}

class TLBDataCSRContext extends CoreBundle {
    val asid_asid = UInt(10.W)

    val estat_ecode  = UInt(6.W)

    val tlbidx_index = UInt(5.W)
    val tlbidx_ps    = UInt(6.W)
    val tlbidx_ne    = Bool()

    val tlbehi_vppn  = UInt(19.W)

    val tlbelo0_v    = Bool()
    val tlbelo0_d    = Bool()
    val tlbelo0_plv  = UInt(2.W)
    val tlbelo0_mat  = UInt(2.W)
    val tlbelo0_g    = Bool()
    val tlbelo0_ppn  = UInt(20.W)

    val tlbelo1_v    = Bool()
    val tlbelo1_d    = Bool()
    val tlbelo1_plv  = UInt(2.W)
    val tlbelo1_mat  = UInt(2.W)
    val tlbelo1_g    = Bool()
    val tlbelo1_ppn  = UInt(20.W)
}

class TLBDataInstr extends CoreBundle {
    val cmd     = Input(UInt(CSR_SZ.W))
    val op      = Input(UInt(5.W))
    val rj_0_9  = Input(UInt(10.W))
    val rk      = Input(UInt(xLen.W))
}

class TLBDataCSRIO extends CoreBundle {
    val csr_ctx   = Input(new TLBDataCSRContext)
    val instr     = Input(new TLBDataInstr)
    val sch_idx   = Valid(UInt(log2Ceil(TLB_NUM).W))
    val tlb_entry = Output(new TLBEntry)
}

class TLBDataManagerIO extends CoreBundle {
    val csr    = new TLBDataCSRIO
    val r_req  = Vec(memWidth + 1, Flipped(Valid(new TLBDataRReq)))
    val r_resp = Vec(memWidth + 1, Valid(new TLBDataRResp))
    val fill_idx = if (!FPGAPlatform) Output(UInt(log2Ceil(TLB_NUM).W)) else null
}

class TLBDataManager extends CoreModule {
    val io = IO(new TLBDataManagerIO)
    io <> DontCare

// --------------------------------------------------------
// tlb entry definition
    // the valid bit TLBMeta.e is random initialized now
    // because we assume that user will call invtlb before using TLB
    val tlb_entries = Reg(Vec(TLB_NUM, new TLBEntry))

    val csr_ctx = RegNext(io.csr.csr_ctx)
// --------------------------------------------------------
    // stage 0
    val qry_idx    = PriorityEncoder(io.r_req.map(_.valid))
    // stage 1
    val qry_vaddr  = RegNext(io.r_req(qry_idx).bits.vaddr)
    val resp_valid = RegNext(VecInit(UIntToOH(qry_idx, memWidth + 1).asBools))
    val matchOH    = VecInit(
        tlb_entries.map(_.matches(qry_vaddr(vaddrBits - 1, 13), csr_ctx.asid_asid))
    )

    io.r_resp zip resp_valid foreach { case (resp, valid) =>
        resp.valid      := valid
        resp.bits.found := matchOH.asUInt.orR
        resp.bits.entry := Mux1H(matchOH, tlb_entries)
    }
// --------------------------------------------------------
    val instr = io.csr.instr
    when (instr.cmd === TLB_S) {
        val matches = VecInit(tlb_entries.map(_.matches(csr_ctx.tlbehi_vppn, csr_ctx.asid_asid)))
        io.csr.sch_idx.valid := matches.asUInt.orR
        io.csr.sch_idx.bits := OHToUInt(matches)
    } .elsewhen (instr.cmd === TLB_R) {
        io.csr.tlb_entry := tlb_entries(csr_ctx.tlbidx_index)
    } .elsewhen (instr.cmd === TLB_W || instr.cmd === TLB_F) {
        val fill_idx = RegInit(0.U(log2Ceil(TLB_NUM).W))
        fill_idx := Mux(instr.cmd === TLB_F, fill_idx + 1.U, fill_idx)
        if (!FPGAPlatform) {
            io.fill_idx := fill_idx
        }
        val idx = Mux(instr.cmd === TLB_W,
            csr_ctx.tlbidx_index, fill_idx
        )
        val entry = tlb_entries(idx)
        entry.meta.vppn   := csr_ctx.tlbehi_vppn
        entry.meta.ps     := csr_ctx.tlbidx_ps
        entry.meta.g      := csr_ctx.tlbelo0_g && csr_ctx.tlbelo1_g
        entry.meta.asid   := csr_ctx.asid_asid
        entry.meta.e      := Mux(csr_ctx.estat_ecode === 0x3F.U(ecodeBits.W), 1.B, !csr_ctx.tlbidx_ne)
        entry.data(0).ppn := csr_ctx.tlbelo0_ppn
        entry.data(0).plv := csr_ctx.tlbelo0_plv
        entry.data(0).mat := csr_ctx.tlbelo0_mat
        entry.data(0).d   := csr_ctx.tlbelo0_d
        entry.data(0).v   := csr_ctx.tlbelo0_v
        entry.data(1).ppn := csr_ctx.tlbelo1_ppn
        entry.data(1).plv := csr_ctx.tlbelo1_plv
        entry.data(1).mat := csr_ctx.tlbelo1_mat
        entry.data(1).d   := csr_ctx.tlbelo1_d
        entry.data(1).v   := csr_ctx.tlbelo1_v
    } .elsewhen (instr.cmd === TLB_I) {
        for (entry <- tlb_entries) {
            when (instr.op === 0.U || instr.op === 1.U) {
                entry.meta.e := false.B
            } .elsewhen (instr.op === 2.U) {
                when (entry.meta.g) {
                    entry.meta.e := false.B
                }
            } .elsewhen (instr.op === 3.U) {
                when (!entry.meta.g) {
                    entry.meta.e := false.B
                }
            } .elsewhen (instr.op === 4.U) {
                when (!entry.meta.g && entry.meta.asid === instr.rj_0_9) {
                    entry.meta.e := false.B
                }
            } .elsewhen (instr.op === 5.U) {
                val ppn_match = Mux(entry.meta.ps === 12.U,
                    entry.meta.vppn === instr.rk(vaddrBits - 1, 13),
                    entry.meta.vppn(18, 9) === instr.rk(vaddrBits - 1, 22)
                )
                when (
                    !entry.meta.g && entry.meta.asid === instr.rj_0_9 &&
                    ppn_match
                ) {
                    entry.meta.e := false.B
                }
            } .elsewhen (instr.op === 6.U) {
                val ppn_match = Mux(entry.meta.ps === 12.U,
                    entry.meta.vppn === instr.rk(vaddrBits - 1, 13),
                    entry.meta.vppn(18, 9) === instr.rk(vaddrBits - 1, 22)
                )
                when (
                    (entry.meta.g || entry.meta.asid === instr.rj_0_9) &&
                    ppn_match
                ) {
                    entry.meta.e := false.B
                }
            }
        }
    }
}
