package iFu.tlb

import chisel3._
import chisel3.util._

import iFu.common._
import iFu.common.Consts._

class DTLBException extends CoreBundle {
    val xcpt_cause = UInt(CauseCode.microCauseBits.W)
}

class DTLBReq extends CoreBundle {
    val vaddr   = UInt(vaddrBits.W)
    val size    = UInt(2.W)
    val use_ldq = Bool()
    val use_stq = Bool()
}

class DTLBResp extends CoreBundle {
    val paddr          = UInt(paddrBits.W)
    val exception      = Valid(new DTLBException())
    val is_uncacheable = Bool()
}

class DTLBIO extends CoreBundle {
    val req              = Vec(memWidth, Flipped(Valid(new DTLBReq)))
    val resp             = Vec(memWidth, Output(new DTLBResp))
    val r_req            = Vec(memWidth, Valid(new TLBDataRReq))
    val r_resp           = Vec(memWidth, Flipped(Valid(new TLBDataRResp)))
    val dtlb_csr_context = Input(new TLBCsrContext)
}

class DTLB(num_l0_dtlb_entries: Int = 2) extends CoreModule with L0TLBState {
    require(isPow2(num_l0_dtlb_entries))

    val io = IO(new DTLBIO)

    val state     = RegInit(s_ready)
    val state_nxt = WireInit(state)
    state := state_nxt

    // L0 DTLB
    val l0_entry = (0 until memWidth) map { w => 
        RegInit(VecInit(
            Seq.fill(num_l0_dtlb_entries)(0.U.asTypeOf(new L0ITLBEntry))
        ))
    }

    val csr_regs = io.dtlb_csr_context

    // addr translation
    val l0_miss = WireInit(VecInit(Seq.fill(memWidth)(false.B)))
    val use_page_table = WireInit(VecInit(Seq.fill(memWidth)(false.B)))
    for (w <- 0 until memWidth) {
        val trans_resp   = WireInit(0.U.asTypeOf(new DTLBResp))
        val vaddr        = io.req(w).bits.vaddr
        val l0_hit_oh    = VecInit(l0_entry(w).map(
            e => e.entry.matches(vaddr(vaddrBits - 1, 13), csr_regs.asid_asid)
        ))
        val l0_hit       = l0_hit_oh.asUInt.orR
        val l0_hit_idx   = OHToUInt(l0_hit_oh)
        val l0_hit_entry = l0_entry(w)(l0_hit_idx)

        when (
            (vaddr(0)            && io.req(w).bits.size === 1.U) ||
            (vaddr(1, 0) =/= 0.U && io.req(w).bits.size === 2.U)
        ) {
            trans_resp.exception.valid           := true.B
            trans_resp.exception.bits.xcpt_cause := CauseCode.ALE
        } .elsewhen (csr_regs.da_mode) {
            trans_resp.paddr          := vaddr
            trans_resp.is_uncacheable := csr_regs.crmd_datm === 0.U
        } .elsewhen (csr_regs.pg_mode) {
            val dmw0_en = (
                (csr_regs.dmw0_plv0 && csr_regs.crmd_plv === 0.U) ||
                (csr_regs.dmw0_plv3 && csr_regs.crmd_plv === 3.U)
            ) && vaddr(31, 29) === csr_regs.dmw0_vseg
            val dmw1_en = (
                (csr_regs.dmw1_plv0 && csr_regs.crmd_plv === 0.U) ||
                (csr_regs.dmw1_plv3 && csr_regs.crmd_plv === 3.U)
            ) && vaddr(31, 29) === csr_regs.dmw1_vseg
            if (!FPGAPlatform) dontTouch(dmw0_en)
            if (!FPGAPlatform) dontTouch(dmw1_en)

            when (dmw0_en || dmw1_en) {
                trans_resp.paddr           := Cat(
                    Mux(dmw0_en, csr_regs.dmw0_pseg, csr_regs.dmw1_pseg), vaddr(28, 0)
                )
                trans_resp.is_uncacheable  := (
                    (dmw0_en && (csr_regs.dmw0_mat === 0.U)) ||
                    (dmw1_en && (csr_regs.dmw1_mat === 0.U))
                )
            } .otherwise {
                use_page_table(w) := true.B
                val entry = l0_hit_entry.entry
                val odd_even_page = Mux(entry.meta.ps === 12.U, vaddr(12), vaddr(21))
                val data = entry.data(odd_even_page)
                switch (state) {
                    is (s_ready) {
                        when (!l0_hit) {
                            l0_miss(w) := io.req(w).valid
                            trans_resp.exception.valid           := true.B
                            trans_resp.exception.bits.xcpt_cause := CauseCode.MINI_EXCEPTION_L0TLB_MISS
                        } .elsewhen (!l0_hit_entry.exist) {
                            trans_resp.exception.valid           := true.B
                            trans_resp.exception.bits.xcpt_cause := CauseCode.TLBR
                        } .otherwise {
                            when (!data.v) {
                                trans_resp.exception.valid           := true.B
                                trans_resp.exception.bits.xcpt_cause := Mux(io.req(w).bits.use_ldq, CauseCode.PIL, CauseCode.PIS)
                            } .elsewhen (io.dtlb_csr_context.crmd_plv > data.plv) {
                                trans_resp.exception.valid           := true.B
                                trans_resp.exception.bits.xcpt_cause := CauseCode.PPI
                            } .elsewhen (io.req(w).bits.use_stq && !data.d) {
                                trans_resp.exception.valid           := true.B
                                trans_resp.exception.bits.xcpt_cause := CauseCode.PME
                            }
                        }
                    }
                    is (s_refill) {
                        trans_resp.exception.valid           := true.B
                        trans_resp.exception.bits.xcpt_cause := CauseCode.MINI_EXCEPTION_L0TLB_MISS
                    }
                }
                trans_resp.paddr := Mux(
                    entry.meta.ps === 12.U,
                    Cat(data.ppn, vaddr(11, 0)),
                    Cat(data.ppn(paddrBits - 13, 9), vaddr(20, 0))
                )
                trans_resp.is_uncacheable := data.mat === 0.U
            }
        }
        io.resp(w) := RegNext(trans_resp)
    }
    state_nxt := Mux(l0_miss.reduce(_||_), s_refill, state)

    // access L1 TLB
    for (w <- 0 until memWidth) {
        io.r_req(w).valid      := RegNext(l0_miss(w))
        io.r_req(w).bits.vaddr := RegNext(io.req(w).bits.vaddr)
        val r_resp = RegNext(io.r_resp(w))

        val refill_vppn = RegNext(RegNext(RegNext(io.req(w).bits.vaddr(vaddrBits - 1, 13))))
        val refill_en   = RegNext(RegNext(RegNext(l0_miss(w) && use_page_table(w)))) && (state === s_refill)
        val refill_idx  = RegInit(0.U(log2Ceil(num_l0_dtlb_entries).W))
        refill_idx := refill_idx + refill_en
        if (!FPGAPlatform) dontTouch(refill_idx)

        when (refill_en) {
            when (r_resp.valid) {
                l0_entry(w)(refill_idx) := Mux(
                    r_resp.bits.found,
                    L0ITLBEntry.new_entry(r_resp.bits.entry),
                    L0ITLBEntry.fake_entry(refill_vppn, csr_regs.asid_asid)
                )
            }
            state_nxt := s_ready
        }
        when (csr_regs.inv_l0_tlb) {
            l0_entry(w) map { e => e.entry.meta.e := false.B }
        }
    }
}
