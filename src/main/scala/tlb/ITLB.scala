package iFu.tlb

import chisel3._
import chisel3.util._

import iFu.common._
import iFu.common.Consts._

class ITLBReq extends CoreBundle {
    val vaddr = UInt(vaddrBits.W)
}

// PIF PPI ADEF TLBR
class ITLBException extends CoreBundle {
    val xcpt_cause = UInt(CauseCode.microCauseBits.W)
}

class ITLBResp extends CoreBundle {
    val paddr     = UInt(paddrBits.W)
    val exception = Valid(new ITLBException)
}

class ITLBIO extends CoreBundle {
    val itlb_csr_cxt = Input(new TLBCsrContext)
    val req          = Flipped(Valid(new ITLBReq))
    val resp         = Output(new ITLBResp)
    val r_req        = Valid(new TLBDataRReq)
    val r_resp       = Flipped(Valid(new TLBDataRResp))
}

class ITLB(num_l0_itlb_entries: Int = 2) extends CoreModule with L0TLBState {
    require(isPow2(num_l0_itlb_entries))

    val io = IO(new ITLBIO)

    val state     = RegInit(s_ready)
    val state_nxt = WireInit(state)
    state := state_nxt

    // L0 ITLB
    val l0_entry = RegInit(VecInit(
        Seq.fill(num_l0_itlb_entries)(0.U.asTypeOf(new L0ITLBEntry))
    ))

    val csr_regs = WireInit(io.itlb_csr_cxt)
    // csr_regs.da_mode := io.itlb_csr_cxt.da_mode
    // csr_regs.pg_mode := io.itlb_csr_cxt.pg_mode

    val vaddr        = io.req.bits.vaddr
    val l0_hit_oh    = VecInit(l0_entry.map(
        e => e.entry.matches(vaddr(vaddrBits - 1, 13), (csr_regs.asid_asid))
    ))
    val l0_hit       = l0_hit_oh.asUInt.orR
    val l0_hit_idx   = OHToUInt(l0_hit_oh)
    val l0_hit_entry = l0_entry(l0_hit_idx)


    val dmw0_en = (
        (csr_regs.dmw0_plv0 && csr_regs.crmd_plv === 0.U) ||
        (csr_regs.dmw0_plv3 && csr_regs.crmd_plv === 3.U)
    ) && (vaddr(31, 29) === (csr_regs.dmw0_vseg))
    val dmw1_en = (
        (csr_regs.dmw1_plv0 && csr_regs.crmd_plv === 0.U) ||
        (csr_regs.dmw1_plv3 && csr_regs.crmd_plv === 3.U)
    ) && (vaddr(31, 29) === (csr_regs.dmw1_vseg))
    if (!FPGAPlatform) dontTouch(dmw0_en)
    if (!FPGAPlatform) dontTouch(dmw1_en)

    // addr translation
    val use_page_table = WireInit(false.B)
    io.resp := 0.U.asTypeOf(new ITLBResp)
    when (vaddr(1, 0) =/= 0.U) {
        io.resp.exception.valid           := true.B
        io.resp.exception.bits.xcpt_cause := CauseCode.ADEF
    } .elsewhen (csr_regs.da_mode) {
        io.resp.paddr := vaddr
    } .elsewhen (csr_regs.pg_mode) {
        when (dmw0_en || dmw1_en) {
            io.resp.paddr           := Cat(
                Mux(dmw0_en, (csr_regs.dmw0_pseg), (csr_regs.dmw1_pseg)), vaddr(28, 0)
            )
            io.resp.exception.valid := false.B
        } .otherwise {
            use_page_table := true.B
            val entry = l0_hit_entry.entry
            val odd_even_page = Mux(entry.meta.ps === 12.U, vaddr(12), vaddr(21))
            val data = entry.data(odd_even_page)
            switch (state) {
                is (s_ready) {
                    when (!l0_hit) {
                        state_nxt := Mux(io.req.valid, s_refill, s_ready)
                        io.resp.exception.valid           := true.B
                        io.resp.exception.bits.xcpt_cause := CauseCode.MINI_EXCEPTION_L0TLB_MISS
                    } .elsewhen (!l0_hit_entry.exist) {
                        io.resp.exception.valid           := true.B
                        io.resp.exception.bits.xcpt_cause := CauseCode.TLBR
                    } .otherwise {
                        when (!data.v) {
                            io.resp.exception.valid           := true.B
                            io.resp.exception.bits.xcpt_cause := CauseCode.PIF
                        } .elsewhen((csr_regs.crmd_plv) > data.plv) {
                            io.resp.exception.valid           := true.B
                            io.resp.exception.bits.xcpt_cause := CauseCode.PPI
                        }
                    }
                }
                is (s_refill) {
                    io.resp.exception.valid           := true.B
                    io.resp.exception.bits.xcpt_cause := CauseCode.MINI_EXCEPTION_L0TLB_MISS
                }
            }
            io.resp.paddr := Mux(
                entry.meta.ps === 12.U,
                Cat(data.ppn, vaddr(11, 0)),
                Cat(data.ppn(paddrBits - 13, 9), vaddr(20, 0))
            )
        }
    }

    // access L1 TLB
    io.r_req.valid      := RegNext(io.req.valid && !l0_hit)
    io.r_req.bits.vaddr := RegNext(vaddr)
    val r_resp = RegNext(io.r_resp)

    val refill_vppn = RegNext(RegNext(RegNext(vaddr(vaddrBits - 1, 13))))
    val refill_en   = RegNext(RegNext(RegNext(io.req.valid && !l0_hit && use_page_table))) && (state === s_refill)
    val refill_idx  = RegInit(0.U(log2Ceil(num_l0_itlb_entries).W))
    refill_idx := refill_idx + refill_en
    if (!FPGAPlatform) dontTouch(refill_idx)

    when (refill_en) {
        when (r_resp.valid) {
            l0_entry(refill_idx) := Mux(
                r_resp.bits.found,
                L0ITLBEntry.new_entry(r_resp.bits.entry),
                L0ITLBEntry.fake_entry(refill_vppn, (csr_regs.asid_asid))
            )
        }
        state_nxt := s_ready
    }
    when ((csr_regs.inv_l0_tlb)) {
        l0_entry map { e => e.entry.meta.e := false.B }
    }
}
