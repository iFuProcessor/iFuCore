package iFu.tlb

import chisel3._
import chisel3.util._

import iFu.common._

class TLBCsrContext extends CoreBundle {
    val inv_l0_tlb = Bool()
    val asid_asid  = UInt(10.W)

    val da_mode    = Bool()
    val pg_mode    = Bool()
    val crmd_datm  = UInt(2.W)
    val crmd_plv   = UInt(2.W)

    val dmw0_plv0  = Bool()
    val dmw0_plv3  = Bool()
    val dmw0_mat   = UInt(2.W)
    val dmw0_pseg  = UInt(3.W)
    val dmw0_vseg  = UInt(3.W)

    val dmw1_plv0  = Bool()
    val dmw1_plv3  = Bool()
    val dmw1_mat   = UInt(2.W)
    val dmw1_pseg  = UInt(3.W)
    val dmw1_vseg  = UInt(3.W)
}

class L0ITLBEntry extends CoreBundle {
    val exist = Bool()
    val entry = new TLBEntry()
}
object L0ITLBEntry {
    def new_entry(entry: TLBEntry) = {
        val e = Wire(new L0ITLBEntry)
        e.exist := true.B
        e.entry := entry
        e
    }

    def fake_entry(vppn: UInt, asid: UInt) = {
        val e = Wire(new L0ITLBEntry)
        e := DontCare
        e.exist           := false.B
        e.entry.meta.vppn := vppn
        e.entry.meta.ps   := 12.U
        e.entry.meta.g    := false.B
        e.entry.meta.asid := asid
        e.entry.meta.e    := true.B
        e
    }
}

trait L0TLBState {
    val s_ready :: s_refill :: Nil = Enum(2)
}
