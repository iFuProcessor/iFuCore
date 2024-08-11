package iFu.backend

import chisel3._
import chisel3.util._

import iFu.common._
import iFu.common.Consts._
import iFu.util._

class IssueSlotIO(val numWakeupPorts: Int) extends CoreBundle {
    val valid       = Output(Bool())
    val willBeValid = Output(Bool())

    val request = Output(Bool())
    val grant   = Input(Bool())

    val brUpdate   = Input(new BrUpdateInfo)
    val kill       = Input(Bool())
    val clear      = Input(Bool())
    val ldSpecMiss = Input(Bool())

    val wakeupPorts = Vec(numWakeupPorts, Flipped(Valid(new IssueWakeup(pregSz))))
    val specLdWakeupPorts = Vec(memWidth, Flipped(Valid(UInt(pregSz.W))))

    val inUop  = Flipped(Valid(new MicroOp))
    val outUop = Output(new MicroOp)    // passed to next slot uop
    val uop    = Output(new MicroOp)    // issued uop
}

class IssueSlot(val numWakeupPorts: Int) extends CoreModule with IssueState {
    val io = IO(new IssueSlotIO(numWakeupPorts))

    val state = RegInit(s_invalid)
    val next_state = WireInit(state)
    when (io.kill) {
        state := s_invalid
    } .elsewhen (io.inUop.valid) {
        state := io.inUop.bits.iwState
    } .elsewhen (io.clear) {
        state := s_invalid
    } .otherwise {
        state := next_state
    }

    val slot_uop = Reg(new MicroOp)
    val next_uop = WireInit(UpdateBrMask(io.brUpdate, slot_uop))
    when (io.inUop.valid) {
        slot_uop := io.inUop.bits
    } .otherwise {
        slot_uop := next_uop
    }

    io.valid  := isValid(state)
    io.outUop := next_uop

    val p1          = RegInit(false.B)
    val p2          = RegInit(false.B)
    val p1_poisoned = RegInit(false.B)
    val p2_poisoned = RegInit(false.B)

    next_uop.iwState        := next_state
    next_uop.prs1_busy      := !p1
    next_uop.prs2_busy      := !p2
    next_uop.iw_p1_poisoned := p1_poisoned
    next_uop.iw_p2_poisoned := p2_poisoned

    val killed = IsKilledByBranch(io.brUpdate, slot_uop)
    when (io.kill || killed) {
        next_state := s_invalid
    } .elsewhen (io.grant && (state === s_valid_1)) {
        when (!(io.ldSpecMiss && (p1_poisoned || p2_poisoned))) {
            next_state := s_invalid
        }
    } .elsewhen (io.grant && (state === s_valid_2)) {
        when (!(io.ldSpecMiss && (p1_poisoned || p2_poisoned))) {
            next_state := s_valid_1
            when (p1) {
                next_uop.uopc := uopSTD
                next_uop.lrs1_rtype := RT_X
            } .otherwise {
                next_uop.lrs2_rtype := RT_X
            }
        }
    }

    when (io.inUop.valid) {
        p1 := !io.inUop.bits.prs1_busy
        p2 := !io.inUop.bits.prs2_busy
    }
    p1_poisoned := false.B
    p2_poisoned := false.B

    val in_uop = Mux(io.inUop.valid, io.inUop.bits, slot_uop)
    val in_uop_p1_poisoned = Mux(
        io.inUop.valid, io.inUop.bits.iw_p1_poisoned, p1_poisoned
    )
    val in_uop_p2_poisoned = Mux(
        io.inUop.valid, io.inUop.bits.iw_p2_poisoned, p2_poisoned
    )
    when (io.ldSpecMiss && in_uop_p1_poisoned) { p1 := false.B }
    when (io.ldSpecMiss && in_uop_p2_poisoned) { p2 := false.B }

    val prs1_matches = io.wakeupPorts.map {
        w => w.bits.pdst === in_uop.prs1
    }
    val prs2_matches = io.wakeupPorts.map {
        w => w.bits.pdst === in_uop.prs2
    }
    val prs1_specmatchs = io.specLdWakeupPorts.map {
        w => w.bits === in_uop.prs1 && in_uop.lrs1_rtype === RT_FIX
    }
    val prs2_specmatchs = io.specLdWakeupPorts.map {
        w => w.bits === in_uop.prs2 && in_uop.lrs2_rtype === RT_FIX
    }
    val prs1_wakeups = (io.wakeupPorts zip prs1_matches).map {
        case (w, m) => w.valid && m
    }
    val prs2_wakeups = (io.wakeupPorts zip prs2_matches).map {
        case (w, m) => w.valid && m
    }
    val prs1_specwakeups = (io.specLdWakeupPorts zip prs1_specmatchs).map {
        case (w, m) => w.valid && m
    }
    val prs2_specwakeups = (io.specLdWakeupPorts zip prs2_specmatchs).map {
        case (w, m) => w.valid && m
    }
    val prs1_normalwakeup = prs1_wakeups.reduce(_||_)
    val prs2_normalwakeup = prs2_wakeups.reduce(_||_)
    val prs1_specwakeup   = prs1_specwakeups.reduce(_||_)
    val prs2_specwakeup   = prs2_specwakeups.reduce(_||_)
    when (prs1_normalwakeup || prs1_specwakeup) { p1 := true.B }
    when (prs2_normalwakeup || prs2_specwakeup) { p2 := true.B }
    when (prs1_specwakeup) { p1_poisoned := true.B }
    when (prs2_specwakeup) { p2_poisoned := true.B }

    when (state === s_valid_1) {
        io.request := p1 && p2 && !io.kill
    } .elsewhen (state === s_valid_2) {
        io.request := (p1 || p2) && !io.kill
    } .otherwise {
        io.request := false.B
    }

    io.uop := slot_uop
    io.uop.iw_p1_poisoned := p1_poisoned
    io.uop.iw_p2_poisoned := p2_poisoned
    when (state === s_valid_2) {
        when (p1) {
            io.uop.lrs2_rtype := RT_X
        } .elsewhen (p2) {
            io.uop.uopc := uopSTD
            io.uop.lrs1_rtype := RT_X
        }
    }

    val squash_grant = io.ldSpecMiss && (p1_poisoned || p2_poisoned)
    io.willBeValid := isValid(state) && !(io.grant && (state === s_valid_1) && !squash_grant)
}
