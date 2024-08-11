package iFu.backend

import chisel3._
import chisel3.util._

import iFu.common._
import iFu.common.Consts._

trait IssueState {
    // s_invalid -> no valid instruction
    // s_valid_1 -> normal valid instruction
    // s_valid_2 -> store-like instruction
    val s_invalid :: s_valid_1 :: s_valid_2 :: Nil = Enum(3)

    def isValid(s: UInt): Bool = s =/= s_invalid
}

class IssueWakeup(val pregSz: Int) extends Bundle {
    val pdst = UInt(pregSz.W)   // physical destination register
}

class IssueUnitIO (
    val dispatchWidth: Int,
    val numWakeupPorts: Int,
    val issueWidth: Int
) extends CoreBundle {
    val disUops = Vec(dispatchWidth, Flipped(Decoupled(new MicroOp)))

    // normal wakeup
    val wakeupPorts = Vec(numWakeupPorts, Flipped(Valid(new IssueWakeup(pregSz))))
    // speculative load wakeup
    val specLdWakeupPorts = Vec(memWidth, Flipped(Valid(UInt(pregSz.W))))
    val ldMiss = Input(Bool())  // load miss happened

    val fuTypes = Input(Vec(issueWidth, Bits(FUC_SZ.W)))

    val brUpdate = Input(new BrUpdateInfo)
    val flushPipeline = Input(Bool())

    val issueValids = Output(Vec(issueWidth, Bool()))
    val issueUops = Output(Vec(issueWidth, new MicroOp))
}

class IssueUnit (
    issParams: IssueParams,
    numWakeupPorts: Int
) extends CoreModule with IssueState {
    val numIssueSlots = issParams.numIssueSlots
    val dispatchWidth = issParams.dispatchWidth
    val issueWidth = issParams.issueWidth

    val io = IO(new IssueUnitIO(dispatchWidth, numWakeupPorts, issueWidth))

    val disUops = Wire(Vec(dispatchWidth, new MicroOp))

    for (w <- 0 until dispatchWidth) {
        disUops(w) := io.disUops(w).bits
        disUops(w).iwState := s_valid_1
        disUops(w).iw_p1_poisoned := false.B
        disUops(w).iw_p2_poisoned := false.B

        when (
            (io.disUops(w).bits.uopc === uopSTA) ||
            (io.disUops(w).bits.uopc === uopSC_AG)
        ) {
            disUops(w).iwState := s_valid_2
        }
    }

    val slots = Seq.fill(numIssueSlots) { Module(new IssueSlot(numWakeupPorts)) }
    val issueSlots = VecInit(slots.map(_.io))

    issueSlots.foreach { slot =>
        slot.wakeupPorts := io.wakeupPorts
        slot.specLdWakeupPorts := io.specLdWakeupPorts
        slot.ldSpecMiss := io.ldMiss
        slot.brUpdate := io.brUpdate
        slot.kill := io.flushPipeline
    }

    val maxShift = dispatchWidth
    val vacants = issueSlots.map(_.valid).map(!_.asBool) ++ io.disUops.map(_.valid).map(!_.asBool)

    def getShamtOH(countOH: UInt, inc: Bool): UInt = {
        val next = Wire(UInt(maxShift.W))
        next := countOH
        when(countOH === 0.U && inc) {
            next := 1.U
        }.elsewhen(!countOH(maxShift - 1) && inc) {
            next := (countOH << 1.U)
        }
        next
    }

    val shamtOH = vacants.scanLeft(0.U)(getShamtOH)

    val willBeValid = issueSlots.map(_.willBeValid) ++
        io.disUops.map(dis => dis.valid && !dis.bits.xcpt_valid && !dis.bits.is_ibar && !dis.bits.is_nop)

    val uops = issueSlots.map(_.outUop) ++ disUops.map(uop => uop)

    for (i <- 0 until numIssueSlots) {
        issueSlots(i).inUop.valid := false.B
        issueSlots(i).inUop.bits := uops(i + 1)

        for (j <- 1 to maxShift) {
            when (shamtOH(i + j) === (1 << (j - 1)).U) {
                issueSlots(i).inUop.valid := willBeValid(i + j)
                issueSlots(i).inUop.bits := uops(i + j)
            }
        }
        issueSlots(i).clear := shamtOH(i) =/= 0.U
    }

    val willBeAvailable = issueSlots.map{ i => (!i.willBeValid || i.clear) && !i.inUop.valid }
    val numAvailable = PopCount(willBeAvailable)
    io.disUops.zipWithIndex.foreach { case (uop, idx) => uop.ready := RegNext(numAvailable > idx.U) }

    for (w <- 0 until issueWidth) {
        io.issueValids(w) := false.B
        io.issueUops(w) := NullMicroOp
        io.issueUops(w).prs1 := 0.U
        io.issueUops(w).prs2 := 0.U
        io.issueUops(w).lrs1_rtype := RT_X
        io.issueUops(w).lrs2_rtype := RT_X
    }

    // io.issueValids.foreach {
    //     _ := false.B
    // }
    // io.issueUops := DontCare

    // choose which uops to issue
    val requests = issueSlots.map(_.request)    // get request from each slot
    val portIssued = Array.fill(issueWidth) { false.B }

    val iss_mask = Seq(
        Seq(true, true,  true,  true,  true,  true,  true, false,  true, false),
        Seq(true, true,  true,  true,  true,  true, false,  true, false,  true),
        Seq(true, true,  true, false, false, false, false, false,  true,  true),
    )

    for (i <- 0 until numIssueSlots) {  // iterate through all slots
        issueSlots(i).grant := false.B
        var uopIssued = false.B

        for (w <- 0 until issueWidth) {
            val canAllocate = if (issParams.iqType == IQT_INT.litValue.toInt) {
                iss_mask(w)(i).B && (issueSlots(i).uop.fuCode & io.fuTypes(w)) =/= 0.U
            } else {
                true.B
            }
            when (canAllocate && requests(i) && !uopIssued && !portIssued(w)) {
                issueSlots(i).grant := true.B
                io.issueValids(w) := true.B
                io.issueUops(w) := issueSlots(i).uop
            }
            val wasPortIssuedYet = portIssued(w)
            portIssued(w) = (canAllocate && requests(i) && !uopIssued) | portIssued(w)
            uopIssued = (canAllocate && requests(i) && !wasPortIssuedYet) | uopIssued
        }
    }
}
