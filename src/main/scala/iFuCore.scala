package iFu

import chisel3._
import chisel3.util._
import iFu.frontend._

import iFu.backend._
import iFu.tlb._
import iFu.sma._
import iFu.axi3._
import iFu.common._
import iFu.common.Consts._
import iFu.util._
import iFu.difftest._

class iFuCore extends CoreModule {
    val io = IO(new CoreBundle {
        val ext_int = Input(UInt(8.W))
        val axi3    = new AXI3
    })
/*-----------------------------*/

    val fetchWidth      = frontendParams.fetchWidth
    val memIssueParam   = issueParams.filter(_.iqType == IQT_MEM.litValue).head
    val intIssueParam   = issueParams.filter(_.iqType == IQT_INT.litValue).head
    val numFTQEntries   = frontendParams.numFTQEntries
    val fetchBytes      = frontendParams.fetchBytes

/*-----------------------------*/

    val ifu = Module(new Frontend)

    val dispatcher = Module(new BasicDispatcher)

    val exe_units = new ExecutionUnits
    val jmp_unit_idx = exe_units.jmp_unit_idx
    val jmp_unit = exe_units(jmp_unit_idx)
    val mem_units = exe_units.memory_units

    val numWritePorts = exe_units.numWritePorts + memWidth
    val numFastWakeupPorts = exe_units.count(_.bypassable)
    val numAlwaysBypassable = exe_units.count(_.alwaysBypassable)
    val numIssueWakeupPorts = numWritePorts + numFastWakeupPorts - numAlwaysBypassable
    val numRenameWakeupPorts = numIssueWakeupPorts

    val rename_stage = Module(new RenameStage(numRenameWakeupPorts))

    val mem_iss_unit = Module(new IssueUnit(memIssueParam, numIssueWakeupPorts))
    val int_iss_unit = Module(new IssueUnit(intIssueParam, numIssueWakeupPorts))
    val issue_units = Seq(mem_iss_unit, int_iss_unit)
    require(exe_units.length == issue_units.map(_.issueWidth).sum)

    val iregfile = Module(new RegisterFileSynthesizable(
        numPRegs,
        exe_units.map(_.numReadPorts).sum,
        numWritePorts,
        xLen,
        Seq.fill(memWidth) { true } ++ exe_units.bypassable_write_port_mask
    ))
    val iregister_read = Module(new RegisterRead(
        issue_units.map(_.issueWidth).sum,
        exe_units.map(_.supportedFuncUnits).toSeq,
        exe_units.map(_.numReadPorts).toSeq,
        exe_units.numTotalBypassPorts,
        xLen
    ))

    val lsu         = Module(new Lsu)
    val rob         = Module(new Rob(numWritePorts))
    val csr         = Module(new CSRFile)
    val tlb_data    = Module(new TLBDataManager)

    tlb_data.io.csr      <> csr.io.tlb_data
    tlb_data.io.r_req(0) := lsu.io.core.tlb_data.r_req(0)
    tlb_data.io.r_req(1) := lsu.io.core.tlb_data.r_req(1)
    tlb_data.io.r_req(2) := ifu.io.core.tlb_data.r_req

    lsu.io.core.tlb_data.r_resp(0) := tlb_data.io.r_resp(0)
    lsu.io.core.tlb_data.r_resp(1) := tlb_data.io.r_resp(1)
    ifu.io.core.tlb_data.r_resp    := tlb_data.io.r_resp(2)
    lsu.io.csr                     := csr.io.lsu_csr_ctx
    ifu.io.core.csr.itlb_csr_cxt   := csr.io.itlb_csr_ctx

/*-----------------------------*/

    val int_iss_wakeups = Wire(Vec(numIssueWakeupPorts, Valid(new ExeUnitResp)))
    val int_ren_wakeups = Wire(Vec(numRenameWakeupPorts, Valid(new ExeUnitResp)))

/*-----------------------------*/

    val sma_arb = Module(new SMA_Arbiter(3, 2))
    sma_arb.io.smar(0) <> lsu.io.smar(0)
    sma_arb.io.smar(1) <> lsu.io.smar(1)
    sma_arb.io.smar(2) <> ifu.io.smar

    sma_arb.io.smaw(0) <> lsu.io.smaw(0)
    sma_arb.io.smaw(1) <> lsu.io.smaw(1)

    io.axi3 <> sma_arb.io.axi3

/*-----------------------------*/

    // Rename1
    val ren_stalls = Wire(Vec(coreWidth, Bool()))

    // Rename2/Dispatch
    val dis_valids = Wire(Vec(coreWidth, Bool()))
    val dis_uops   = Wire(Vec(coreWidth, new MicroOp))
    val dis_fire   = Wire(Vec(coreWidth, Bool()))
    val dis_ready  = Wire(Bool())

    // Issue/Register Read
    val iss_valids = Wire(Vec(exe_units.length, Bool()))
    val iss_uops   = Wire(Vec(exe_units.length, new MicroOp))
    val bypasses   = Wire(Vec(exe_units.numTotalBypassPorts, Valid(new ExeUnitResp)))
    require(jmp_unit.bypassable)

    // --------------------------------------
    val ftq_arb = Seq.fill(2) { Module(new Arbiter(UInt(log2Ceil(numFTQEntries).W), 2)) }
    ftq_arb map { _.io.out.ready := true.B }

    val flush_pc_req = Wire(Decoupled(UInt(log2Ceil(numFTQEntries).W)))
    val jmp_pc_req   = Wire(Decoupled(UInt(log2Ceil(numFTQEntries).W)))
    ftq_arb(0).io.in(0) <> flush_pc_req
    ftq_arb(0).io.in(1) <> jmp_pc_req
    ifu.io.core.getFtqPc(0).ftqIdx := ftq_arb(0).io.out.bits

    val mispred_req  = Wire(Decoupled(UInt(log2Ceil(numFTQEntries).W)))
    val xcpt_pc_req  = Wire(Decoupled(UInt(log2Ceil(numFTQEntries).W)))
    ftq_arb(1).io.in(0) <> mispred_req
    ftq_arb(1).io.in(1) <> xcpt_pc_req
    ifu.io.core.getFtqPc(1).ftqIdx := ftq_arb(1).io.out.bits

    // --------------------------------------
    val bruTypes = Seq(
        BRUType(normal =  true, to_exu = false, to_lsu = false),
        BRUType(normal = false, to_exu =  true, to_lsu = false),
        BRUType(normal = false, to_exu = false, to_lsu =  true)
    )
    val brus = bruTypes map { t => Module(new BranchUnit(t, exe_units.alu_units.length)) }
    brus zip bruTypes foreach { case (bru, t) =>
        bru.io.br_infos zip exe_units.alu_units foreach {
            case (b, e) => b := e.io.brinfo
        }
        bru.io.rob_flush := rob.io.flush.valid
        if (t.normal || t.to_lsu) {
            bru.io.rob_head  := rob.io.rob_head_idx
        }
        if (t.normal) {
            bru.io.jalr_tgt  := jmp_unit.io.brinfo.jalrTarget
        }
    }

    val b1_mispredict_val = brus(bruTypes.indexWhere(_.normal)).io.br_s1_mispredict
    mispred_req.valid := b1_mispredict_val
    mispred_req.bits  := brus(bruTypes.indexWhere(_.normal)).io.mis_br_ftqIdx
    val brUpdate = brus(bruTypes.indexWhere(_.normal)).io.br_update

    ifu.io.core.brupdate := brUpdate
    for (exu <- exe_units) {
        exu.io.brupdate := brus(bruTypes.indexWhere(_.to_exu)).io.br_update
    }

    val mem_resps = mem_units.map(_.io.iresp)
    for (i <- 0 until memWidth) {
        mem_units(i).io.lsu_io <> lsu.io.core.exe(i)
    }

    //-------------------------------------------------------------
    //-------------------------------------------------------------
    // **** Fetch Stage ****
    //-------------------------------------------------------------
    //-------------------------------------------------------------

    ifu.io.core.redirect_val := false.B
    ifu.io.core.redirect_flush := false.B
    ifu.io.core.flush_icache := (0 until coreWidth).map { i =>
        rob.io.commit.arch_valids(i) && rob.io.commit.uops(i).is_ibar
    }.reduce(_||_)

    when (RegNext(rob.io.flush.valid)) {
        ifu.io.core.redirect_val := true.B
        ifu.io.core.redirect_flush := true.B
        val flush_type = RegNext(rob.io.flush.bits.flush_typ)
        // Reset ras when we flush the ROB (exceptions, AMOs, unique instructions, etc.)
        val new_ras_ptr = WireInit(0.U.asTypeOf(new RASPtr))
        new_ras_ptr.bits := ifu.io.core.getFtqPc(0).entry.rasIdx
        ifu.io.core.redirect_ras_ptr := new_ras_ptr
        when (FlushTypes.useCsrEvec(flush_type)) {
            ifu.io.core.redirect_pc := csr.io.redirect_pc
        } .otherwise {
            val flush_pc = AlignPCToBoundary(ifu.io.core.getFtqPc(0).pc, fetchBytes) | RegNext(rob.io.flush.bits.pc_lob)
            val flush_pc_next = flush_pc + coreInstrBytes.U
            ifu.io.core.redirect_pc := Mux(
                FlushTypes.useSamePC(flush_type),
                flush_pc, flush_pc_next
            )
        }
        ifu.io.core.redirect_ftq_idx := RegNext(rob.io.flush.bits.ftq_idx)
    } .elsewhen(brUpdate.b2.mispredict) {
        val block_pc = AlignPCToBoundary(ifu.io.core.getFtqPc(1).pc, fetchBytes)
        val uop_pc = block_pc | brUpdate.b2.uop.pcLowBits

        val npc = uop_pc + coreInstrBytes.U
        val jal_br_target = (uop_pc.asSInt + brUpdate.b2.targetOffset).asUInt

        val bj_addr = Mux(brUpdate.b2.cfiType === CFI_JIRL,
            brUpdate.b2.jalrTarget, jal_br_target
        )

        val mispredict_target = Mux(brUpdate.b2.pcSel === PC_PLUS4, npc, bj_addr)

        ifu.io.core.redirect_val := true.B
        ifu.io.core.redirect_flush := true.B
        ifu.io.core.redirect_pc := mispredict_target
        ifu.io.core.redirect_ftq_idx := brUpdate.b2.uop.ftqIdx

        val ftq_entry = ifu.io.core.getFtqPc(1).entry
        val cfi_idx = brUpdate.b2.uop.pcLowBits(log2Ceil(fetchWidth) + 1, 2)
        val ftq_ras_ptr = ifu.io.core.getFtqPc(1).rasPtr
        val next_ras_ptr = ftq_ras_ptr.update(
            true.B,
            ftq_entry.cfiIsCall && ftq_entry.cfiIdx.bits === cfi_idx,
            ftq_entry.cfiIsRet && ftq_entry.cfiIdx.bits === cfi_idx
        )

        ifu.io.core.redirect_ras_ptr := next_ras_ptr
    } .elsewhen (rob.io.flush_frontend || b1_mispredict_val) {
        ifu.io.core.redirect_flush   := true.B
        ifu.io.core.redirect_pc      := DontCare
        ifu.io.core.redirect_ras_ptr := DontCare
        ifu.io.core.redirect_ftq_idx := DontCare
    } .otherwise {
        ifu.io.core.redirect_pc      := DontCare
        ifu.io.core.redirect_ras_ptr := DontCare
        ifu.io.core.redirect_ftq_idx := DontCare
    }

    val youngest_com_idx = (coreWidth - 1).U - PriorityEncoder(rob.io.commit.valids.reverse)
    ifu.io.core.commit.valid := rob.io.commit.valids.reduce(_|_) || rob.io.com_xcpt.valid
    ifu.io.core.commit.bits  := Mux(
        rob.io.com_xcpt.valid,
        rob.io.com_xcpt.bits.ftq_idx,
        rob.io.commit.uops(youngest_com_idx).ftqIdx
    )

    flush_pc_req.valid := rob.io.flush.valid
    flush_pc_req.bits := rob.io.flush.bits.ftq_idx

    jmp_pc_req.valid := RegNext(iss_valids(jmp_unit_idx) && iss_uops(jmp_unit_idx).fuCode === FU_JMP)
    jmp_pc_req.bits := RegNext(iss_uops(jmp_unit_idx).ftqIdx)

    jmp_unit.io.getFtqPc := DontCare
    jmp_unit.io.getFtqPc.pc      := ifu.io.core.getFtqPc(0).pc
    jmp_unit.io.getFtqPc.entry   := ifu.io.core.getFtqPc(0).entry
    jmp_unit.io.getFtqPc.nextVal := ifu.io.core.getFtqPc(0).nextVal
    jmp_unit.io.getFtqPc.nextpc  := ifu.io.core.getFtqPc(0).nextpc

    val reg_xcpt_fetch_pc = RegEnable(ifu.io.core.getFtqPc(1).pc, 0.U, RegNext(xcpt_pc_req.fire))
    dontTouch(reg_xcpt_fetch_pc)
    rob.io.xcpt_fetch_pc := Mux(RegNext(xcpt_pc_req.fire), ifu.io.core.getFtqPc(1).pc, reg_xcpt_fetch_pc)

    //-------------------------------------------------------------
    //-------------------------------------------------------------
    // **** Decode ****
    //-------------------------------------------------------------
    //-------------------------------------------------------------

    val dec_stage = Module(new DecodeStage)
    dec_stage.io.enq               <> ifu.io.core.fetchPacket
    dec_stage.io.intrpt            := csr.io.interrupt
    dec_stage.io.flush             := RegNext(rob.io.flush.valid)
    dec_stage.io.clear             := ifu.io.core.redirect_flush
    dec_stage.io.rollback          := rob.io.commit.rollback
    dec_stage.io.dis_ready         := dis_ready
    dec_stage.io.b1_mispred        := b1_mispredict_val
    dec_stage.io.br_update         := brUpdate
    xcpt_pc_req.valid              := dec_stage.io.xcpt_ftqIdx.valid
    xcpt_pc_req.bits               := dec_stage.io.xcpt_ftqIdx.bits
    dec_stage.io.xcpt_ftqIdx.ready := xcpt_pc_req.ready

    //-------------------------------------------------------------
    //-------------------------------------------------------------
    // **** Register Rename ****
    //-------------------------------------------------------------
    //-------------------------------------------------------------

    // Inputs
    rename_stage.io.kill       := ifu.io.core.redirect_flush
    rename_stage.io.brupdate   := brUpdate
    rename_stage.io.dec_fire   := dec_stage.io.deq.map(_.valid)
    rename_stage.io.dec_uops   := dec_stage.io.deq.map(_.bits)
    rename_stage.io.dis_fire   := dis_fire
    rename_stage.io.dis_ready  := dis_ready
    rename_stage.io.com_valids := rob.io.commit.valids
    rename_stage.io.com_uops   := rob.io.commit.uops
    rename_stage.io.rbk_valids := rob.io.commit.rbk_valids
    rename_stage.io.rollback   := rob.io.commit.rollback

    dis_valids := rename_stage.io.ren2_mask
    dis_uops   := rename_stage.io.ren2_uops
    ren_stalls := rename_stage.io.ren_stalls

    //-------------------------------------------------------------
    //-------------------------------------------------------------
    // **** Dispatch ****
    //-------------------------------------------------------------
    //-------------------------------------------------------------

    //-------------------------------------------------------------
    // Rename2/Dispatch pipeline logic

    // 不包括自己，前面有没有有效/unique指令
    val dis_prior_slot_valid  = dis_valids.scanLeft(false.B)((s, v) => s || v)
    val dis_prior_slot_unique = (dis_uops zip dis_valids).scanLeft(false.B) { case (s, (u, v)) => s || v && u.is_unique }

    // wait_for_empty_pipeline(i) means that the ith instruction needs to wait
    //  if only unique instructions:
    //      need (rob is empty) and (store queue is empty) and (no valid instruction before)
    //  if the instruction is a ibar:
    //      need (rob is empty) and (fence is ready) and (no valid instruction before)
    val wait_for_empty_pipeline = (0 until coreWidth).map { w =>
        (dis_uops(w).is_unique) &&
        (
            // for better timing , now we need rob.io.enq_valids
            RegNext(!rob.io.empty || rob.io.enq_valids.asUInt.orR) ||
            // !(lsu.io.core.stq_empty && !(dis_uops(w).is_ibar && !lsu.io.core.dcache_ord)) ||
            (!lsu.io.core.stq_empty || (dis_uops(w).is_ibar && !lsu.io.core.dcache_ord)) ||
            dis_prior_slot_valid(w)
        )
    }

    val dis_hazards = (0 until coreWidth).map { w =>
        dis_valids(w) && (
            !rob.io.ready                                  ||
            ren_stalls(w)                                  ||
            lsu.io.core.ldq_full(w) && dis_uops(w).use_ldq ||
            lsu.io.core.stq_full(w) && dis_uops(w).use_stq ||
            !dispatcher.io.ren_uops(w).ready               ||
            wait_for_empty_pipeline(w)                     ||
            dis_prior_slot_unique(w)                       ||
            b1_mispredict_val                              ||
            brUpdate.b2.mispredict                         ||
            ifu.io.core.redirect_flush
        )
    }

    /* lsu.io.core.fence_dmem := (dis_valids zip dis_uops zip wait_for_empty_pipeline).map {
        case ((v, u), w) => v && u.is_ibar && w
    }.reduce(_||_) */
    lsu.io.core.fence_dmem := (dis_valids zip dis_uops).map {
        case (v, u) => v && u.is_ibar  // flush dcache when we dispatch an ibar
    }.reduce(_||_)

    val dis_stalls = dis_hazards.scanLeft(false.B)((s, h) => s || h).takeRight(coreWidth)
    dis_fire := dis_valids zip dis_stalls map { case (v, s) => v && !s }
    dis_ready := !dis_stalls.last

    //-------------------------------------------------------------
    // LDQ/STQ Allocation Logic

    for (w <- 0 until coreWidth) {
        lsu.io.core.dis_uops(w).valid := dis_fire(w)
        lsu.io.core.dis_uops(w).bits  := dis_uops(w)
    }

    for (w <- 0 until coreWidth) {
        dis_uops(w).ldqIdx := lsu.io.core.dis_ldq_idx(w)
        dis_uops(w).stqIdx := lsu.io.core.dis_stq_idx(w)
    }

    //-------------------------------------------------------------
    // Rob Allocation Logic

    rob.io.enq_valids        := dis_fire
    rob.io.enq_uops          := dis_uops
    rob.io.enq_partial_stall := dis_stalls.last
    rob.io.idle              := csr.io.idle

    for (w <- 0 until coreWidth) {
        dis_uops(w).robIdx := Cat(
            rob.io.rob_tail_idx >> log2Ceil(coreWidth).U,
            w.U(log2Ceil(coreWidth).W)
        )
    }

    //-------------------------------------------------------------
    // Dispatch to issue queues

    for (w <- 0 until coreWidth) {
        dispatcher.io.ren_uops(w).valid := dis_fire(w)
        dispatcher.io.ren_uops(w).bits  := dis_uops(w)
    }

    var iu_idx = 0
    for (i <- 0 until issueParams.size) {
        issue_units(iu_idx).io.disUops <> dispatcher.io.dis_uops(i)
        iu_idx += 1
    }

    //-------------------------------------------------------------
    //-------------------------------------------------------------
    // **** Issue Stage ****
    //-------------------------------------------------------------
    //-------------------------------------------------------------

    require(issue_units.map(_.issueWidth).sum == exe_units.length)

    var iss_wu_idx = 0
    var ren_wu_idx = 0
    for (i <- 0 until memWidth) {
        int_iss_wakeups(i).valid := mem_resps(i).valid && mem_resps(i).bits.uop.dst_rtype === RT_FIX
        int_iss_wakeups(i).bits  := mem_resps(i).bits
        int_ren_wakeups(i).valid := mem_resps(i).valid && mem_resps(i).bits.uop.dst_rtype === RT_FIX
        int_ren_wakeups(i).bits  := mem_resps(i).bits
        iss_wu_idx += 1
        ren_wu_idx += 1
    }
    for (i <- 0 until exe_units.length) {
        if (exe_units(i).writesIrf) {
            val fast_wakeup = Wire(Valid(new ExeUnitResp(xLen)))
            val slow_wakeup = Wire(Valid(new ExeUnitResp(xLen)))
            fast_wakeup := DontCare
            slow_wakeup := DontCare

            val resp = exe_units(i).io.iresp
            assert(!(resp.valid && resp.bits.uop.rf_wen && resp.bits.uop.dst_rtype =/= RT_FIX))

            fast_wakeup.valid := iss_valids(i) && iss_uops(i).bypassable &&
                iss_uops(i).dst_rtype === RT_FIX && iss_uops(i).ldst_val &&
                !(lsu.io.core.ld_miss && (iss_uops(i).iw_p1_poisoned || iss_uops(i).iw_p2_poisoned))
            fast_wakeup.bits.uop := iss_uops(i)

            slow_wakeup.valid := resp.valid && resp.bits.uop.rf_wen &&
                !resp.bits.uop.bypassable && resp.bits.uop.dst_rtype === RT_FIX
            slow_wakeup.bits.uop := resp.bits.uop

            if (exe_units(i).bypassable) {  // has alu
                int_iss_wakeups(iss_wu_idx) := fast_wakeup
                iss_wu_idx += 1
                int_ren_wakeups(ren_wu_idx) := fast_wakeup
                ren_wu_idx += 1
            }
            if (!exe_units(i).alwaysBypassable) {   // has alu and other
                int_iss_wakeups(iss_wu_idx) := slow_wakeup
                iss_wu_idx += 1
                int_ren_wakeups(ren_wu_idx) := slow_wakeup
                ren_wu_idx += 1
            }
        }
    }
    require(iss_wu_idx == numIssueWakeupPorts)
    require(ren_wu_idx == numRenameWakeupPorts)

    // Perform load-hit speculative wakeup through a special port (performs a poison wake-up).
    issue_units map { iu =>
        iu.io.specLdWakeupPorts := lsu.io.core.spec_ld_wakeup
    }

    // ----------------------------------------------------------------
    
    for ((renport, intport) <- rename_stage.io.wakeups zip int_ren_wakeups) {
        renport <> intport
    }

    var iss_idx = 0
    var int_iss_cnt = 0
    var mem_iss_cnt = 0
    for (w <- 0 until exe_units.length) {
        var fu_types = exe_units(w).io.fu_types
        val exe_unit = exe_units(w)
        if (exe_unit.supportedFuncUnits.muldiv) {
            // Supress just-issued divides from issuing back-to-back, since it's an iterative divider.
            // But it takes a cycle to get to the Exe stage, so it can't tell us it is busy yet.
            val idiv_issued = iss_valids(iss_idx) && iss_uops(iss_idx).fu_code_is(FU_DIV)
            fu_types = fu_types & RegNext(~Mux(idiv_issued, FU_DIV, 0.U)).asUInt
        }
        if (exe_unit.hasMem) {
            mem_iss_unit.io.fuTypes(mem_iss_cnt) := fu_types
            iss_valids(iss_idx) := mem_iss_unit.io.issueValids(mem_iss_cnt)
            iss_uops(iss_idx)   := mem_iss_unit.io.issueUops(mem_iss_cnt)
            mem_iss_cnt += 1
        } else {
            int_iss_unit.io.fuTypes(int_iss_cnt) := fu_types
            iss_valids(iss_idx) := int_iss_unit.io.issueValids(int_iss_cnt)
            iss_uops(iss_idx)   := int_iss_unit.io.issueUops(int_iss_cnt)
            int_iss_cnt += 1
        }
        iss_idx += 1
    }

    issue_units.foreach(_.io.brUpdate := brUpdate)
    issue_units.foreach(_.io.flushPipeline := RegNext(rob.io.flush.valid))
    issue_units.foreach(_.io.ldMiss := lsu.io.core.ld_miss)

    // Wakeup (Issue & Writeback)
    for (iu <- issue_units) {
        for ((issPort, wakeup) <- iu.io.wakeupPorts zip int_iss_wakeups) {
            issPort.valid := wakeup.valid
            issPort.bits.pdst := wakeup.bits.uop.pdst
        }
        require(iu.io.wakeupPorts.length == int_iss_wakeups.length)
    }

    //-------------------------------------------------------------
    //-------------------------------------------------------------
    // **** Register Read Stage ****
    //-------------------------------------------------------------
    //-------------------------------------------------------------

    iregister_read.io.rf_read_ports <> iregfile.io.read_ports

    for (w <- 0 until exe_units.length) {
        iregister_read.io.iss_valids(w) := (
            iss_valids(w) &&
            !(lsu.io.core.ld_miss && (iss_uops(w).iw_p1_poisoned || iss_uops(w).iw_p2_poisoned))
        )
    }

    iregister_read.io.iss_uops := iss_uops
    iregister_read.io.iss_uops foreach { u =>
        u.iw_p1_poisoned := false.B
        u.iw_p2_poisoned := false.B
    }

    iregister_read.io.brupdate := brus(bruTypes.indexWhere(_.to_exu)).io.br_update
    iregister_read.io.kill := RegNext(rob.io.flush.valid)

    iregister_read.io.bypass := bypasses

    //-------------------------------------------------------------
    //--------------------------CSR--------------------------------
    //-------------------------------------------------------------
    val csr_exe_unit = exe_units.csr_unit
    val csr_rw_cmd = csr_exe_unit.io.iresp.bits.csr_cmd

    csr.io.ext_int := io.ext_int
    csr.io.addr    := csr_exe_unit.io.iresp.bits.csr_addr
    csr.io.tlb_op  := csr_exe_unit.io.iresp.bits.tlb_op
    csr.io.r1      := csr_exe_unit.io.iresp.bits.csr_r1
    csr.io.r2      := csr_exe_unit.io.iresp.bits.csr_r2

    csr.io.cmd := csr_rw_cmd
    csr.io.exevalid  := csr_exe_unit.io.iresp.valid
    csr.io.exception := RegNext(rob.io.com_xcpt.valid)
    csr.io.com_xcpt  := RegNext(rob.io.com_xcpt)

    csr.io.err_pc := AlignPCToBoundary(ifu.io.core.getFtqPc(0).compc, fetchBytes) | RegNext(rob.io.com_xcpt.bits.pc_lob)

    csr.io.is_ll := (
        (mem_resps(0).valid && mem_resps(0).bits.uop.is_ll) ||
        (mem_resps(1).valid && mem_resps(1).bits.uop.is_ll)
    )
    csr.io.is_sc := mem_resps(0).valid && mem_resps(0).bits.uop.is_sc

    //-------------------------------------------------------------
    //-------------------------------------------------------------
    // **** Execute Stage ****
    //-------------------------------------------------------------
    //-------------------------------------------------------------

    iss_idx = 0
    var bypass_idx = 0
    for (w <- 0 until exe_units.length) {
        val exe_unit = exe_units(w)
        exe_unit.io.req <> iregister_read.io.exe_reqs(iss_idx)
        if (exe_unit.bypassable) {
            for (i <- 0 until exe_unit.numStages) {
                bypasses(bypass_idx) := exe_unit.io.bypass(i)
                bypass_idx += 1
            }
        }
        iss_idx += 1
    }
    require(bypass_idx == exe_units.numTotalBypassPorts)

    //-------------------------------------------------------------
    //-------------------------------------------------------------
    // **** Load/Store Unit ****
    //-------------------------------------------------------------
    //-------------------------------------------------------------

    // tell LSU about committing loads and stores to clear entries
    lsu.io.core.commit := rob.io.commit

    // tell LSU that it should fire a load that waits for the rob to clear
    lsu.io.core.commit_load_at_rob_head := rob.io.com_load_is_at_rob_head

    lsu.io.core.exception    := RegNext(rob.io.flush.valid)
    lsu.io.core.brupdate     := brus(bruTypes.indexWhere(_.to_lsu)).io.br_update
    lsu.io.core.rob_head_idx := rob.io.rob_head_idx

    //-------------------------------------------------------------
    //-------------------------------------------------------------
    // **** Writeback Stage ****
    //-------------------------------------------------------------
    //-------------------------------------------------------------

    var w_cnt = 0
    for (i <- 0 until memWidth) {
        iregfile.io.write_ports(w_cnt) := WritePort(mem_resps(i), pregSz, xLen, RT_FIX)
        w_cnt += 1
    }
    for (i <- 0 until exe_units.length) {
        if (exe_units(i).writesIrf) {
            val wbresp = exe_units(i).io.iresp
            val wbpdst = wbresp.bits.uop.pdst
            val wbdata = wbresp.bits.data

            def wbIsValid(rtype: UInt) =
                wbresp.valid && wbresp.bits.uop.rf_wen && wbresp.bits.uop.dst_rtype === rtype

            val wbReadsCSR = wbresp.bits.uop.ctrl.csr_cmd =/= CSR_N

            iregfile.io.write_ports(w_cnt).valid := wbIsValid(RT_FIX)
            iregfile.io.write_ports(w_cnt).bits.addr := wbpdst
            wbresp.ready := true.B
            if (exe_units(i).hasCSR) {
                iregfile.io.write_ports(w_cnt).bits.data := Mux(wbReadsCSR, csr.io.rdata, wbdata)
            } else {
                iregfile.io.write_ports(w_cnt).bits.data := wbdata
            }

            assert(!(wbresp.valid && !wbresp.bits.uop.rf_wen && wbresp.bits.uop.dst_rtype === RT_FIX),
                "[fppipeline] An Int writeback is being attempted with rf_wen disabled.")

            assert(!(wbresp.valid && wbresp.bits.uop.rf_wen && wbresp.bits.uop.dst_rtype =/= RT_FIX),
                "[fppipeline] writeback being attempted to Int RF with dst != Int type exe_units(" + i + ").iresp")

            w_cnt += 1
        }
    }
    require(w_cnt == iregfile.io.write_ports.length)

    //-------------------------------------------------------------
    //-------------------------------------------------------------
    // **** Commit Stage ****
    //-------------------------------------------------------------
    //-------------------------------------------------------------

    // Writeback
    // ---------
    var cnt = 0
    for (i <- 0 until memWidth) {
        val mem_uop = mem_resps(i).bits.uop
        rob.io.wb_resps(cnt).valid := mem_resps(i).valid && !(mem_uop.use_stq && !mem_uop.is_sc)
        rob.io.wb_resps(cnt).bits := mem_resps(i).bits
        if (!FPGAPlatform) {
            rob.io.debug_wb_valids(cnt) := mem_resps(i).valid && mem_uop.dst_rtype =/= RT_X
            rob.io.debug_wb_wdata(cnt) := mem_resps(i).bits.data
            rob.io.debug_wb_ldst(cnt) := mem_uop.ldst
            rob.io.debug_wb_pc(cnt) := mem_uop.debug_pc
            rob.io.debug_load_uncacheable(cnt) := mem_uop.debug_load_uncacheable

            dontTouch(mem_uop.debug_pc)
        }
        cnt += 1
    }
    for (eu <- exe_units) {
        if (eu.writesIrf) {
            val resp   = eu.io.iresp
            val wb_uop = resp.bits.uop
            val data   = resp.bits.data

            rob.io.wb_resps(cnt).valid := resp.valid && !(wb_uop.use_stq && !wb_uop.is_sc)
            rob.io.wb_resps(cnt).bits  := resp.bits
            if (!FPGAPlatform) {
                rob.io.debug_wb_valids(cnt) := resp.valid && wb_uop.rf_wen && wb_uop.dst_rtype === RT_FIX
                if (eu.hasCSR) {
                    rob.io.debug_wb_wdata(cnt) := Mux(
                        wb_uop.ctrl.csr_cmd =/= CSR_N,
                        csr.io.rdata,
                        data
                    )
                    rob.io.debug_wb_ldst(cnt) := wb_uop.ldst
                    rob.io.debug_wb_pc(cnt) := wb_uop.debug_pc
                } else {
                    rob.io.debug_wb_wdata(cnt) := data
                    rob.io.debug_wb_ldst(cnt) := wb_uop.ldst
                    rob.io.debug_wb_pc(cnt) := wb_uop.debug_pc
                }
                rob.io.debug_load_uncacheable(cnt) := false.B
            }
            cnt += 1

            if(!FPGAPlatform)dontTouch(wb_uop.debug_pc)
        }
    }
    require(cnt == numWritePorts)

    rob.io.brupdate <> brUpdate

    // LSU <> ROB
    rob.io.lsu_clr_bsy := lsu.io.core.clr_bsy
    rob.io.lsu_xcpt <> lsu.io.core.lsu_xcpt

    assert(!(rob.io.com_xcpt.valid && !rob.io.flush.valid),
        "[core] exception occurred, but pipeline flush signal not set!")

    //-------------------------------------------------------------
    // *** debug for difftest
    //-------------------------------------------------------------
    if (!FPGAPlatform) {

        val logic_registers = Module(new LogicRegisters)
        logic_registers.io.commit := rob.io.commit

        val instr_commits = Module(new InstrCommits)
        val rawCommit = WireInit(0.U.asTypeOf(new InstrCommit))
        instr_commits.io.exception := RegNext(rob.io.com_xcpt.valid)
        instr_commits.io.rawCommit := rawCommit
        instr_commits.io.fill_idx  := tlb_data.io.fill_idx

        for (w <- 0 until robParameters.retireWidth) {
            rawCommit.debug_uopc(w)  := rob.io.commit.uops(w).uopc
            rawCommit.debug_pc(w)    := rob.io.commit.uops(w).debug_pc
            rawCommit.debug_ldst(w)  := rob.io.commit.uops(w).ldst
            rawCommit.debug_insts(w) := rob.io.commit.uops(w).debug_inst
            rawCommit.debug_load_uncacheable(w) := rob.io.commit.debug_load_uncacheable(w)
            rawCommit.debug_wdata(w) := rob.io.commit.debug_wdata(w)
            rawCommit.debug_wen(w)   := rob.io.commit.uops(w).ldst_val && rob.io.commit.arch_valids(w)
            rawCommit.valids(w)      := rob.io.commit.arch_valids(w) & (~RegNext(rob.io.com_xcpt.valid))
        }
    }
}
