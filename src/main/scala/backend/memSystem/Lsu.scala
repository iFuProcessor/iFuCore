package iFu.backend

import chisel3._
import chisel3.util._

import iFu.sma._

import iFu.tlb._

import iFu.common._
import iFu.common.Consts._
import iFu.common.CauseCode._
import iFu.util._

import iFu.lsu.utils._

object AgePriorityEncoder {
    def apply(in: Seq[Bool], head: UInt): UInt = {
        val n = in.size
        val width = log2Ceil(in.size)
        val n_padded = 1 << width
        val temp_vec = (0 until n_padded).map(i => if (i < n) in(i) && i.U >= head else false.B) ++ in
        val idx = PriorityEncoder(temp_vec)
        idx(width-1, 0) //discard msb
    }
}

class LSUExeIO extends CoreBundle {
    val req   = Flipped(new Valid(new FuncUnitResp))
    val iresp = new DecoupledIO(new ExeUnitResp)
}

class LSUTLBDataIO extends CoreBundle {
    val r_req  = Vec(memWidth, Valid(new TLBDataRReq))
    val r_resp = Vec(memWidth, Flipped(Valid(new TLBDataRResp)))
}
/**
 * 输入dispatch阶段的uop，commit的信息，rob，brupdate
 * 输出ldq，stq的索引，是否full，给rob的clear信号
 */
// lsu <> core
class LSUCoreIO extends CoreBundle {
    /** *********************************** */
    val robAddrSz = robParameters.robAddrSz
    val ldqAddrSz = lsuParameters.ldqAddrSz
    val stqAddrSz = lsuParameters.stqAddrSz
    /** ************************************ */

    val exe = Vec(memWidth, new LSUExeIO)

    val dis_uops    = Flipped(Vec(coreWidth, Valid(new MicroOp)))
    val dis_ldq_idx = Output(Vec(coreWidth, UInt(ldqAddrSz.W)))
    val dis_stq_idx = Output(Vec(coreWidth, UInt(stqAddrSz.W)))

    val ldq_full    = Output(Vec(coreWidth,Bool()))
    val stq_full    = Output(Vec(coreWidth,Bool()))

    val commit      = Input(new CommitSignals)
    val commit_load_at_rob_head = Input(Bool())

    val clr_bsy     = Output(Vec(memWidth,Valid(UInt(robAddrSz.W))))    // TODO

    val fence_dmem  = Input(Bool())

    // Speculatively tell the IQs that we'll get load data back next cycle
    val spec_ld_wakeup = Output(Vec(memWidth, Valid(UInt(pregSz.W))))
    val ld_miss     = Output(Bool())

    val brupdate    = Input(new BrUpdateInfo)
    val rob_head_idx= Input(UInt(robAddrSz.W))
    val exception   = Input(Bool())

    val stq_empty   = Output(Bool())
    val dcache_ord  = Output(Bool())

    val lsu_xcpt    = Output(Valid(new Exception))

    val tlb_data    = new LSUTLBDataIO
}

class LSUCsrIO extends CoreBundle {
    val dtlb_csr_ctx = Input(new TLBCsrContext)
    val llbit        = Input(Bool())
}

class LSUIO extends CoreBundle {
    val core  = new LSUCoreIO
    val smar = Vec(2, new SMAR)
    val smaw = Vec(2, new SMAW)
    val csr   = new LSUCsrIO
}

trait HasUop extends Bundle {
    val uop = new MicroOp
}

class LDQEntry extends CoreBundle with HasUop {
    /** *********************************** */
    val numStqEntries = lsuParameters.numSTQEntries
    val numLdqEntries = lsuParameters.numLDQEntries
    val stqAddrSz     = lsuParameters.stqAddrSz
    val ldqAddrSz     = lsuParameters.ldqAddrSz

    // require(isPow2(numLdqEntries), "numLdqEntries must be a power of 2")
    // require(isPow2(numStqEntries), "numStqEntries must be a power of 2")
    /** ************************************ */

    val addr             = Valid(UInt(xLen.W))
    val xcpt_valid       = Bool()

    val executed         = Bool()
    val succeeded        = Bool() // dcache 返回结果
    val order_fail       = Bool() // raw 冒险

    val st_dep_mask      = UInt(numStqEntries.W)
    val youngest_stq_idx = UInt(stqAddrSz.W) // 第一条晚于该load的store指令

    val forward_std_val  = Bool()
    val forward_stq_idx  = UInt(stqAddrSz.W)

    val is_uncacheable   = Bool()
}

class STQEntry extends CoreBundle with HasUop {
    val addr            = Valid(UInt(xLen.W))
    val xcpt_valid      = Bool()
    val data            = Valid(UInt(xLen.W))
    val is_uncacheable  = Bool()

    val committed       = Bool()
    val succeeded       = Bool() // 访存成功
}

class Lsu extends CoreModule {
    //-------------------------------------------------------------
    val numStqEntries = lsuParameters.numSTQEntries
    val numLdqEntries = lsuParameters.numLDQEntries
    val stqAddrSz     = lsuParameters.stqAddrSz
    val ldqAddrSz     = lsuParameters.ldqAddrSz
    //-------------------------------------------------------------

    assert(memWidth == 2, "LSU only support memWidth = 2")

    //-------------------------------------------------------------
    def widthMap[T <: Data](f: Int => T) = VecInit((0 until memWidth).map(f))
    //-------------------------------------------------------------

    //-------------------------------------------------------------
    val io = IO(new LSUIO)
    io.core.exe(0).iresp := DontCare
    io.core.exe(1).iresp := DontCare
    //-------------------------------------------------------------

    // -------------------------------------------------------------
    val dtlb = Module(new DTLB) // latency = 1
    dtlb.io.dtlb_csr_context   := io.csr.dtlb_csr_ctx
    io.core.tlb_data.r_req     := dtlb.io.r_req
    dtlb.io.r_resp             := io.core.tlb_data.r_resp

    val dcache  = Module(new NonBlockingDcache) // latency = 2
    dcache.io.lsu.llbit        := io.csr.llbit
    dcache.io.lsu.brupdate     := io.core.brupdate
    dcache.io.lsu.exception    := io.core.exception
    io.smar(0) <> dcache.io.smar(0)
    io.smar(1) <> dcache.io.smar(1)
    io.smaw(0) <> dcache.io.smaw(0)
    io.smaw(1) <> dcache.io.smaw(1)
    // -------------------------------------------------------------

    // -------------------------------------------------------------
    // val ldq = Reg(Vec(numLdqEntries, Valid(new LDQEntry)))
    val ldq = RegInit(0.U.asTypeOf(Vec(numLdqEntries, Valid(new LDQEntry))))
    val ldq_head = Reg(UInt(ldqAddrSz.W))
    val ldq_tail = Reg(UInt(ldqAddrSz.W))
    // -------------------------------------------------------------

    // -------------------------------------------------------------
    // val stq = Reg(Vec(numStqEntries, Valid(new STQEntry)))
    val stq = RegInit(0.U.asTypeOf(Vec(numStqEntries, Valid(new STQEntry))))
    val stq_head = Reg(UInt(stqAddrSz.W)) // will dequeue
    val stq_tail = Reg(UInt(stqAddrSz.W))
    val stq_commit_head  = Reg(UInt(stqAddrSz.W))   // point to next store to commit
    val stq_execute_head = Reg(UInt(stqAddrSz.W))   // point to next store to execute
    // -------------------------------------------------------------

    // -------------------------------------------------------------
    val finish_store = WireInit(false.B)    // we finish a store this cycle
    // -------------------------------------------------------------

    val live_store_mask = RegInit(0.U(numStqEntries.W)) // 当前哪些位置有store指令 // diff with valid?
    var next_live_store_mask = Mux(finish_store, 
        live_store_mask & (~(1.U << stq_head)).asUInt, // -> 如果store指令提交了，那么该位置零
        live_store_mask
    )

/*=============================================================================*/
    //-------------------------------------------------------------
    // Dequeue store entries
    //-------------------------------------------------------------
    for (i <- 0 until numLdqEntries) { // store指令提交后，修改ldq中与该条指令有关的位
        when(finish_store) {
            ldq(i).bits.st_dep_mask := ldq(i).bits.st_dep_mask & (~(1.U << stq_head)).asUInt
        }
    }

/*=============================================================================*/
    //-------------------------------------------------------------
    // Enqueue new entries
    //-------------------------------------------------------------
    // var ld_enq_idx = ldq_tail
    // var st_enq_idx = stq_tail
    // var ldq_full = Bool() // 过程中出现ldq已满
    // var stq_full = Bool() // 过程中出现stq已满
    val padded_ldq_head = ldq_head.pad(ldq_head.getWidth + 1)
    val padded_ldq_tail = ldq_tail.pad(ldq_tail.getWidth + 1)
    val padded_stq_head = stq_head.pad(stq_head.getWidth + 1)
    val padded_stq_tail = stq_tail.pad(stq_tail.getWidth + 1)

    val ldq_full = Mux(padded_ldq_tail >= padded_ldq_head, padded_ldq_tail + coreWidth.U >= padded_ldq_head + numLdqEntries.U,
                                                                padded_ldq_tail + coreWidth.U >= padded_ldq_head)

    val stq_full = Mux(padded_stq_tail >= padded_stq_head, padded_stq_tail + coreWidth.U >= padded_stq_head + numStqEntries.U,
                                                                padded_stq_tail + coreWidth.U >= padded_stq_head)

    val idxAllocator = Module(new IndexAllocator)
    idxAllocator.io.dis_uops := io.core.dis_uops
    idxAllocator.io.old_ldq_tail := ldq_tail
    idxAllocator.io.old_stq_tail := stq_tail
    for (w <- 0 until coreWidth) {
        // ldq_full = WrapInc(ld_enq_idx, numLdqEntries) === ldq_head
        io.core.ldq_full(w) := ldq_full
        
        val ld_enq_idx = idxAllocator.io.ldq_enq_idxs(w)
        io.core.dis_ldq_idx(w) := ld_enq_idx

        // stq_full = WrapInc(st_enq_idx, numStqEntries) === stq_head
        io.core.stq_full(w) := stq_full

        val st_enq_idx = idxAllocator.io.stq_enq_idxs(w)
        io.core.dis_stq_idx(w) := st_enq_idx

        // enq
        val dis_ld_val = io.core.dis_uops(w).valid && io.core.dis_uops(w).bits.use_ldq && !io.core.dis_uops(w).bits.xcpt_valid
        val dis_st_val = io.core.dis_uops(w).valid && io.core.dis_uops(w).bits.use_stq && !io.core.dis_uops(w).bits.xcpt_valid
        when(dis_ld_val) {
            ldq(ld_enq_idx).valid := true.B
            ldq(ld_enq_idx).bits.uop := io.core.dis_uops(w).bits
            ldq(ld_enq_idx).bits.youngest_stq_idx := st_enq_idx
            ldq(ld_enq_idx).bits.st_dep_mask := next_live_store_mask

            ldq(ld_enq_idx).bits.addr.valid := false.B
            ldq(ld_enq_idx).bits.executed := false.B
            ldq(ld_enq_idx).bits.succeeded := false.B
            ldq(ld_enq_idx).bits.order_fail := false.B
            ldq(ld_enq_idx).bits.forward_std_val := false.B

            assert(ld_enq_idx === io.core.dis_uops(w).bits.ldqIdx, "[lsu] mismatch enq load tag.")
            assert(!ldq(ld_enq_idx).valid, "[lsu] Enqueuing uop is overwriting ldq entries")
        } .elsewhen(dis_st_val) {
            stq(st_enq_idx).valid := true.B
            stq(st_enq_idx).bits.uop := io.core.dis_uops(w).bits

            stq(st_enq_idx).bits.addr.valid := false.B
            stq(st_enq_idx).bits.data.valid := false.B
            stq(st_enq_idx).bits.committed := false.B
            stq(st_enq_idx).bits.succeeded := false.B

            assert(st_enq_idx === io.core.dis_uops(w).bits.stqIdx, "[lsu] mismatch enq store tag.")
            // assert(!stq(st_enq_idx).valid, "[lsu] Enqueuing uop is overwriting stq entries")
        }

        // ld_enq_idx = Mux(dis_ld_val, WrapInc(ld_enq_idx, numLdqEntries), ld_enq_idx)

        next_live_store_mask = Mux(dis_st_val,      // 新增store指令
            next_live_store_mask | (1.U << st_enq_idx).asUInt,
            next_live_store_mask
        )

        // st_enq_idx = Mux(dis_st_val, WrapInc(st_enq_idx, numStqEntries), st_enq_idx)

        assert(!(dis_ld_val && dis_st_val), "A UOP is trying to go into both the LDQ and the STQ")
    }

    // ldq_tail := ld_enq_idx
    // stq_tail := st_enq_idx
    ldq_tail := idxAllocator.io.new_ldq_tail
    stq_tail := idxAllocator.io.new_stq_tail

    val stqEmpty = VecInit((0 until numStqEntries).map{ i => !stq(i).valid }).asUInt.andR
    dcache.io.lsu.fence_dmem := io.core.fence_dmem
    io.core.stq_empty  := stqEmpty
    io.core.dcache_ord := RegNext(dcache.io.lsu.ordered)    // delay one cycle for better timing

/*=============================================================================*/

// -----------------------------------------------------------------------
// s0 stage: dtlb access
    val s0_exe_tlb_uop   = widthMap(w => io.core.exe(w).req.bits.uop)
    // we send a request current cycle(s0)
    val s0_exe_tlb_valid = widthMap(w =>
        io.core.exe(w).req.valid && (
            io.core.exe(w).req.bits.uop.ctrl.is_load ||
            io.core.exe(w).req.bits.uop.ctrl.is_sta
        )                                                   &&
        !io.core.exception                                  &&
        !IsKilledByBranch(io.core.brupdate, s0_exe_tlb_uop(w))
    )
    val s0_exe_tlb_vaddr = widthMap(w => io.core.exe(w).req.bits.addr)

    for (w <- 0 until memWidth) {
        dtlb.io.req(w).valid        := s0_exe_tlb_valid(w)
        dtlb.io.req(w).bits.vaddr   := s0_exe_tlb_vaddr(w)
        dtlb.io.req(w).bits.size    := s0_exe_tlb_uop(w).mem_size
        dtlb.io.req(w).bits.use_stq := s0_exe_tlb_uop(w).use_stq
        dtlb.io.req(w).bits.use_ldq := s0_exe_tlb_uop(w).use_ldq
    }
// -----------------------------------------------------------------------
// s1 stage: handle tlb response
    val s1_exe_req = Reg(Vec(memWidth, Valid(new FuncUnitResp)))
    for (w <- 0 until memWidth) {
        s1_exe_req(w).valid :=
            io.core.exe(w).req.valid &&
            !io.core.exception       &&
            !IsKilledByBranch(io.core.brupdate, io.core.exe(w).req.bits.uop)

        s1_exe_req(w).bits := io.core.exe(w).req.bits
        s1_exe_req(w).bits.uop.brMask :=
            GetNewBrMask(io.core.brupdate, io.core.exe(w).req.bits.uop)
    }

    val s1_tlb_xcpt_valids = Wire(Vec(memWidth, Bool()))
    for (w <- 0 until memWidth) {
        s1_tlb_xcpt_valids(w) :=
            dtlb.io.resp(w).exception.valid                          &&
            RegNext(    // we send a request last cycle
                s0_exe_tlb_valid(w) &&
                !(s0_exe_tlb_uop(w).is_sc && !io.csr.llbit)    // sc throw exception only when llbit is 1
            )                                                        &&
            !io.core.exception                                       &&
            !IsKilledByBranch(io.core.brupdate, s1_exe_req(w).bits.uop)
    }
    val s1_tlb_xcpt_uops   = WireInit(widthMap(w => s1_exe_req(w).bits.uop))
    val s1_tlb_xcpt_causes = WireInit(widthMap(w => dtlb.io.resp(w).exception.bits.xcpt_cause))
    val s1_tlb_xcpt_vaddrs = RegNext(s0_exe_tlb_vaddr)

    // search for the oldest exception
    val s1_tlb_xcpt_valid = s1_tlb_xcpt_valids.asUInt.orR
    val s1_tlb_xcpt_cause = Wire(UInt())
    val s1_tlb_xcpt_uop   = Wire(new MicroOp)
    val s1_tlb_xcpt_vaddr = Wire(UInt())

    s1_tlb_xcpt_cause := s1_tlb_xcpt_causes(0)
    s1_tlb_xcpt_uop   := s1_tlb_xcpt_uops(0)
    s1_tlb_xcpt_vaddr := s1_tlb_xcpt_vaddrs(0)

    when (
        s1_tlb_xcpt_valids(1) &&
        (
            !s1_tlb_xcpt_valids(0) ||   // pipeline 0 has no exception, choose pipeline 1
            IsOlder(s1_tlb_xcpt_uops(1).robIdx, s1_tlb_xcpt_uops(0).robIdx, io.core.rob_head_idx)
        )
    ) {
        s1_tlb_xcpt_cause := s1_tlb_xcpt_causes(1)
        s1_tlb_xcpt_uop   := s1_tlb_xcpt_uops(1)
        s1_tlb_xcpt_vaddr := s1_tlb_xcpt_vaddrs(1)
    }

    val s1_exe_tlb_xcpt = widthMap(w =>
        dtlb.io.resp(w).exception.valid &&
        RegNext(
            s0_exe_tlb_valid(w) &&
            !(s0_exe_tlb_uop(w).is_sc && !io.csr.llbit)    // sc throw exception only when llbit is 1
        )
    )
    val s1_exe_tlb_paddr = widthMap(w => dtlb.io.resp(w).paddr)
    val s1_exe_tlb_uncacheable = widthMap(w => dtlb.io.resp(w).is_uncacheable)

    // update ldq and stq
    for (w <- 0 until memWidth) {
        when (s1_tlb_xcpt_valids(w)) {
            when (s1_tlb_xcpt_uops(w).use_ldq) {
                ldq(s1_tlb_xcpt_uops(w).ldqIdx).bits.uop.xcpt_valid := true.B
            } .otherwise {
                stq(s1_tlb_xcpt_uops(w).stqIdx).bits.uop.xcpt_valid := true.B
            }
        }
    }
// -----------------------------------------------------------------------
// s1 stage: prepare for scheduling
    val will_fire_load_incoming = Wire(Vec(memWidth, Bool()))
    val will_fire_sta_incoming  = Wire(Vec(memWidth, Bool()))
    val will_fire_std_incoming  = Wire(Vec(memWidth, Bool()))
    val will_fire_store_commit  = Wire(Vec(memWidth, Bool()))
    val will_fire_load_wakeup   = Wire(Vec(memWidth, Bool()))

    // block load wakeups for the next 2 cycles after we wake up the load
    val p0_block_load_mask = WireInit(VecInit(IndexedSeq.fill(numLdqEntries){ false.B }))
    val p1_block_load_mask = RegNext(p0_block_load_mask)
    val p2_block_load_mask = RegNext(p1_block_load_mask)

    // load_incoming
    val ldq_incoming_idx = widthMap(i => s1_exe_req(i).bits.uop.ldqIdx)
    val ldq_incoming_e   = widthMap(i => ldq(ldq_incoming_idx(i)))

    // sta_incoming / std_incoming
    val stq_incoming_idx = widthMap(i => s1_exe_req(i).bits.uop.stqIdx)
    val stq_incoming_e   = widthMap(i => stq(stq_incoming_idx(i)))

    // store_commit
    val stq_commit_e     = stq(stq_execute_head)

    // load_wakeup
    val ldq_wakeup_idx = RegNext(
        AgePriorityEncoder((0 until numLdqEntries).map(
            i => {
                val e = ldq(i).bits
                val block = p0_block_load_mask(i) || p1_block_load_mask(i)
                e.addr.valid && !e.executed && !e.succeeded && !e.xcpt_valid && !block
            }
        ), ldq_head)
    )
    val ldq_wakeup_e = ldq(ldq_wakeup_idx)

    // can_fire_xxx logic
    val can_fire_load_incoming = widthMap(
        w => s1_exe_req(w).valid && s1_exe_req(w).bits.uop.ctrl.is_load
    )
    val can_fire_sta_incoming = widthMap(
        w => s1_exe_req(w).valid && s1_exe_req(w).bits.uop.ctrl.is_sta && !s1_exe_req(w).bits.uop.ctrl.is_std
    )
    val can_fire_std_incoming = widthMap(
        w => s1_exe_req(w).valid && s1_exe_req(w).bits.uop.ctrl.is_std && !s1_exe_req(w).bits.uop.ctrl.is_sta
    )
    val can_fire_store_commit = widthMap(w => (
        (w == 0).B                        &&    // only the first pipeline can fire a store commit
        stq_commit_e.valid                &&
        !stq_commit_e.bits.uop.xcpt_valid &&
        !stq_commit_e.bits.succeeded      &&
        (
            stq_commit_e.bits.committed ||  // for a normal store, we execute it after it is committed
            (                               // for a sc, we execute it after all its requires are satisfied
                stq_commit_e.bits.uop.is_sc   &&
                stq_commit_e.bits.addr.valid  &&    // we have the physical address
                !stq_commit_e.bits.xcpt_valid &&
                stq_commit_e.bits.data.valid        // we have the data
            )
        )
    ))
    val can_fire_load_wakeup = widthMap(w => (
        (w == memWidth - 1).B               &&  // only the last pipeline can fire a load wakeup
        ldq_wakeup_e.valid                  &&
        ldq_wakeup_e.bits.addr.valid        &&
        !ldq_wakeup_e.bits.xcpt_valid       &&
        !ldq_wakeup_e.bits.executed         &&
        !ldq_wakeup_e.bits.succeeded        &&
        !ldq_wakeup_e.bits.order_fail       &&
        !p1_block_load_mask(ldq_wakeup_idx) &&
        !p2_block_load_mask(ldq_wakeup_idx) &&
        (
            !ldq_wakeup_e.bits.is_uncacheable ||    // block uncacheable loads
            (                                       // until the load is at the head of the ROB
                io.core.commit_load_at_rob_head &&
                ldq_head === ldq_wakeup_idx     &&  // there are no loads ahead of it(insure all uncacheable loads are ordered)
                ldq_wakeup_e.bits.st_dep_mask.asUInt === 0.U    // there are no relative stores ahead of it
            )
        )
    ))

// -----------------------------------------------------------------------
// s1 stage: out of order scheduling
    for (w <- 0 until memWidth) {
        var dcache_avail = true.B
        var lcam_avail   = true.B

        def lsu_sched(can_fire: Bool, uses_dcache: Boolean, uses_lcam: Boolean): Bool = {
            val will_fire = (
                can_fire                          && 
                !(uses_lcam.B && !lcam_avail)     &&
                !(uses_dcache.B && !dcache_avail)
            )

            lcam_avail   = lcam_avail   && !(will_fire && uses_lcam.B)
            dcache_avail = dcache_avail && !(will_fire && uses_dcache.B)
            will_fire
        }

        will_fire_load_incoming(w) := lsu_sched(can_fire_load_incoming(w), true , true ) // DC , LCAM
        will_fire_sta_incoming(w)  := lsu_sched(can_fire_sta_incoming(w) , false, true ) //    , LCAM
        will_fire_std_incoming(w)  := lsu_sched(can_fire_std_incoming(w) , false, false) //    ,     
        will_fire_load_wakeup(w)   := lsu_sched(can_fire_load_wakeup(w)  , true , true ) // DC , LCAM
        will_fire_store_commit(w)  := lsu_sched(can_fire_store_commit(w) , true , false) // DC ,     

        when (will_fire_load_wakeup(w)) {
            p0_block_load_mask(ldq_wakeup_idx) := true.B
        } .elsewhen(will_fire_load_incoming(w)) {
            p0_block_load_mask(s1_exe_req(w).bits.uop.ldqIdx) := true.B
        }
    }
// -----------------------------------------------------------------------
// s1 stage: dcache access
    val dmem_req = Wire(Vec(memWidth, Valid(new DCacheReq)))
    dcache.io.lsu.req.valid := VecInit(dmem_req.map(_.valid)).asUInt.orR
    dcache.io.lsu.req.bits := dmem_req
    // decache will accept both two requests or reject both two requests
    val dmem_req_fire = widthMap(w => dmem_req(w).valid && dcache.io.lsu.req.fire)

    val s1_executing_loads = WireInit(VecInit((0 until numLdqEntries).map(x => false.B)))
    for (w <- 0 until memWidth) {
        dmem_req(w).valid               := false.B
        dmem_req(w).bits                := DontCare
        dmem_req(w).bits.is_uncacheable := false.B

        when (will_fire_load_incoming(w)) {
            assert(!ldq_incoming_e(w).bits.executed)

            dmem_req(w).valid     := !s1_exe_tlb_xcpt(w) && !s1_exe_tlb_uncacheable(w)
            dmem_req(w).bits.addr := s1_exe_tlb_paddr(w)
            dmem_req(w).bits.uop  := s1_exe_req(w).bits.uop

            s1_executing_loads(ldq_incoming_idx(w)) := dmem_req_fire(w)
        } .elsewhen(will_fire_store_commit(w)) {
            dmem_req(w).valid               := true.B
            dmem_req(w).bits.addr           := stq_commit_e.bits.addr.bits
            dmem_req(w).bits.data           := stq_commit_e.bits.data.bits
            dmem_req(w).bits.mask           := storeMaskGen(
                stq_commit_e.bits.addr.bits(1,0),
                stq_commit_e.bits.uop.mem_size
            )
            dmem_req(w).bits.uop            := stq_commit_e.bits.uop
            dmem_req(w).bits.is_uncacheable := stq_commit_e.bits.is_uncacheable

            stq_execute_head := Mux(
                dmem_req_fire(w), WrapInc(stq_execute_head, numStqEntries),
                stq_execute_head
            )
            stq(stq_execute_head).bits.succeeded := false.B
        } .elsewhen(will_fire_load_wakeup(w)) {
            assert(!ldq_wakeup_e.bits.executed && !ldq_wakeup_e.bits.xcpt_valid)

            dmem_req(w).valid               := true.B
            dmem_req(w).bits.addr           := ldq_wakeup_e.bits.addr.bits
            dmem_req(w).bits.uop            := ldq_wakeup_e.bits.uop
            dmem_req(w).bits.is_uncacheable := ldq_wakeup_e.bits.is_uncacheable

            s1_executing_loads(ldq_wakeup_idx) := dmem_req_fire(w)
        }
    }

    // decache only accept 1 uncacheable request
    // priority: store > load
    when (dmem_req(0).bits.is_uncacheable) {
        dmem_req(1).valid := false.B
    } .elsewhen (dmem_req(1).bits.is_uncacheable) {
        dmem_req(0).valid := false.B
    }
// -----------------------------------------------------------------------
// s1 stage: write address/data into the LDQ/STQ
    for (w <- 0 until memWidth) {        
        when (will_fire_load_incoming(w)) {
            val ldq_idx = ldq_incoming_idx(w)
            ldq(ldq_idx).bits.addr.valid     := true.B
            ldq(ldq_idx).bits.addr.bits      := s1_exe_tlb_paddr(w)
            /* ldq(ldq_idx).bits.uop.pdst       := s1_exe_req(w).bits.uop.pdst */
            assert(ldq(ldq_idx).bits.uop.pdst === s1_exe_req(w).bits.uop.pdst, "[lsu] mismatch load pdst")
            ldq(ldq_idx).bits.xcpt_valid     := s1_exe_tlb_xcpt(w)
            ldq(ldq_idx).bits.is_uncacheable := s1_exe_tlb_uncacheable(w)

            assert(!(will_fire_load_incoming(w) && ldq_incoming_e(w).bits.addr.valid),
                "[lsu] Incoming load is overwriting a valid address")
        }

        when (will_fire_sta_incoming(w)) {
            val stq_idx = stq_incoming_idx(w)
            stq(stq_idx).bits.addr.valid     := true.B
            stq(stq_idx).bits.addr.bits      := s1_exe_tlb_paddr(w)
            stq(stq_idx).bits.uop.pdst       := s1_exe_req(w).bits.uop.pdst
            stq(stq_idx).bits.xcpt_valid     := s1_exe_tlb_xcpt(w)
            stq(stq_idx).bits.is_uncacheable := s1_exe_tlb_uncacheable(w)

            assert(!(will_fire_sta_incoming(w) && stq_incoming_e(w).bits.addr.valid),
                "[lsu] Incoming store is overwriting a valid address")
        }

        when (will_fire_std_incoming(w)) {
            val stq_idx = stq_incoming_idx(w)
            stq(stq_idx).bits.data.valid := true.B
            stq(stq_idx).bits.data.bits  := s1_exe_req(w).bits.data

            assert(!(stq(stq_idx).bits.data.valid),
                "[lsu] Incoming store is overwriting a valid data entry")
        }
    }
// -----------------------------------------------------------------------
// s2 stage: check which instructions we issued last cycle
    val s1_exe_req_killed = widthMap(w => IsKilledByBranch(io.core.brupdate, s1_exe_req(w).bits.uop))

    val fired_load_incoming = widthMap(w => RegNext(will_fire_load_incoming(w) && !s1_exe_req_killed(w)))
    val fired_sta_incoming  = widthMap(w => RegNext(will_fire_sta_incoming(w)  && !s1_exe_req_killed(w)))
    val fired_std_incoming  = widthMap(w => RegNext(will_fire_std_incoming(w)  && !s1_exe_req_killed(w)))
    val fired_load_wakeup   = widthMap(w => RegNext(will_fire_load_wakeup(w)   && !IsKilledByBranch(io.core.brupdate, ldq_wakeup_e.bits.uop)))
// -----------------------------------------------------------------------
// s2 stage: prepare uops, entries, and so on
    val mem_incoming_uop   = RegNext(widthMap(w => UpdateBrMask(io.core.brupdate, s1_exe_req(w).bits.uop)))

    val mem_ldq_incoming_e = RegNext(widthMap(w => UpdateBrMask(io.core.brupdate, ldq_incoming_e(w))))
    val mem_ldq_wakeup_e   = RegNext(UpdateBrMask(io.core.brupdate, ldq_wakeup_e))
    val mem_ldq_e = widthMap(w =>
        Mux(fired_load_incoming(w), mem_ldq_incoming_e(w),
        Mux(fired_load_wakeup(w)  , mem_ldq_wakeup_e,
                                    0.U.asTypeOf(Valid(new LDQEntry))))
    )

    val mem_stq_incoming_e = RegNext(widthMap(w => UpdateBrMask(io.core.brupdate, stq_incoming_e(w))))
    val mem_stq_e = widthMap(w =>
        Mux(fired_sta_incoming(w), mem_stq_incoming_e(w),
                                   0.U.asTypeOf(Valid(new STQEntry)))
    )

    val mem_tlb_xcpt        = RegNext(s1_exe_tlb_xcpt)
    val mem_paddr           = RegNext(widthMap(w => dmem_req(w).bits.addr))
    val mem_tlb_uncacheable = RegNext(s1_exe_tlb_uncacheable)

    val do_ld_search = widthMap(w => ((fired_load_incoming(w) && !mem_tlb_xcpt(w)) || fired_load_wakeup(w)))
    val do_st_search = widthMap(w => fired_sta_incoming(w) && !mem_tlb_xcpt(w))

    // we get store address from dtlb, and load address from pipeline
    val lcam_addr = widthMap(w =>
        Mux(fired_sta_incoming(w), RegNext(s1_exe_tlb_paddr(w)),
                                   mem_paddr(w))
    )
    val lcam_uop = widthMap(w => 
        Mux(do_st_search(w), mem_stq_e(w).bits.uop,
        Mux(do_ld_search(w), mem_ldq_e(w).bits.uop,
                             NullMicroOp))
    )
    val lcam_mask = widthMap(w => GenByteMask(lcam_addr(w), lcam_uop(w).mem_size))
    val lcam_st_dep_mask = widthMap(w => mem_ldq_e(w).bits.st_dep_mask)
    val lcam_ldq_idx = widthMap(w =>
        Mux(fired_load_incoming(w), mem_incoming_uop(w).ldqIdx,
        Mux(fired_load_wakeup(w)  , RegNext(ldq_wakeup_idx),
                                    0.U))
    )
    val lcam_stq_idx = widthMap(w =>
        Mux(fired_sta_incoming(w), mem_incoming_uop(w).stqIdx, 0.U)
    )
    // can we forward to the load?
    val can_forward_to = WireInit(widthMap(w =>
        Mux(fired_load_incoming(w), !mem_tlb_uncacheable(w),
                                    !ldq(lcam_ldq_idx(w)).bits.is_uncacheable)
    ))

    // stq_idx forwards to ldq_idx
    val mem_forward_valid   = Wire(Vec(memWidth, Bool()))
    val mem_forward_ldq_idx = lcam_ldq_idx
    val mem_forward_ld_addr = lcam_addr
    val mem_forward_stq_idx = Wire(Vec(memWidth, UInt(log2Ceil(numStqEntries).W)))
    val mem_forward_st_addr = Wire(Vec(memWidth, UInt((xLen - 20).W)))

    val wb_forward_valid    = RegNext(mem_forward_valid)
    val wb_forward_ldq_idx  = RegNext(mem_forward_ldq_idx)
    val wb_forward_ld_addr  = RegNext(mem_forward_ld_addr)
    val wb_forward_stq_idx  = RegNext(mem_forward_stq_idx)
    val wb_forward_st_addr  = RegNext(mem_forward_st_addr)
// -----------------------------------------------------------------------
// s2 stage: st-ld search for ordering failures
    // loads which we will throws a mini-exception
    val failed_loads = WireInit(VecInit((0 until numLdqEntries).map(x => false.B)))
    for (i <- 0 until numLdqEntries) {
        val l_valid = ldq(i).valid
        val l_bits  = ldq(i).bits
        val l_addr  = ldq(i).bits.addr.bits
        val l_mask  = GenByteMask(l_addr, l_bits.uop.mem_size)

        // last cycle, there is a store forword to this load
        val l_forwarders      = widthMap(w => wb_forward_valid(w) && wb_forward_ldq_idx(w) === i.U)
        val l_is_forwarding   = l_forwarders.reduce(_ || _)
        val l_forward_stq_idx = Mux(l_is_forwarding, Mux1H(l_forwarders, wb_forward_stq_idx), l_bits.forward_stq_idx)

        val offsetBits = dcacheParameters.nOffsetBits
        // same dcache line
        val block_addr_matches = widthMap(w => lcam_addr(w) >> offsetBits === l_addr >> offsetBits)
        // same word in the dcache line
        val word_addr_matches  = widthMap(w => block_addr_matches(w) && lcam_addr(w)(offsetBits - 1, 2) === l_addr(offsetBits - 1, 2))
        // mask overlap
        val mask_overlap = widthMap(w => (l_mask & lcam_mask(w)).orR)

        // searcher is a store
        for (w <- 0 until memWidth) {
            when (
                do_st_search(w)                                          && // check current store
                l_valid && l_bits.addr.valid && !l_bits.xcpt_valid       && // this load is valid
                (l_bits.executed || l_bits.succeeded || l_is_forwarding) && // this load has been executed
                l_bits.st_dep_mask(lcam_stq_idx(w))                      && // this load is dependent on the store
                word_addr_matches(w) && mask_overlap(w)                     // this load has overlap with the store
            ) {
                // this store is younger than the load.forward_stq_idx(this load is forwarded before)
                val forwarded_is_older = IsOlder(l_forward_stq_idx, lcam_stq_idx(w), l_bits.youngest_stq_idx)
                when (
                    !l_bits.forward_std_val || // If the load wasn't forwarded, it definitely failed
                    ((l_forward_stq_idx =/= lcam_stq_idx(w)) && forwarded_is_older) // If the load forwarded from a older store, it failed
                ) {
                    ldq(i).bits.order_fail := true.B
                    failed_loads(i)        := true.B
                }
            }
        }
    }
// -----------------------------------------------------------------------
// s2 stage: ld-st search for forwarding opportunities
    for (w <- 0 until memWidth) {
        dcache.io.lsu.s1_kill(w) := false.B
    }

    val s2_executing_loads = RegNext(s1_executing_loads)
    val s2_set_execute = WireInit(s2_executing_loads)

    // Mask of stores which we conflict on address with
    val ldst_addr_matches = WireInit(widthMap(w => VecInit((0 until numStqEntries).map(x => false.B))))
    // Mask of stores which we can forward from
    val ldst_forward_matches = WireInit(widthMap(w => VecInit((0 until numStqEntries).map(x => false.B))))
    val s2_load_need_killed = WireInit(widthMap(w => VecInit((0 until numStqEntries).map(x => false.B))))
    val s2_load_need_cancel = WireInit(widthMap(w => VecInit((0 until numStqEntries).map(x => false.B))))
    for (i <- 0 until numStqEntries) {
        val s_addr = stq(i).bits.addr.bits
        val s_uop  = stq(i).bits.uop
        val word_addr_matches = widthMap(w => ( // same word
            stq(i).bits.addr.valid  &&
            !stq(i).bits.xcpt_valid &&
            /* (s_addr(19, 2) === lcam_addr(w)(19, 2)) */
            IsEqual(s_addr(19, 2), lcam_addr(w)(19, 2))
        ))
        val write_mask = GenByteMask(s_addr, s_uop.mem_size)

        // searcher is a load
        for (w <- 0 until memWidth) {
            when (do_ld_search(w) && stq(i).valid && lcam_st_dep_mask(w)(i)) {
                when (((lcam_mask(w) & write_mask) === lcam_mask(w)) && word_addr_matches(w) && can_forward_to(w)) {
                    ldst_addr_matches(w)(i)         := true.B
                    ldst_forward_matches(w)(i)      := true.B
                    /* dcache.io.lsu.s1_kill(w)        := RegNext(dmem_req_fire(w)) */
                    s2_load_need_killed(w)(i)       := RegNext(dmem_req_fire(w))
                    /* s2_set_execute(lcam_ldq_idx(w)) := false.B */
                    s2_load_need_cancel(w)(i)       := true.B
                } .elsewhen (((lcam_mask(w) & write_mask) =/= 0.U) && word_addr_matches(w)) {
                    ldst_addr_matches(w)(i)         := true.B
                    /* dcache.io.lsu.s1_kill(w)        := RegNext(dmem_req_fire(w)) */
                    s2_load_need_killed(w)(i)       := RegNext(dmem_req_fire(w))
                    /* s2_set_execute(lcam_ldq_idx(w)) := false.B */
                    s2_load_need_cancel(w)(i)       := true.B
                } .elsewhen(s_uop.is_sc) {
                    ldst_addr_matches(w)(i)         := true.B
                    /* dcache.io.lsu.s1_kill(w)        := RegNext(dmem_req_fire(w)) */
                    s2_load_need_killed(w)(i)       := RegNext(dmem_req_fire(w))
                    /* s2_set_execute(lcam_ldq_idx(w)) := false.B */
                    s2_load_need_cancel(w)(i)       := true.B
                }
            }
        }
    }
    dcache.io.lsu.s1_kill zip s2_load_need_killed map {
        case (kill, need_killed) => kill := need_killed.asUInt.orR
    }
    for (w <- 0 until memWidth) {
        when (s2_load_need_cancel(w).asUInt.orR) {
            s2_set_execute(lcam_ldq_idx(w)) := false.B
        }
    }
// -----------------------------------------------------------------------
// s2 stage: set the load as executed
    for (i <- 0 until numLdqEntries) {
        when (s2_set_execute(i)) {
            ldq(i).bits.executed := true.B
        }
    }
// -----------------------------------------------------------------------
// s2 stage: forwarding logic
    val forwarding_age_logic = Seq.fill(memWidth) {
        Module(new ForwardingAgeLogic(numStqEntries))
    }
    for (w <- 0 until memWidth) {
        forwarding_age_logic(w).io.addr_matches    := ldst_addr_matches(w).asUInt
        forwarding_age_logic(w).io.youngest_st_idx := lcam_uop(w).stqIdx
    }
    val forwarding_idx = widthMap(w => forwarding_age_logic(w).io.forwarding_idx)

    // forward if st-ld forwarding is possible
    mem_forward_valid := widthMap(w => (
        ldst_forward_matches(w)(forwarding_idx(w)) &&
        !IsKilledByBranch(io.core.brupdate, lcam_uop(w)) &&
        !io.core.exception && !RegNext(io.core.exception)
    ))
    mem_forward_stq_idx := forwarding_idx
    mem_forward_st_addr zip forwarding_idx map {
        case (st_addr, idx) => st_addr := stq(idx).bits.addr.bits(xLen - 1, 20)
    }
// -----------------------------------------------------------------------
// s2 stage: exception detection
    // one exception port, but multiple causes!
    // - 1 the incoming store-address finds a faulting load(mini-exception)
    // - 2 the incoming load or store address is excepting. It must be older and thus takes precedent.

    // detect which loads get marked as failures, but broadcast to the ROB the oldest failing load
    val temp_bits = (VecInit(
        VecInit.tabulate(numLdqEntries)(
            i => failed_loads(i) && i.U >= ldq_head
        ) ++
        failed_loads
    )).asUInt

    if(!FPGAPlatform)dontTouch(temp_bits)

    val s2_l_idx = PriorityEncoder(temp_bits)

    // s3 stage
    // wrap
    val l_idx         = RegNext(Mux(s2_l_idx >= numLdqEntries.U, s2_l_idx - numLdqEntries.U, s2_l_idx)(ldqAddrSz - 1, 0))
    val ld_xcpt_valid = RegNext(failed_loads.asUInt.orR) && ldq(l_idx).valid
    val ld_xcpt_uop   = ldq(l_idx).bits.uop

    /* val r_xcpt_valid = RegInit(false.B) */
    /* val r_xcpt       = Reg(new Exception) */

    // s3 stage: choose the oldest exception
    val use_tlb_xcpt =
        !ld_xcpt_valid ||
        (s1_tlb_xcpt_valid && IsOlder(s1_tlb_xcpt_uop.robIdx, ld_xcpt_uop.robIdx, io.core.rob_head_idx))

    val xcpt_uop = Mux(use_tlb_xcpt, s1_tlb_xcpt_uop, ld_xcpt_uop)

    /* r_xcpt_valid := (ld_xcpt_valid || s1_tlb_xcpt_valid) &&
                    !io.core.exception                   &&
                    !IsKilledByBranch(io.core.brupdate, xcpt_uop) */
    /* r_xcpt.uop        := xcpt_uop */
    /* r_xcpt.uop.brMask := GetNewBrMask(io.core.brupdate, xcpt_uop) */
    /* r_xcpt.cause      := Mux(use_tlb_xcpt, s1_tlb_xcpt_cause, MINI_EXCEPTION_MEM_ORDERING) */
    /* r_xcpt.badvaddr   := s1_tlb_xcpt_vaddr */

    // s3 stage: throw the exception
    /* io.core.lsu_xcpt.valid := r_xcpt_valid && !io.core.exception && !IsKilledByBranch(io.core.brupdate, r_xcpt.uop) */
    io.core.lsu_xcpt.valid           := (
        (ld_xcpt_valid || s1_tlb_xcpt_valid)          &&
        !io.core.exception                            &&
        !IsKilledByBranch(io.core.brupdate, xcpt_uop)
    )
    /* io.core.lsu_xcpt.bits  := r_xcpt */
    io.core.lsu_xcpt.bits.uop        := xcpt_uop
    io.core.lsu_xcpt.bits.uop.brMask := GetNewBrMask(io.core.brupdate, xcpt_uop)
    io.core.lsu_xcpt.bits.cause      := Mux(use_tlb_xcpt, s1_tlb_xcpt_cause, MINI_EXCEPTION_MEM_ORDERING)
    io.core.lsu_xcpt.bits.badvaddr   := s1_tlb_xcpt_vaddr
// -----------------------------------------------------------------------
// s1 stage: speculative wakeup
    for (w <- 0 until memWidth) {
        io.core.spec_ld_wakeup(w).valid := fired_load_incoming(w) && mem_incoming_uop(w).pdst =/= 0.U
        /* io.core.spec_ld_wakeup(w).valid := will_fire_load_incoming(w) && s1_exe_req(w).bits.uop.pdst =/= 0.U */
        io.core.spec_ld_wakeup(w).bits  := mem_incoming_uop(w).pdst
        /* io.core.spec_ld_wakeup(w).bits  := s1_exe_req(w).bits.uop.pdst */
    }
// -----------------------------------------------------------------------
// s3 stage: clear busy bits for store instructions (load's busy bit is cleared when it writes back)
    val clr_bsy_valid   = RegInit(widthMap(w => false.B))
    val clr_bsy_rob_idx = Reg(Vec(memWidth, UInt(robParameters.robAddrSz.W)))
    val clr_bsy_brmask  = Reg(Vec(memWidth, UInt(maxBrCount.W)))

    for (w <- 0 until memWidth) {
        clr_bsy_valid(w)   := false.B

        clr_bsy_rob_idx(w) := mem_stq_incoming_e(w).bits.uop.robIdx
        clr_bsy_brmask(w)  := GetNewBrMask(io.core.brupdate, mem_stq_incoming_e(w).bits.uop)        

        when(fired_sta_incoming(w)) {
            clr_bsy_valid(w) := (
                mem_stq_incoming_e(w).valid            &&
                mem_stq_incoming_e(w).bits.data.valid  &&
                !mem_tlb_xcpt(w)                       &&
                !mem_stq_incoming_e(w).bits.uop.is_sc  &&
                !IsKilledByBranch(io.core.brupdate, mem_stq_incoming_e(w).bits.uop)
            )
        } .elsewhen(fired_std_incoming(w)) {
            clr_bsy_valid(w) := (
                mem_stq_incoming_e(w).valid            &&
                mem_stq_incoming_e(w).bits.addr.valid  &&
                !mem_stq_incoming_e(w).bits.xcpt_valid &&
                !mem_stq_incoming_e(w).bits.uop.is_sc  &&
                !IsKilledByBranch(io.core.brupdate, mem_stq_incoming_e(w).bits.uop)
            )
        }

        io.core.clr_bsy(w).valid := (
            clr_bsy_valid(w) &&
            !IsKilledByBranch(io.core.brupdate, clr_bsy_brmask(w)) &&
            !io.core.exception && !RegNext(io.core.exception) && !RegNext(RegNext(io.core.exception))
        )
        io.core.clr_bsy(w).bits := clr_bsy_rob_idx(w)
    }
// -----------------------------------------------------------------------
// s3 stage: handle decache responses and nacks
    for (w <- 0 until memWidth) {
        io.core.exe(w).iresp.valid := false.B
    }

    val dmem_resp_fired = WireInit(widthMap(w => false.B))
    val ld_forward_success = WireInit(0.U.asTypeOf(Vec(memWidth, Bool())))
    for (w <- 0 until memWidth) {
        // handle nacks
        when (dcache.io.lsu.nack(w).valid) {
            when (dcache.io.lsu.nack(w).bits.uop.use_ldq) {
                assert(ldq(dcache.io.lsu.nack(w).bits.uop.ldqIdx).bits.executed)

                ldq(dcache.io.lsu.nack(w).bits.uop.ldqIdx).bits.executed := false.B
            } .otherwise {
                when (
                    IsOlder(dcache.io.lsu.nack(w).bits.uop.stqIdx, stq_execute_head, stq_head) ||
                    dcache.io.lsu.nack(w).bits.uop.stqIdx === stq_execute_head
                ) {
                    stq_execute_head := dcache.io.lsu.nack(w).bits.uop.stqIdx
                }
            }
        }
        // handle the response
        when (dcache.io.lsu.resp(w).valid) {
            when (dcache.io.lsu.resp(w).bits.uop.use_ldq) {
                val ldq_idx = dcache.io.lsu.resp(w).bits.uop.ldqIdx

                /* io.core.exe(w).iresp.bits.uop := ldq(ldq_idx).bits.uop */
                io.core.exe(w).iresp.bits.uop := dcache.io.lsu.resp(w).bits.uop
                if (!FPGAPlatform) {
                    io.core.exe(w).iresp.bits.uop.debug_load_uncacheable := ldq(ldq_idx).bits.is_uncacheable
                }
                /* io.core.exe(w).iresp.valid := ldq(ldq_idx).bits.uop.dst_rtype === RT_FIX */
                io.core.exe(w).iresp.valid     := dcache.io.lsu.resp(w).bits.uop.dst_rtype === RT_FIX
                io.core.exe(w).iresp.bits.data := dcache.io.lsu.resp(w).bits.data

                dmem_resp_fired(w) := true.B

                ldq(ldq_idx).bits.succeeded := io.core.exe(w).iresp.valid
            } .otherwise {
                assert(dcache.io.lsu.resp(w).bits.uop.use_stq)

                stq(dcache.io.lsu.resp(w).bits.uop.stqIdx).bits.succeeded := true.B
                when (dcache.io.lsu.resp(w).bits.uop.is_sc) {
                    dmem_resp_fired(w) := true.B
                    io.core.exe(w).iresp.valid     := true.B
                    /* io.core.exe(w).iresp.bits.uop  := stq(dcache.io.lsu.resp(w).bits.uop.stqIdx).bits.uop */
                    io.core.exe(w).iresp.bits.uop  := dcache.io.lsu.resp(w).bits.uop
                    io.core.exe(w).iresp.bits.data := dcache.io.lsu.resp(w).bits.data
                }
            }
        }

        /* when(dmem_resp_fired(w) && wb_forward_valid(w)) {
            // possible because if fire_load_incoming could do wb_forward_valid 
            // and it dont see dcache fire 
        } .else */
        when (!dmem_resp_fired(w) && wb_forward_valid(w)) {
            val f_idx       = wb_forward_ldq_idx(w)
            val forward_uop = ldq(f_idx).bits.uop
            val stq_e       = stq(wb_forward_stq_idx(w))
            /* val addr_match  = stq_e.bits.addr.bits(xLen - 1, 20) === wb_forward_ld_addr(w)(xLen - 1, 20) */
            val addr_match  = IsEqual(wb_forward_st_addr(w), wb_forward_ld_addr(w)(xLen - 1, 20))
            val data_ready  = stq_e.bits.data.valid
            val live        = !IsKilledByBranch(io.core.brupdate, forward_uop)
            val storegen    = storeDataGen(
                stq_e.bits.addr.bits(1,0),
                stq_e.bits.data.bits,
                stq_e.bits.uop.mem_size
            )
            val loadgen     = loadDataGen(
                wb_forward_ld_addr(w)(1,0),
                storegen,
                forward_uop.mem_size,
                forward_uop.mem_signed
            )

            ld_forward_success(w)          := addr_match && data_ready && live
            // io.core.exe(w).iresp.valid     := /* (forward_uop.dst_rtype === RT_FIX) && */ data_ready && live
            io.core.exe(w).iresp.valid     := ld_forward_success(w)
            io.core.exe(w).iresp.bits.uop  := forward_uop
            io.core.exe(w).iresp.bits.data := loadgen

            // when (data_ready && live) {
            when (ld_forward_success(w)) {
                ldq(f_idx).bits.succeeded       := data_ready
                ldq(f_idx).bits.forward_std_val := true.B
                ldq(f_idx).bits.forward_stq_idx := wb_forward_stq_idx(w)
            }
        }
    }
// -----------------------------------------------------------------------
// s2 stage: handle speculative load wakeup failure
    io.core.ld_miss := RegNext(io.core.spec_ld_wakeup.map(_.valid).reduce(_||_))
    val spec_ld_succeed = widthMap(w =>
        !RegNext(io.core.spec_ld_wakeup(w).valid) ||
        // (io.core.exe(w).iresp.valid && io.core.exe(w).iresp.bits.uop.ldqIdx === RegNext(mem_incoming_uop(w).ldqIdx))
        (
            /* (   // case 1: from forwarding
                ld_forward_success(w) &&
                (wb_forward_valid(w) && wb_forward_ldq_idx(w) === RegNext(mem_incoming_uop(w).ldqIdx))
            )
            || */
            (   // case 2: from dcache response
                /* RegNext */
                RegNext(fired_load_incoming(w) && !dcache.io.lsu.s1_kill(w)) &&
                dcache.io.lsu.s2_hit(w)
            )
        )
    ).reduce(_ && _)
    when (spec_ld_succeed) {
        io.core.ld_miss := false.B
    }
// -----------------------------------------------------------------------
// handle branch update
    val st_brkilled_mask = Wire(Vec(numStqEntries, Bool()))
    for (i <- 0 until numStqEntries) {
        st_brkilled_mask(i) := false.B

        when (stq(i).valid) {
            stq(i).bits.uop.brMask := GetNewBrMask(io.core.brupdate, stq(i).bits.uop.brMask)

            when (IsKilledByBranch(io.core.brupdate, stq(i).bits.uop)) {
                stq(i).valid           := false.B
                stq(i).bits.addr.valid := false.B
                stq(i).bits.data.valid := false.B

                st_brkilled_mask(i) := true.B
            }
        }

        assert(!(IsKilledByBranch(io.core.brupdate, stq(i).bits.uop) && stq(i).valid && stq(i).bits.committed),
            "Branch is trying to clear a committed store.")
    }

    for (i <- 0 until numLdqEntries) {
        when (ldq(i).valid) {
            ldq(i).bits.uop.brMask := GetNewBrMask(io.core.brupdate, ldq(i).bits.uop.brMask)
            when (IsKilledByBranch(io.core.brupdate, ldq(i).bits.uop)) {
                ldq(i).valid           := false.B
                ldq(i).bits.addr.valid := false.B
            }
        }
    }

    when(io.core.brupdate.b2.mispredict && !io.core.exception) {
        stq_tail := io.core.brupdate.b2.uop.stqIdx
        ldq_tail := io.core.brupdate.b2.uop.ldqIdx
    }
// -----------------------------------------------------------------------
// handle load/store commit
    var temp_stq_commit_head = stq_commit_head
    var temp_ldq_head = ldq_head
    for (w <- 0 until coreWidth) {
        val commit_store = io.core.commit.valids(w) && io.core.commit.uops(w).use_stq
        val commit_load = io.core.commit.valids(w) && io.core.commit.uops(w).use_ldq
        val idx = Mux(commit_store, temp_stq_commit_head, temp_ldq_head)
        when (commit_store) {
            stq(idx).bits.committed := true.B
        } .elsewhen(commit_load) {
            assert(ldq(idx).valid, "[lsu] trying to commit an un-allocated load entry.")
            assert((ldq(idx).bits.executed || ldq(idx).bits.forward_std_val) && ldq(idx).bits.succeeded,
                "[lsu] trying to commit an un-executed load entry.")

            ldq(idx).valid                := false.B
            ldq(idx).bits.addr.valid      := false.B
            ldq(idx).bits.executed        := false.B
            ldq(idx).bits.succeeded       := false.B
            ldq(idx).bits.order_fail      := false.B
            ldq(idx).bits.forward_std_val := false.B
        }

        temp_stq_commit_head = Mux(commit_store, WrapInc(temp_stq_commit_head, numStqEntries),
                                                 temp_stq_commit_head)
        temp_ldq_head = Mux(commit_load, WrapInc(temp_ldq_head, numLdqEntries),
                                         temp_ldq_head)
    }
    stq_commit_head := temp_stq_commit_head
    ldq_head        := temp_ldq_head
// -----------------------------------------------------------------------
    // try to finish a store instruction
    when (stq(stq_head).valid && stq(stq_head).bits.committed) {
        finish_store := stq(stq_head).bits.succeeded
    }

    when (finish_store) {
        stq(stq_head).valid           := false.B
        stq(stq_head).bits.addr.valid := false.B
        stq(stq_head).bits.data.valid := false.B
        stq(stq_head).bits.succeeded  := false.B
        stq(stq_head).bits.committed  := false.B

        stq_head := WrapInc(stq_head, numStqEntries)
    }

//-------------------------------------------------------------
// handle exception and reset
    // for the live_store_mask, need to kill stores that haven't been committed
    val st_exc_killed_mask = WireInit(VecInit((0 until numStqEntries).map(x => false.B)))
    when (reset.asBool || io.core.exception) {
        ldq_head := 0.U
        ldq_tail := 0.U
        for (i <- 0 until numLdqEntries) {
            ldq(i).valid := false.B
            ldq(i).bits.addr.valid := false.B
            ldq(i).bits.executed := false.B
        }

        when (reset.asBool) {
            stq_head := 0.U
            stq_tail := 0.U
            stq_commit_head := 0.U
            stq_execute_head := 0.U

            for (i <- 0 until numStqEntries) {
                stq(i).valid           := false.B
                stq(i).bits.addr.valid := false.B
                stq(i).bits.data.valid := false.B
            }
        } .otherwise { // exception
            stq_tail := stq_commit_head

            for (i <- 0 until numStqEntries) {
                when (!stq(i).bits.committed) {
                    stq(i).valid           := false.B
                    stq(i).bits.addr.valid := false.B
                    stq(i).bits.data.valid := false.B
                    st_exc_killed_mask(i)  := true.B
                }
            }
        }
    }
// -----------------------------------------------------------------------
// handle live store mask
    live_store_mask := next_live_store_mask & ~(st_brkilled_mask.asUInt) & ~(st_exc_killed_mask.asUInt)
}
