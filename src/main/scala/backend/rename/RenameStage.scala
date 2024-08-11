package iFu.backend

import chisel3._
import chisel3.util._
import iFu.common.Consts._
import iFu.common._
import iFu.util._

class RenameStage (numRenameWakeupPorts: Int) extends CoreModule {
    val pregSize = log2Ceil(numPRegs)
    val io = IO(new Bundle {
        val ren_stalls = Output(Vec(coreWidth, Bool()))
        val kill = Input(Bool())

        val dec_fire = Input(Vec(coreWidth, Bool())) // will commit state updates
        val dec_uops = Input(Vec(coreWidth, new MicroOp)) //译码级输出的微指令

        val ren2_mask = Output(Vec(coreWidth, Bool())) // mask of valid instructions
        val ren2_uops = Output(Vec(coreWidth, new MicroOp)) //输出的微指令

        val brupdate = Input(new BrUpdateInfo)

        val dis_fire = Input(Vec(coreWidth, Bool())) //派遣级各支完成指令派遣的信号
        val dis_ready = Input(Bool()) //派遣级可以接收数据的ready信号

        val wakeups = Flipped(Vec(numRenameWakeupPorts, Valid(new ExeUnitResp)))
        //从commit阶段输入的信息
        val com_valids = Input(Vec(coreWidth, Bool()))
        val com_uops = Input(Vec(coreWidth, new MicroOp))
        val rbk_valids = Input(Vec(coreWidth, Bool()))
        val rollback = Input(Bool())
    })
    
    def DoBypass(uop:MicroOp, older:Seq[MicroOp], allocReqs:Seq[Bool]): MicroOp = {
        val bypassedUop = Wire(new MicroOp)
        bypassedUop := uop

        // check if bypassing is possible
        val bypassHitRs1 = (older zip allocReqs) map {case (r,a) => a && r.ldst === uop.lrs1}
        val bypassHitRs2 = (older zip allocReqs) map {case (r,a) => a && r.ldst === uop.lrs2}
        val bypassHitDst = (older zip allocReqs) map {case (r,a) => a && r.ldst === uop.ldst}

        // select the data to bypass
        val bypassSelRs1 = PriorityEncoderOH(bypassHitRs1.reverse).reverse
        val bypassSelRs2 = PriorityEncoderOH(bypassHitRs2.reverse).reverse
        val bypassSelDst = PriorityEncoderOH(bypassHitDst.reverse).reverse

        // check if we need to bypass
        val doBypassRs1 = bypassHitRs1.reduce(_||_)
        val doBypassRs2 = bypassHitRs2.reduce(_||_)
        val doBypassDst = bypassHitDst.reduce(_||_)

        val bypassPdsts = older.map(_.pdst)

        when (doBypassRs1) { bypassedUop.prs1 := Mux1H(bypassSelRs1, bypassPdsts) }
        when (doBypassRs2) { bypassedUop.prs2 := Mux1H(bypassSelRs2, bypassPdsts) }
        when (doBypassDst) { bypassedUop.stale_pdst := Mux1H(bypassSelDst, bypassPdsts) }

        bypassedUop.prs1_busy := uop.prs1_busy || doBypassRs1
        bypassedUop.prs2_busy := uop.prs2_busy || doBypassRs2

        bypassedUop
    }
    
    val ren1Fire = Wire(Vec(coreWidth, Bool()))
    val ren1Uops = Wire(Vec(coreWidth, new MicroOp))

    val ren2Fire = io.dis_fire
    val ren2Ready = io.dis_ready
    val ren2Valids = Wire(Vec(coreWidth, Bool()))
    val ren2Uops = Wire(Vec(coreWidth, new MicroOp))
    val ren2AllocReqs = Wire(Vec(coreWidth, Bool()))

    for (w <- 0 until coreWidth) {
        ren1Fire(w) := io.dec_fire(w)
        ren1Uops(w) := io.dec_uops(w)
    }

    for (w <- 0 until coreWidth) {
        val rValid = RegInit(false.B)
        val rUop = Reg(new MicroOp)
        val nextUop = Wire(new MicroOp)

        nextUop := rUop
        //if kill
        when(io.kill) {
            rValid := false.B
        }.elsewhen(ren2Ready) {
            rValid := ren1Fire(w)
            //kill 信号无效，派遣级准备好接受新的微指令时，r_valid 与译码级的输入的指令有效信号(ren1_fire) 保持一致，并将 next_uop 置为 decode 阶段输入的微指令 (ren1_uop)
            nextUop := ren1Uops(w)
        }.otherwise { ////派遣级没准备好接受新的微指令，若前一条指令已经完成派遣，r_valid信号置0，next_uop保持
            rValid := rValid && !ren2Fire(w)
            nextUop := rUop
        }

        rUop := GetNewUopAndBrMask(DoBypass(nextUop, ren2Uops, ren2AllocReqs), io.brupdate)

        ren2Valids(w) := rValid
        ren2Uops(w) := rUop
    }
    io.ren2_mask := ren2Valids

    //实例化模块
    val maptable = Module(new MapTable)

    val freelist = Module(new FreeList)

    val busytable = Module(new BusyTable(
        coreWidth,
        numPRegs,
        numRenameWakeupPorts
    ))

    val ren2BrTags = Wire(Vec(coreWidth, Valid(UInt(brTagSz.W))))

    val comValids = Wire(Vec(coreWidth,Bool()))
    val rbkValids = Wire(Vec(coreWidth,Bool()))

    for (w <- 0 until coreWidth) {
        ren2AllocReqs(w) := ren2Uops(w).ldst_val && ren2Uops(w).dst_rtype === RT_FIX && ren2Fire(w)
        ren2BrTags(w).valid := ren2Fire(w) && ren2Uops(w).allocate_brtag
        ren2BrTags(w).bits  := ren2Uops(w).brTag
        comValids(w) := io.com_uops(w).ldst_val && io.com_uops(w).dst_rtype === RT_FIX && io.com_valids(w)
        rbkValids(w) := io.com_uops(w).ldst_val && io.com_uops(w).dst_rtype === RT_FIX && io.rbk_valids(w)
    }

    //-------------------RenameTable-------------------

    //inputs
    val mapReqs = Wire(Vec(coreWidth, new MaptableReq))
    val remapReqs = Wire(Vec(coreWidth, new ReMapReq))

    for ((((ren1, ren2), com), w) <- (ren1Uops zip ren2Uops zip io.com_uops.reverse).zipWithIndex) {
        mapReqs(w).lrs1 := ren1.lrs1
        mapReqs(w).lrs2 := ren1.lrs2
        mapReqs(w).ldst := ren1.ldst //读stale_pdst

        remapReqs(w).ldst := Mux(io.rollback, com.ldst, ren2.ldst)
        remapReqs(w).pdst := Mux(io.rollback, com.stale_pdst, ren2.pdst)
    }

    ren2AllocReqs zip rbkValids.reverse zip remapReqs map {
        case((a, r), rr) => rr.valid := a || r
    }

    maptable.io.map_reqs := mapReqs
    maptable.io.remap_reqs := remapReqs
    maptable.io.ren_br_tags := ren2BrTags
    maptable.io.brupdate := io.brupdate

    //outputs
    for ((uop, w) <- ren1Uops.zipWithIndex) {
        val mappings = maptable.io.map_resps(w)

        uop.prs1 := mappings.prs1
        uop.prs2 := mappings.prs2
        uop.stale_pdst := mappings.stale_pdst
    }

    //---------------------------freelist------------------------------

    freelist.io.reqs := ren2AllocReqs
    freelist.io.dealloc_pregs zip comValids zip rbkValids map {
        case ((d, c), r) => d.valid := c || r
    }
    freelist.io.dealloc_pregs zip io.com_uops map {
        case(d, c) => d.bits := Mux(io.rollback, c.pdst, c.stale_pdst)
    }
    freelist.io.ren_br_tags := ren2BrTags
    freelist.io.brupdate := io.brupdate

    //outputs
    for ((uop, w) <- ren2Uops.zipWithIndex) {
        val preg = freelist.io.alloc_pregs(w).bits
        uop.pdst := Mux(uop.ldst =/= 0.U, preg, 0.U)
    }

    //---------------------------busytable------------------------------

    busytable.io.ren_uops := ren2Uops
    busytable.io.rebusy_reqs := ren2AllocReqs
    busytable.io.wakeup_valids := io.wakeups.map(_.valid)
    busytable.io.wakeup_pdsts := io.wakeups.map(_.bits.uop.pdst)

    //outputs
    for ((uop, w) <- ren2Uops.zipWithIndex) {
        val busy = busytable.io.busy_resps(w)

        uop.prs1_busy := uop.lrs1_rtype === RT_FIX && busy.prs1_busy
        uop.prs2_busy := uop.lrs2_rtype === RT_FIX && busy.prs2_busy
    }

    //---------------------------outputs------------------------------
    for (w <- 0 until coreWidth) {
        val canAllocate = freelist.io.alloc_pregs(w).valid

        io.ren_stalls(w) := (ren2Uops(w).dst_rtype === RT_FIX) && !canAllocate

        val bypassedUop = Wire(new MicroOp)

        //当w为0时不需要转发 不会有冲突
        if (w > 0) {
            bypassedUop := DoBypass(ren2Uops(w), ren2Uops.slice(0,w), ren2AllocReqs.slice(0,w))
        } else {
            bypassedUop := ren2Uops(w)
        }

        io.ren2_uops(w) := GetNewUopAndBrMask(bypassedUop,io.brupdate)
    }
}
