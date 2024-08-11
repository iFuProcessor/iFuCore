package iFu.backend

import chisel3._
import chisel3.util._

import iFu.common._

class BusyTableResp extends Bundle {
    val prs1_busy = Bool()
    val prs2_busy = Bool()
}

class BusyTable (
    val plWidth : Int,
    val numPregs: Int,
    val numWakeupPorts: Int
) extends CoreModule {
    val pregSize = log2Ceil(numPregs)

    val io = IO(new Bundle {
        val ren_uops = Input(Vec(plWidth, new MicroOp))
        val busy_resps = Output(Vec(plWidth, new BusyTableResp))
        val rebusy_reqs = Input(Vec(plWidth, Bool()))

        val wakeup_valids = Input(Vec(numWakeupPorts, Bool()))
        val wakeup_pdsts = Input(Vec(numWakeupPorts, UInt(pregSize.W)))
    })

    val busyTable = RegInit(0.U(numPregs.W))

    //将写回的寄存器置为非忙
    val busyTableWakeup = busyTable &
        ~(io.wakeup_pdsts zip io.wakeup_valids).map {
            case (pdst, valid) => valid.asUInt << pdst
        }.reduce(_|_)

    //将新分配的寄存器置为忙
    val busyTableNext = busyTableWakeup |
        (io.ren_uops zip io.rebusy_reqs) .map {
            case (uop, req) => req.asUInt << uop.pdst
        }.reduce(_|_)

    //更新busytable
    busyTable := busyTableNext

    //输出
    //这里我们将不考虑转发，转发逻辑在外面顶层模块进行
    for (i <- 0 until plWidth){
        io.busy_resps(i).prs1_busy := busyTable(io.ren_uops(i).prs1)
        io.busy_resps(i).prs2_busy := busyTable(io.ren_uops(i).prs2)
    }
}