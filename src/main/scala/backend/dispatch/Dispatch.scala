package iFu.backend

import chisel3._
import chisel3.util._

import iFu.common._
import iFu.common.Consts._

class DispatchIO extends CoreBundle {
    val ren_uops = Vec(coreWidth, Flipped(DecoupledIO(new MicroOp)))
    val dis_uops = MixedVec(
        issueParams.map(ip =>
            Vec(ip.dispatchWidth, DecoupledIO(new MicroOp))
        )
    )
}

abstract class Dispatcher extends CoreModule {
    val io = IO(new DispatchIO)
}

class BasicDispatcher extends Dispatcher {
    issueParams.map(ip => require(ip.dispatchWidth == coreWidth))

    /* // both int issue queue and mem issue queue must be ready
    val ren_readys = io.dis_uops.map(d => VecInit(d.map(_.ready)).asUInt).reduce(_&_)

    for (w <- 0 until coreWidth) {
        io.ren_uops(w).ready := ren_readys(w)
    } */

    val ren_readys = Wire(Vec(issueParams.size, Vec(coreWidth, Bool())))
    for (i <- 0 until issueParams.size) {
        val ip = issueParams(i)
        val iqType_match = io.ren_uops.map(r => (r.bits.iqType & ip.iqType.U).orR)
        val iqType_ready = io.dis_uops(i).map(_.ready)
        ren_readys(i) := VecInit(iqType_match zip iqType_ready map { case (m, r) => m && r })
    }
    for (w <- 0 until coreWidth) {
        io.ren_uops(w).ready := ren_readys.map(_(w)).reduce(_||_) || io.ren_uops(w).bits.xcpt_valid
    }

    for (i <- 0 until issueParams.size) {
        val issueParam = issueParams(i)
        val dis        = io.dis_uops(i)
        for (w <- 0 until coreWidth) {
            dis(w).valid := io.ren_uops(w).valid && ((io.ren_uops(w).bits.iqType & issueParam.iqType.U) =/= 0.U)
            dis(w).bits  := io.ren_uops(w).bits
        }
    }
}
