package iFu.backend

import chisel3._
import chisel3.util._
import iFu.common._

class MaptableReq extends CoreBundle {
    val lrs1 = UInt(lregSz.W)
    val lrs2 = UInt(lregSz.W)
    val ldst = UInt(lregSz.W)
}

class MaptableResp extends CoreBundle {
    val prs1 = UInt(pregSz.W)
    val prs2 = UInt(pregSz.W)
    val stale_pdst = UInt(pregSz.W)
}

class ReMapReq extends CoreBundle {
    val ldst = UInt(lregSz.W)
    val pdst = UInt(pregSz.W)
    val valid = Bool()
}

class MapTable extends CoreModule {

    val io = IO(new CoreBundle{
        val map_reqs = Input(Vec(coreWidth, new MaptableReq))
        val map_resps = Output(Vec(coreWidth, new MaptableResp))

        val remap_reqs = Input(Vec(coreWidth, new ReMapReq))

        val ren_br_tags = Input(Vec(coreWidth, Valid(UInt(brTagSz.W))))

        val brupdate = Input(new BrUpdateInfo)
    })

    val mapTable = RegInit(VecInit(Seq.fill(numLRegs) { 0.U(pregSz.W) }))
    val brShot = Reg(Vec(maxBrCount, Vec(numLRegs, UInt(pregSz.W))))

    val remapTable = io.remap_reqs.scanLeft(mapTable) { case (table, req) =>
        VecInit(table.zipWithIndex map { case (preg, lreg) =>
            if (lreg == 0) {
                0.U
            } else {
                Mux(req.valid && req.ldst === lreg.U, req.pdst, preg)
            }
        })
    }

    // save maps at br for rollback
    io.ren_br_tags zip remapTable.slice(1, coreWidth + 1) foreach { case (tag, table) =>
        when (tag.valid) {
            brShot(tag.bits) := table
        }
    }

    mapTable := Mux(io.brupdate.b2.mispredict, brShot(io.brupdate.b2.uop.brTag), remapTable.last)

    // forwarding is done at top modules
    io.map_resps zip io.map_reqs foreach { case (resp, req) =>
        resp.prs1 := mapTable(req.lrs1)
        resp.prs2 := mapTable(req.lrs2)
        resp.stale_pdst := mapTable(req.ldst)
    }
}
