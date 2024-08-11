package iFu.backend

import chisel3._
import chisel3.util._

import iFu.common._
import iFu.util._

case class BRUType (
    normal: Boolean,
    to_exu: Boolean,
    to_lsu: Boolean
)

class BRUIO(t: BRUType, brinfoWidth: Int) extends CoreBundle {
    val br_infos  = Input(Vec(brinfoWidth, new BrResolutionInfo))
    val rob_flush = Input(Bool())
    val rob_head  = if (t.normal || t.to_lsu) Input(UInt(robParameters.robAddrSz.W)) else null
    val jalr_tgt  = if (t.normal) Input(UInt(vaddrBits.W)) else null

    val br_s1_mispredict = if (t.normal) Output(Bool()) else null
    val mis_br_ftqIdx    = if (t.normal) Output(UInt(log2Ceil(frontendParams.numFTQEntries).W)) else null
    val br_update        = Output(new BrUpdateInfo)
}

class BranchUnit(t: BRUType, brinfoWidth: Int) extends CoreModule {
    val io = IO(new BRUIO(t, brinfoWidth))

// -----------------------------------------------------------------------
    // clear branch mask and kill instructions
    val br_s1 = Reg(new BrUpdateMasks)
    br_s1.resolveMask := io.br_infos.map { i =>
        (i.valid << i.uop.brTag).asUInt
    }.reduce(_ | _)
    br_s1.mispredictMask := io.br_infos.map { i =>
        ((i.valid && !io.rob_flush && i.mispredict) << i.uop.brTag).asUInt
    }.reduce(_ | _)

    if (t.normal) {
        io.br_s1_mispredict := RegNext(
            io.br_infos.map { i =>
                i.valid && !io.rob_flush && i.mispredict
            }.reduce(_ || _)
        )
    }
    io.br_update.b1     := br_s1
// -----------------------------------------------------------------------
    if (t.normal || t.to_lsu) {
        // store branch resolution info for redirect
        val br_infos = Reg(Vec(coreWidth, new BrResolutionInfo))
        br_infos zip io.br_infos map { case (r, i) => {
            r       := i
            r.valid := i.valid && !io.rob_flush
        }}

        val br_s2 = Reg(new BrResolutionInfo)
        // find the oldest mispredicted branch
        var mis_found     = false.B
        var oldest_mis_br = br_infos(0)
        for (br <- br_infos) {
            val mis  = br.valid && br.mispredict
            val prio = (
                !mis_found ||
                mis && IsOlder(br.uop.robIdx, oldest_mis_br.uop.robIdx, io.rob_head)
            )
            mis_found     = mis_found || mis
            oldest_mis_br = Mux(prio, br, oldest_mis_br)
        }
        br_s2.mispredict   := mis_found
        br_s2.cfiType      := (if (t.normal) oldest_mis_br.cfiType else DontCare)
        br_s2.taken        := (if (t.normal) oldest_mis_br.taken else DontCare)
        br_s2.pcSel        := (if (t.normal) oldest_mis_br.pcSel else DontCare)
        br_s2.uop          := UpdateBrMask(io.br_update, oldest_mis_br.uop)
        br_s2.jalrTarget   := (if (t.normal) RegNext(io.jalr_tgt) else DontCare)
        br_s2.targetOffset := (if (t.normal) oldest_mis_br.targetOffset else DontCare)

        if (t.normal) {
            io.mis_br_ftqIdx := oldest_mis_br.uop.ftqIdx
        }
        io.br_update.b2  := br_s2
    } else {
        io.br_update.b2 := DontCare
    }
}
