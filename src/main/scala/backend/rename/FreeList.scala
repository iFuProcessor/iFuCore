package iFu.backend

import chisel3._
import chisel3.util._
import iFu.common._
import iFu.util._

class FreeList extends CoreModule {
    val pregSize = log2Ceil(numPRegs)

    val io = IO(new Bundle {
        val reqs = Input(Vec(coreWidth, Bool()))
        val alloc_pregs = Vec(coreWidth, Valid(UInt(pregSize.W)))

        // from ROB
        val dealloc_pregs = Input(Vec(coreWidth, Valid(UInt(pregSize.W))))

        val ren_br_tags = Input(Vec(coreWidth, Valid(UInt(brTagSz.W))))

        val brupdate = Input(new BrUpdateInfo)
    })

    // all free except x0
    val freeList = RegInit(UInt(numPRegs.W), (~1.U(numPRegs.W)).asUInt)
    val allocsAfterBr = Reg(Vec(maxBrCount, UInt(numPRegs.W)))

    // allocate
    val selPregs = Wire(Vec(coreWidth, UInt(numPRegs.W)))
    val selPregsValid = VecInit(selPregs.map(_.orR))
    var mask = freeList
    for (i <- 0 until coreWidth) {
        selPregs(i) := PriorityEncoderOH(mask)
        mask = mask & (~selPregs(i)).asUInt
    }

    // maintain stored allocated regs
    val regValids = Seq.fill(coreWidth) {RegInit(false.B)}
    val regIndices = Seq.fill(coreWidth) {Reg(UInt(pregSize.W))}
    // if selected reg is valid, always valid, otherwise may be used by req
    regValids zip selPregsValid zip io.reqs foreach { case ((regValid, selValid), req) =>
        regValid := selValid || (regValid && !req)
    }
    // whether should fill in new regs
    val selPregFire = VecInit(selPregsValid zip regValids zip io.reqs map { case ((selValid, regValid), req) =>
        (!regValid || req) && selValid
    })
    regIndices zip selPregs zip selPregFire map { case ((regNum, selPreg), fire) =>
        when (fire) {
            regNum := OHToUInt(selPreg)
        }
    }

    io.alloc_pregs zip regValids zip regIndices foreach { case ((io, valid), num) =>
        io.valid := valid
        io.bits := num
    }

    val allocOHs = regIndices map {UIntToOH(_)}
    val allocMasks = (allocOHs zip io.reqs).scanRight(0.U(numPRegs.W)) { case ((alloc, req), mask) =>
        mask | Mux(req, alloc, 0.U)
    }

    val selMask = ((selPregs zip selPregFire) map { case (reg, fire) => Mux(fire, reg, 0.U) }).reduce(_|_)
    // free pregs in mispredicted branch
    val brDeallocs = Mux(io.brupdate.b2.mispredict, allocsAfterBr(io.brupdate.b2.uop.brTag), 0.U)
    // pregs to free from rob
    val deallocMask =
        (io.dealloc_pregs map { de => Mux(de.valid, UIntToOH(de.bits)(numPRegs - 1, 0), 0.U) }).reduce(_|_) | brDeallocs

    val brTagValids = VecInit(io.ren_br_tags map (_.valid)).asUInt

    for (i <- 0 until maxBrCount) {
        val updateList = VecInit(io.ren_br_tags.map(_.bits === i.U)).asUInt & brTagValids
        allocsAfterBr(i) := Mux(
            updateList.orR,
            Mux1H(updateList, allocMasks.slice(1, coreWidth + 1)),
            (allocsAfterBr(i) & (~brDeallocs).asUInt) | allocMasks.head
        )
    }

    freeList := (freeList & (~selMask).asUInt | deallocMask) & (~1.U(numPRegs.W)).asUInt

}
