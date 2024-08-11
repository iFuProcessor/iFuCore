package iFu.backend

import chisel3._
import chisel3.util._

import iFu.common._
import iFu.common.Consts._
import iFu.util._

object WordWrite {
    def apply(req: DCacheReq, rawData: UInt) = {
        val memSize = req.uop.mem_size
        val wdata = rawData.asTypeOf(Vec(4, UInt(8.W)))
        val lob = req.data(7, 0)
        val loh = req.data(15, 0)
        val low = req.data(31, 0)
        when(memSize === 0.U) {

            when(req.mask === "b0001".U) {
                wdata(0) := lob
            }
              .elsewhen(req.mask === "b0010".U) {
                  wdata(1) := lob
              }
              .elsewhen(req.mask === "b0100".U) {
                  wdata(2) := lob
              }
              .elsewhen(req.mask === "b1000".U) {
                  wdata(3) := lob
              }

        }.elsewhen(memSize === 1.U) {

            when(req.mask === "b0011".U) {
                wdata(0) := loh(7, 0)
                wdata(1) := loh(15, 8)
            }.elsewhen(req.mask === "b1100".U) {
                wdata(2) := loh(7, 0)
                wdata(3) := loh(15, 8)
            }

        }.elsewhen(memSize === 2.U) {
            when(req.mask === "b1111".U) {
                wdata(0) := low(7, 0)
                wdata(1) := low(15, 8)
                wdata(2) := low(23, 16)
                wdata(3) := low(31, 24)
            }
        }
        wdata.asUInt
    }
}
