package iFu.frontend

import chisel3._
import chisel3.util._

import iFu.common.FrontendParameters

object FrontendUtils extends FrontendParameters {

    def fetchAlign(addr: UInt): UInt = ~(~addr | (iCacheParams.fetchBytes - 1).U)

    def nextFetch(addr: UInt): UInt = {
        fetchAlign(addr) + fetchBytes.U
    }

    def fetchMask(addr: UInt): UInt = {
        val idx = addr(log2Ceil(fetchBytes) - 1, log2Ceil(instrBytes))
        (((1 << fetchWidth) - 1).U << idx).asUInt(fetchWidth - 1, 0)
    }

    def fetchIdx(addr: UInt): UInt = (addr >> log2Ceil(fetchBytes)).asUInt

    def getPc(pc: UInt, idx: UInt): UInt = Cat(fetchIdx(pc), idx(log2Ceil(fetchWidth) - 1, 0), 0.U(2.W))

}