package iFu.frontend

import chisel3._
import chisel3.util._

trait HasBPUParameters {
    val vaddrBits = 32
    val fetchWidth = 4
    val fetchBytes = fetchWidth * 4

    

    val mixSize = 24
    // def mixHILO(pc: UInt): UInt = Cat(pc(vaddrBits - 1 , mixSize) , pc(mixSize - 1, 0) ^ pc(vaddrBits - 1 , vaddrBits - mixSize))
    def mixHILO(pc: UInt): UInt = pc

    val targetSz = 15

    def getTargetPC(pc: UInt , target : UInt): UInt = {
        Cat(pc(vaddrBits - 1, targetSz + 2) , target(targetSz - 1 , 0) , 0.U(2.W))
    }

    def getTarget(tgtpc : UInt): UInt = tgtpc(targetSz + 2 - 1 , 2)
}

trait HasUbtbParameters extends HasBPUParameters {
    // val nWays = 16
    val nWays = 4
    /* def tagSz = vaddrBits - log2Ceil(fetchBytes) */
    // def tagSz = 8

    // tag视野大小
    val tagView = 11
    // val offsetSz = 6
    def tagSz = tagView - log2Ceil(fetchBytes) + 1
    def getTag(pc: UInt): UInt =  pc(tagView , log2Ceil(fetchBytes))
}

trait HasBimParameters extends HasBPUParameters {
    val nSets = 512
    val nWrBypassEntries = 2

    def bimWrite(v: UInt, taken: Bool): UInt = {
        val oldBimSatTaken  = v === 3.U
        val oldBimSatNtaken = v === 0.U
        Mux(oldBimSatTaken  &&  taken, 3.U,
        Mux(oldBimSatNtaken && !taken, 0.U,
        Mux(taken, v + 1.U, v - 1.U)))
    }
}

trait HasBtbParameters extends HasBPUParameters {
    val nWays        = 2
    // def tagSz        = vaddrBits - log2Ceil(nSets) - log2Ceil(fetchBytes)
    val nSets = 64
    // val lowBitSz = 16

    def nIdxBits = log2Ceil(nSets)
    def getIdx(pc: UInt): UInt = pc(nIdxBits + log2Ceil(fetchBytes) - 1, log2Ceil(fetchBytes))
    val tagView = 16
    def tagSz = tagView - nIdxBits - log2Ceil(fetchBytes) + 1
    def getTag(pc: UInt): UInt = pc(tagView , nIdxBits + log2Ceil(fetchBytes))
}

trait HasLocalHistoryParameters extends HasBPUParameters {
    val localHistoryLength = 13
    val nLHRs = 64
    val nCounters = 8192
    val nLHRBits = log2Ceil(nLHRs)
    val nCounterBits = log2Ceil(nCounters)
    val nCacheCounters = 64
    val nCacheCounterBits = log2Ceil(nCacheCounters)

    def update(v: UInt, taken: Bool): UInt = {
        val extended = Cat(0.U(1.W), v)
        val newCnt = Mux(taken, extended + 1.U, extended - 1.U)
        Mux(newCnt(2), v, newCnt(1, 0))
    }

    def idxHash(pc: UInt, hist: UInt): UInt = {
        hist
    }

    def cacheIdxHash(hist: UInt): UInt = {
        hist(nCacheCounterBits - 1, 0)
    }
}
