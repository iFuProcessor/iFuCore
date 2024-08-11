package iFu.common

import chisel3._
import chisel3.util._

import iFu.common.Consts._
import iFu.frontend.{HasBPUParameters}

class BPUParameters {
    val numRasEntries: Int = 8
}

class ICacheParameters(fetchWidth: Int) {
    val nSets: Int        = 64
    val nWays: Int        = 8
    val lineBytes:Int     = 64
    val indexBits: Int    = log2Ceil(nSets)
    val offsetBits : Int  = log2Ceil(lineBytes)
    val untagBits: Int    = indexBits + offsetBits
    val tagBits: Int      = 32 - untagBits
    val fetchBytes        = fetchWidth * 4
    val fetchesPerLine    = lineBytes / fetchBytes

    require(isPow2(fetchesPerLine))
}

class FrontendParameters{
    val instrBytes: Int                = 4
    val fetchWidth: Int                = 4
    val fetchBytes: Int                = fetchWidth * instrBytes
    // val numFTQEntries: Int             = 40
    val numFTQEntries: Int             = 16
    // val numFetchBufferEntries: Int     = 32
    val numFetchBufferEntries: Int     = 9
    val iCacheParams: ICacheParameters = new ICacheParameters(fetchWidth)
    val bpdParams: BPUParameters       = new BPUParameters

    val bpuParams = new HasBPUParameters {}
    val targetSz = bpuParams.targetSz
    def getTargetPC(pc: UInt , target : UInt): UInt = {
        bpuParams.getTargetPC(pc, target)
    }
    def getTarget(tgtpc : UInt): UInt = bpuParams.getTarget(tgtpc)

}

class ROBParameters(coreWidth: Int) {
    val numRobEntries: Int = 36
    val retireWidth: Int   = coreWidth
    val numRobRows: Int    = numRobEntries / coreWidth
    val robAddrSz: Int     = log2Ceil(numRobRows) + log2Ceil(coreWidth)
}

class LSUParameters{
    val numSTQEntries: Int = 12
    val numLDQEntries: Int = 12
    val ldqAddrSz    : Int = log2Ceil(numLDQEntries)
    val stqAddrSz    : Int = log2Ceil(numSTQEntries)
}

class DcacheParameters {
    val nRowWords = 16
    def nRowBits = nRowWords * 32
    def nRowBytes = nRowWords * 4
    val nSets = 64
    val nWays = 4

    def nTotalWords    = nSets * nWays * nRowWords

    def n1vIdxBits     = log2Ceil(nTotalWords)

    val nMSHR = 8
    val nTLBEntries = 32
    def nOffsetBits = log2Ceil(nRowBytes)
    def nIdxBits = log2Ceil(nSets)
    def nTagBits = 32 - nOffsetBits - nIdxBits
    def nAgebits = 10
    val coreDataBits = 32
    val vaddrBits = 32
    def nBlockAddrBits = vaddrBits - nOffsetBits

    val memWidth = 2

    val nFirstMSHRs = 2
    val nSecondMSHRs = 3

    def getWordOffset(vaddr: UInt): UInt = vaddr(nOffsetBits - 1, 2)
    def getIdx(vaddr: UInt): UInt = vaddr(nOffsetBits + nIdxBits - 1, nOffsetBits)
    def getTag(vaddr: UInt): UInt = vaddr(vaddrBits - 1, vaddrBits - nTagBits)
    def getBlockAddr(vaddr: UInt): UInt = vaddr(vaddrBits - 1, nOffsetBits)

    def isStore(req : DCacheReq): Bool = req.uop.use_stq

    def isMMIO(req : DCacheReq): Bool = req.is_uncacheable

    def isUncacheable(req : DCacheReq): Bool = req.is_uncacheable

    def isLL(req : DCacheReq) : Bool = req.uop.is_ll

    def isSC(req : DCacheReq) : Bool = req.uop.is_sc

}

case class IssueParams(
    iqType: Int,        // issue queue type: IQT_INT, IQT_MEM
    numIssueSlots: Int, // number of issue slots
    dispatchWidth: Int,
    issueWidth: Int    // maximum number of uops issued per cycle
)

trait HasCoreParameters {
    val resetPC: Int = 0x1c000000
    val xLen: Int = 32
    val vaddrBits: Int = xLen
    val paddrBits: Int = xLen
    val coreInstrBytes: Int = 4
    val coreInstrBits: Int = coreInstrBytes * 8
    val coreWidth = 3
    val memWidth = 2
    val maxBrCount: Int = 8
    val brTagSz: Int = log2Ceil(maxBrCount)
    val numLRegs: Int = 32
    val lregSz: Int = log2Ceil(numLRegs)
    // val numPRegs: Int = 108
    val numPRegs: Int = 63
    val pregSz: Int = log2Ceil(numPRegs)
    val frontendParams: FrontendParameters = new FrontendParameters
    val robParameters: ROBParameters = new ROBParameters(coreWidth)
    val lsuParameters: LSUParameters = new LSUParameters
    val dcacheParameters: DcacheParameters = new DcacheParameters
    val issueParams: Seq[IssueParams] = Seq(
        IssueParams(issueWidth = 2, numIssueSlots =  6, iqType = IQT_MEM.litValue.toInt, dispatchWidth = 3),
        IssueParams(issueWidth = 3, numIssueSlots = 10, iqType = IQT_INT.litValue.toInt, dispatchWidth = 3)
    )
}
