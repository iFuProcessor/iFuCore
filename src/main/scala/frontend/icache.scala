package iFu.frontend

import chisel3._
import chisel3.util._
import iFu.axi3._
import iFu.common._
import iFu.frontend.FrontendUtils._
import iFu.sma._
import ram.SDPRam

class ICacheReq extends CoreBundle {
    val addr = UInt(vaddrBits.W)
}

class ICacheResp extends CoreBundle {
    val data = UInt((frontendParams.fetchBytes*8).W)
}

class ICache(val iParams : ICacheParameters) extends CoreModule {
    val io = IO(new Bundle{
        val req = Flipped(Decoupled(new ICacheReq))
        val s1_paddr = Input(UInt(paddrBits.W))

        val s1_kill = Input(Bool())
        val s2_kill = Input(Bool())

        val resp = Valid(new ICacheResp)
        val invalidate = Input(Bool())

        val smar = new SMAR
    })
    /*---------------------------------------------------------------------*/
    //========== ----i$ params--- ==========
    val fetchesPerLine = iParams.fetchesPerLine
    val lineBytes      = iParams.lineBytes
    val smaBytes       = io.smar.resp.rdata.getWidth / 8
    require(
        iParams.lineBytes % smaBytes == 0,
        "LineBytes must be divisible by data width."
    )

    val packetBits = frontendParams.fetchBytes * 8
    //========== ----i$ params--- ==========
    /*---------------------------------------------------------------------*/
    //========== ----i$ funcs---- ==========
    def getTag(addr: UInt) = addr(iParams.tagBits + iParams.untagBits - 1, iParams.untagBits)
    def getSet(addr: UInt) = addr(iParams.untagBits - 1, iParams.untagBits - iParams.indexBits)
    //========== ----i$ funcs---- ==========
    /*---------------------------------------------------------------------*/
    //========== ----i$ body----- ==========
    val s_Normal :: s_Fetch :: Nil = Enum(2)
    val iCacheState = RegInit(s_Normal)

    val s2_miss     = Wire(Bool())
    /* val replWay     = LFSR(16, s2_miss)(log2Ceil(iParams.nWays) - 1, 0) */
    val repls       = (0 until iParams.nSets) map { i =>
        val repl = Module(new PseudoLRU(iParams.nWays))
        repl.io
    }
    val replWays    = VecInit(repls map { _.repl_way })

    val validArray  = RegInit(0.U((iParams.nSets * iParams.nWays).W))
    val tagArray    = SyncReadMem(
        iParams.nSets,
        Vec(iParams.nWays, UInt(iParams.tagBits.W))
    )
    val dataArray   = Module(new SDPRam(iParams.nSets * iParams.nWays * fetchesPerLine, UInt(packetBits.W)))
    //========== ----i$ body----- ==========
    /*---------------------------------------------------------------------*/
    //========== ----S0 Stage---- ==========
    val s0_valid = io.req.fire
    val s0_vaddr = io.req.bits.addr
    //========== ----S0 Stage---- ==========
    /*---------------------------------------------------------------------*/
    //========== S0 - S1 Register ==========
    val s1_valid    = RegNext(s0_valid)
    val s1_idx      = RegNext(getSet(s0_vaddr))
    val s1_fetchIdx = RegNext(s0_vaddr(iParams.offsetBits - 1, log2Ceil(fetchBytes)))
    val tagReadData = tagArray.read(getSet(s0_vaddr))
    //========== S0 - S1 Register ==========
    /*---------------------------------------------------------------------*/
    //========== ----S1 Stage---- ==========
    val s1_tagHit = Wire(Vec(iParams.nWays, Bool()))
    for (i <- 0 until iParams.nWays) {
        val s1_tag      = getTag(io.s1_paddr)
        val s1_validBit = validArray(Cat(i.U(log2Ceil(iParams.nWays).W), s1_idx))
        val tag         = tagReadData(i)

        s1_tagHit(i) := s1_validBit && tag === s1_tag
    }
    val s1_hit     = s1_tagHit.asUInt.orR
    val s1_hit_pos = OHToUInt(s1_tagHit)
    //========== ----S1 Stage---- ==========
    /*---------------------------------------------------------------------*/
    //========== S1 - S2 Register ==========
    val s2_valid   = RegNext(s1_valid && !io.s1_kill)
    val s2_idx     = RegNext(s1_idx)
    val s2_hit     = RegNext(s1_hit)
    val s2_hit_pos = RegNext(s1_hit_pos)
    dataArray.io.raddr := Cat(s1_idx, s1_hit_pos, s1_fetchIdx)
    val s2_data    = dataArray.io.rdata.head
    //========== S1 - S2 Register ==========
    /*---------------------------------------------------------------------*/
    //========== ----S2 Stage---- ==========
    s2_miss := s2_valid && !s2_hit && (iCacheState === s_Normal)

    repls.zipWithIndex.foreach { case (repl, i) =>
        repl.access.valid := (
            (iCacheState === s_Normal) &&
                (i.U === s2_idx)           &&
                s2_valid                   &&
                s2_hit
            )
        repl.access.bits  := s2_hit_pos
    }
    //========== ----S2 Stage---- ==========
    /*---------------------------------------------------------------------*/
    //========== --inv i$ logic-- ==========
    val invalidated = Reg(Bool())
    when(io.invalidate) {
        validArray := 0.U
        invalidated := true.B
    }
    when (iCacheState =/= s_Fetch) {
        invalidated := false.B
    }
    //========== --inv i$ logic-- ==========
    //-----------------------------------------------------------------------
    //========== --Refill Logic-- ==========
    val refillPaddr = RegEnable(
        io.s1_paddr,
        s1_valid && !(s2_miss || iCacheState === s_Fetch)
    )
    val refillTag = getTag(refillPaddr)
    val refillIdx = getSet(refillPaddr)

    var refillEn: Bool = null
    var refillData: UInt = null
    var refillLast: Bool = null
    val needBuffer = smaBytes != fetchBytes
    if (!needBuffer) {
        refillEn   = io.smar.req.arvalid && io.smar.resp.rvalid
        refillData = io.smar.resp.rdata
        refillLast = io.smar.resp.rvalid && io.smar.resp.rlast
    } else {
        val refillBufWidth = fetchBytes / smaBytes
        val refillBufCnt   = RegInit(0.U(log2Ceil(refillBufWidth).W))
        val refillBuf      = RegInit(
            VecInit(Seq.fill(refillBufWidth)(0.U((smaBytes * 8).W)))
        )

        val refillBufWriteEn = io.smar.req.arvalid && io.smar.resp.rvalid
        when (refillBufWriteEn) {
            refillBufCnt            := refillBufCnt + 1.U
            refillBuf(refillBufCnt) := io.smar.resp.rdata
        }

        refillEn   = RegNext(refillBufWriteEn) && refillBufCnt === 0.U
        refillData = refillBuf.asUInt
        refillLast = RegNext(io.smar.resp.rvalid && io.smar.resp.rlast)
    }

    val refillCnt = RegInit(0.U(log2Ceil(fetchesPerLine).W))
    when (refillEn) {
        refillCnt := refillCnt + 1.U
    }

    val writeEn = refillEn
    val writeIdx = Cat(refillIdx, replWays(refillIdx), refillCnt)
    dataArray.io.wen := writeEn
    dataArray.io.waddr := writeIdx
    dataArray.io.wdata.head := refillData
    dataArray.io.wstrobe := 1.U

    when (refillLast) {
        tagArray.write(
            refillIdx,
            VecInit(Seq.fill(iParams.nWays)(refillTag)),
            Seq.tabulate(iParams.nWays)(replWays(refillIdx) === _.U)
        )
        validArray := validArray.bitSet(
            Cat(replWays(refillIdx), refillIdx), refillLast && !invalidated
        )
    }

    //========== --Refill Logic-- ==========
    /*---------------------------------------------------------------------*/
    //========== ----- FSM  ----- ==========
    when (iCacheState === s_Normal) {
        iCacheState := Mux(s2_miss && !io.s2_kill, s_Fetch, s_Normal)
    } .elsewhen (iCacheState === s_Fetch) {
        iCacheState := Mux(refillLast, s_Normal, s_Fetch)
    } .otherwise {
        iCacheState := iCacheState
    }
    //========== ----- FSM  ----- ==========
    /*---------------------------------------------------------------------*/
    //========== ------ IO ------ ==========
    io.req.ready := iCacheState === s_Normal
    io.resp.valid := s2_valid && s2_hit
    io.resp.bits.data := s2_data

    io.smar.req.arvalid := ((s2_miss && !io.s2_kill) || (iCacheState === s_Fetch)) && !RegNext(io.smar.resp.rlast)
    io.smar.req.arsize := AXI3Parameters.MSIZE4
    io.smar.req.araddr := (refillPaddr >> iParams.offsetBits) << iParams.offsetBits
    io.smar.req.arburst := AXI3Parameters.BURST_WRAP
    io.smar.req.arlen := AXI3Parameters.MLEN16
    require(iParams.lineBytes == 64)
    //========== ------ IO ------ ==========
}
