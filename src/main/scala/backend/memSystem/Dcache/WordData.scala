package iFu.backend

import chisel3._
import chisel3.util._
import scala.annotation.switch

import iFu.common._
import iFu.common.Consts._
import iFu.util._
import ram.SDPRam


class DcacheData extends Module with HasDcacheParameters{
    val io = IO(new CoreBundle{
        // 2读口
        val read     = Vec( memWidth ,new DcacheDataIO)
        // 1写口
        val write    = new DcacheDataIO
    })

    val data = SyncReadMem(nTotalWords, UInt(xLen.W))

    // reset
    val reseting = RegInit(true.B)
    val reset_1vIdx = RegInit(0.U(n1vIdxBits.W))

    when (reseting) {
        when (reset_1vIdx === (nTotalWords - 1).U) {
            reseting := false.B
        }

        // data.write(reset_1vIdx, 0.U)
        reset_1vIdx := reset_1vIdx + 1.U
    }

    // read
    val rvalid  = io.read.map( _.req.valid)
    val rreq    = io.read.map( _.req.bits)
    val ridx1v  = rreq.map(req => Cat(req.idx, req.pos, req.offset))
 
    for (w <- 0 until memWidth) {
        io.read(w).resp := 0.U.asTypeOf(Valid(new DcacheDataResp))
        io.read(w).resp.valid := RegNext(rvalid(w))
        val rdata = data.read(ridx1v(w))
        if(!FPGAPlatform)dontTouch(rdata)
        io.read(w).resp.bits.data := rdata
    }

    // write
    val wvalid  = io.write.req.valid
    val wreq    = io.write.req.bits
    val widx1v  = Cat(wreq.idx, wreq.pos, wreq.offset)

    io.write.resp := 0.U.asTypeOf(Valid(new DcacheDataResp))
    
    when (wvalid) {
        data.write(widx1v, wreq.data)
    }
    io.write.resp.valid := RegNext(wvalid)
    io.write.resp.bits.data := DontCare

    // bypass
    val bypass = Wire(Vec(memWidth, Bool()))
    if(!FPGAPlatform)dontTouch(bypass)
    for (w <- 0 until memWidth) {
        //下周期判断转发
        bypass(w) := RegNext(rvalid(w)) && RegNext(wvalid) && IsEqual( RegNext(ridx1v(w)) , RegNext(widx1v))
        when ((bypass(w))) {
            io.read(w).resp.bits.data := RegNext(wreq.data)
        }
    }

}