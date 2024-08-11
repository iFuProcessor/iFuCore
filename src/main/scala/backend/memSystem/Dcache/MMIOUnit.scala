package iFu.backend

import scala.collection.View.Fill

import chisel3._
import chisel3.util._

import  iFu.sma._
import  iFu.axi3._

import iFu.common._
import iFu.common.Consts._
import iFu.lsu.utils._

class MMIOUnit extends Module with HasDcacheParameters {
    val io = IO(new CoreBundle{
        val mmioReq  = Flipped(Decoupled(new DCacheReq))
        // mmioResp is a replay request
        val mmioResp = Decoupled(new DCacheReq)

        val smar = new SMAR
        val smaw = new SMAW
    })
    if (!FPGAPlatform) dontTouch (io)

    val s_ready :: s_fetch :: s_wb :: s_resp :: Nil = Enum(4)
    val state = RegInit(s_ready)

    val mmioReq = RegInit(0.U.asTypeOf(new DCacheReq))
    if (!FPGAPlatform) dontTouch(mmioReq)

    io.mmioReq.ready := state === s_ready

    io.mmioResp.valid     := state === s_resp
    io.mmioResp.bits      := mmioReq
    io.mmioResp.bits.data := loadDataGen(
        mmioReq.addr(1, 0),
        mmioReq.data,
        mmioReq.uop.mem_size,
        mmioReq.uop.mem_signed
    )

    io.smar.req.arvalid     := state === s_fetch
    io.smaw.req.awvalid     := state === s_wb
    io.smaw.req.wvalid      := state === s_wb

    io.smaw.req.wstrb       := mmioReq.mask

    io.smar.req.arlen       := AXI3Parameters.MLEN1
    io.smaw.req.awlen       := AXI3Parameters.MLEN1

    io.smar.req.arburst     := AXI3Parameters.BURST_FIXED
    io.smaw.req.awburst     := AXI3Parameters.BURST_FIXED

    io.smar.req.arsize      := mmioReq.uop.mem_size
    io.smaw.req.awsize      := mmioReq.uop.mem_size

    io.smar.req.araddr      := mmioReq.addr
    io.smaw.req.awaddr      := mmioReq.addr

    io.smaw.req.wdata       :=
        Mux(mmioReq.uop.mem_size === 0.U, Fill(4, mmioReq.data( 7, 0)),
        Mux(mmioReq.uop.mem_size === 1.U, Fill(2, mmioReq.data(15, 0)),
                                          mmioReq.data))

    io.smaw.req.wlast        := state === s_wb

    when (state === s_ready) {
        when (io.mmioReq.fire) {
            state := Mux(isStore(io.mmioReq.bits), s_wb, s_fetch)
            mmioReq := io.mmioReq.bits
        }
    } .elsewhen (state === s_fetch) {
        state := Mux(io.smar.resp.rvalid, s_resp, s_fetch)
        mmioReq.data := io.smar.resp.rdata
    } .elsewhen (state === s_wb){
        if(!FPGAPlatform){
            val debug_lo_byte = mmioReq.data(7, 0)
            val debug_lo_half = mmioReq.data(15, 0)
            dontTouch(debug_lo_byte)
            dontTouch(debug_lo_half)
        }


        state := Mux(io.smaw.resp.wready, s_resp, s_wb)
    } .elsewhen(state === s_resp) {
        state := Mux(io.mmioResp.fire, s_ready, s_resp)
    }
}
