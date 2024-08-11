package iFu.backend

import chisel3._
import chisel3.util._

import iFu.axi3._
import iFu.sma._

import iFu.common._

class WriteFetchUnit extends Module with HasDcacheParameters {
    val io = IO(new CoreBundle {
        // can wfu accept new request
        val ready = Output(Bool())

        // request from MSHR or fence
        val req_valid   = Input(Bool())
        val req_addr    = Input(UInt(xLen.W))
        val req_wb_only = Input(Bool()) // only write back, no refill

        // meta response from Meta Array
        val meta_resp = Input(new DcacheMetaResp)

        // which way wfu is working on
        val pos = Output(UInt(log2Ceil(nWays).W))

        // finish fetch, ready to replay request in MSHR
        val fetch_ready  = Output(Bool())
        val fetched_addr = Output(UInt(xLen.W))
        // new meta line to be written
        val new_meta = Output(new MetaLine)

        // send to Meta Array and Data Array
        val wfu_read_req  = Output(Valid(new DCacheReq))
        val wfu_read_resp = Input(Valid(new DCacheResp))
        val wfu_write_req = Output(Valid(new DCacheReq))

        // clear a dirty line
        val line_clear_req = Output(Valid(new DCacheReq))

        val smar = new SMAR
        val smaw = new SMAW
    })

    // ready -> no thing to do, fetch -> fetch data from memory, wb -> write back dirty line to memory
    val ready :: fetch :: wb :: Nil = Enum(3)
    val state = RegInit(ready)

    // which position to write when receive data from Data Array
    val receive_head = RegInit(0.U(log2Ceil(nRowWords).W))
    // which position to read when send data to memory
    val execute_head = RegInit(0.U(log2Ceil(nRowWords).W))
    val tail = RegInit(0.U((log2Ceil(nRowWords) + 1).W))//宽度加1位,不然根本无法增大到nRowWords
    val refillIdx = RegInit(0.U(log2Ceil(nRowWords).W))
    val dataLineBuffer = RegInit(0.U.asTypeOf(Vec(nRowWords, UInt(xLen.W))))
    val replaceAddr = RegInit(0.U(xLen.W))
    val fetchAddr = RegInit(0.U(xLen.W))
    val wbOnly = RegInit(false.B)
    val getfirstWord = RegInit(false.B)
    val replaceWay = RegInit(0.U(log2Ceil(nWays).W))

    io.ready := state === ready

    io.pos := replaceWay

    io.fetch_ready  := false.B
    io.fetched_addr := fetchAddr

    io.new_meta.valid    := true.B
    io.new_meta.tag      := getTag(fetchAddr)
    io.new_meta.dirty    := false.B
    io.new_meta.readOnly := false.B
    io.new_meta.fixed    := false.B

    io.wfu_read_req := DontCare
    io.wfu_read_req.valid     := false.B
    io.wfu_read_req.bits.addr := replaceAddr | (tail << 2.U).asUInt

    io.wfu_write_req := DontCare
    io.wfu_write_req.valid     := false.B
    io.wfu_write_req.bits.addr := fetchAddr | (refillIdx << 2.U).asUInt
    io.wfu_write_req.bits.data := io.smar.resp.rdata

    io.line_clear_req := DontCare
    io.line_clear_req.valid     := false.B
    io.line_clear_req.bits.addr := replaceAddr

    io.smar.req.arvalid := state === fetch
    io.smar.req.arlen   := AXI3Parameters.MLEN16
    io.smar.req.arburst := AXI3Parameters.BURST_WRAP
    io.smar.req.arsize  := AXI3Parameters.MSIZE4
    io.smar.req.araddr  := fetchAddr

    io.smaw.req.awvalid := state === wb && getfirstWord
    io.smaw.req.wvalid  := state === wb && getfirstWord
    io.smaw.req.wstrb   := 0xf.U
    io.smaw.req.awlen   := AXI3Parameters.MLEN16
    io.smaw.req.awburst := AXI3Parameters.BURST_WRAP
    io.smaw.req.awsize  := AXI3Parameters.MSIZE4
    io.smaw.req.awaddr  := replaceAddr
    io.smaw.req.wdata   := dataLineBuffer(execute_head)
    io.smaw.req.wlast   := execute_head === 0xf.U

    when (state === ready) {
        when (io.req_valid) {
            replaceWay  := io.meta_resp.pos
            replaceAddr := Cat(io.meta_resp.rmeta.tag, io.meta_resp.idx, 0.U(nOffsetBits.W))
            fetchAddr   := Cat(io.req_addr >> nOffsetBits.U, 0.U(nOffsetBits.W))
            wbOnly      := io.req_wb_only

            // init
            receive_head := 0.U
            execute_head := 0.U
            tail         := 0.U
            refillIdx    := 0.U

            state := Mux(io.meta_resp.rmeta.valid && io.meta_resp.rmeta.dirty, wb, fetch)
        }
    } .elsewhen (state === fetch) {
        when (io.smar.resp.rvalid) {
            state := Mux(io.smar.resp.rlast, ready, fetch)

            // refill Meta Array and Data Array(when first word comes, it will invalidate the old meta)
            io.wfu_write_req.valid := true.B
            refillIdx := refillIdx + 1.U

            when (io.smar.resp.rlast) { // finish fetch
                io.fetch_ready := true.B
            }
        }
    } .elsewhen (state === wb) {
        // send a request to Data Array
        when (tail < nRowWords.U) {
            io.wfu_read_req.valid := true.B
            tail := tail + 1.U
        }
        // receive data from Data Array
        when (io.wfu_read_resp.valid) {
            getfirstWord                 := true.B
            dataLineBuffer(receive_head) := io.wfu_read_resp.bits.data
            receive_head                 := receive_head + 1.U
        }

        // write back to memory
        when (io.smaw.resp.wready) {
            state := Mux(!io.smaw.req.wlast, wb,
                     Mux(wbOnly             , ready,    // do not fetch, only write back
                                              fetch))

            execute_head := execute_head + 1.U

            when (io.smaw.req.wlast) { // finish write back
                when (wbOnly) {
                    // clear a dirty line
                    io.line_clear_req.valid := true.B
                }
                // reset
                getfirstWord := false.B
            }
        }
    }
}
