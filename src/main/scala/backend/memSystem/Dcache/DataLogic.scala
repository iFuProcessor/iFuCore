
package iFu.backend

import chisel3._
import chisel3.util._
import scala.annotation.switch

import iFu.common._
import iFu.common.Consts._
import iFu.util._

class DcacheDataReq extends CoreBundle with HasDcacheParameters {
    val idx      = UInt(nIdxBits.W)
    val offset   = UInt(log2Ceil(nRowWords).W)
    val pos  = UInt(log2Ceil(nWays).W)
    val data    = UInt(xLen.W)

}

class DcacheDataResp extends CoreBundle with HasDcacheParameters {
    val data = UInt(xLen.W)
}

class DcacheDataIO extends CoreBundle with HasDcacheParameters {
    val req  = Input(Valid(new DcacheDataReq))
    val resp = Output(Valid(new DcacheDataResp))
}


class DcacheDataLogic extends Module with HasDcacheParameters{
    val io = IO(new CoreBundle{
        val read = Vec( memWidth, (new DcacheDataIO))
        val write = (new DcacheDataIO)
    })

    // data
    val data = Module(new DcacheData)

    // Read
    for(w <- 0 until memWidth){
        data.io.read(w) <> io.read(w)
    }
    data.io.write <> io.write
}