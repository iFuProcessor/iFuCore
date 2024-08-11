package iFu.axi3

import chisel3._
import chisel3.util._

object AXI3Parameters {
    val idBits    = 4
    val addrBits  = 32
    val lenBits   = 8
    val sizeBits  = 3
    val burstBits = 2
    val cacheBits = 4
    val protBits  = 3
    val dataBits  = 32
    val respBits  = 2

    def MLEN1   = 0x0.U(lenBits.W)
    def MLEN2   = 0x1.U(lenBits.W)
    def MLEN4   = 0x3.U(lenBits.W)
    def MLEN8   = 0x7.U(lenBits.W)
    def MLEN16  = 0xF.U(lenBits.W) // max supported length
    def MLEN32  = 0x1F.U(lenBits.W)
    def MLEN64  = 0x3F.U(lenBits.W)
    def MLEN128 = 0x7F.U(lenBits.W)
    def MLEN256 = 0xFF.U(lenBits.W)

    def MSIZE1   = 0.U(sizeBits.W)
    def MSIZE2   = 1.U(sizeBits.W)
    def MSIZE4   = 2.U(sizeBits.W)  // max supported size
    def MSIZE8   = 3.U(sizeBits.W)
    def MSIZE16  = 4.U(sizeBits.W)
    def MSIZE32  = 5.U(sizeBits.W)
    def MSIZE64  = 6.U(sizeBits.W)
    def MSIZE128 = 7.U(sizeBits.W)

    def BURST_FIXED    = 0.U(burstBits.W)
    def BURST_INCR     = 1.U(burstBits.W)
    def BURST_WRAP     = 2.U(burstBits.W)
    def BURST_RESERVED = 3.U(burstBits.W)
}

trait AXI3ID {
    def idBits = AXI3Parameters.idBits
    val id = Output(UInt(idBits.W))
}

trait AXI3Data {
    def dataBits = AXI3Parameters.dataBits
    val data = Output(UInt(dataBits.W))
}

trait AXI3Last {
    val last = Output(Bool())
}

class AXI3BundleA extends Bundle with AXI3ID {
    val addr  = Output(UInt(AXI3Parameters.addrBits.W))
    val len   = Output(UInt(AXI3Parameters.lenBits.W))
    val size  = Output(UInt(AXI3Parameters.sizeBits.W))
    val burst = Output(UInt(AXI3Parameters.burstBits.W))
    val lock  = Output(Bool())
    val cache = Output(UInt(AXI3Parameters.cacheBits.W))
    val prot  = Output(UInt(AXI3Parameters.protBits.W))
}

class AXI3BundleB extends Bundle with AXI3ID {
    val resp = Output(UInt(AXI3Parameters.respBits.W))
}

class AXI3BundleR extends AXI3BundleB with AXI3ID with AXI3Data with AXI3Last

class AXI3BundleW extends Bundle with AXI3ID with AXI3Data with AXI3Last {
    val strb = Output(UInt((AXI3Parameters.dataBits / 8).W))
}

class AXI3 extends Bundle {
    val ar = Decoupled(new AXI3BundleA)
    val r  = Flipped(Decoupled(new AXI3BundleR))
    val aw = Decoupled(new AXI3BundleA)
    val w  = Decoupled(new AXI3BundleW)
    val b  = Flipped(Decoupled(new AXI3BundleB))
}
