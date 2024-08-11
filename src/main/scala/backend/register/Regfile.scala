package iFu.backend

import chisel3._
import chisel3.util._

import scala.collection.mutable.ArrayBuffer

import iFu.common._

class RegisterFileReadPortIO (
    val addrWidth: Int, val dataWidth: Int
) extends CoreBundle {
    val addr = Input(UInt(addrWidth.W))
    val data = Output(UInt(dataWidth.W))
}

class RegisterFileWritePort (
    val addrWidth: Int, val dataWidth: Int
) extends CoreBundle {
    val addr = UInt(addrWidth.W)
    val data = UInt(dataWidth.W)
}

object WritePort {
    def apply(
        enq: DecoupledIO[ExeUnitResp], addrWidth: Int, dataWidth: Int, rtype: UInt
    ): Valid[RegisterFileWritePort] = {
        val wport = Wire(Valid(new RegisterFileWritePort(addrWidth, dataWidth)))

        enq.ready       := true.B
        wport.valid     := enq.valid && enq.bits.uop.dst_rtype === rtype
        wport.bits.addr := enq.bits.uop.pdst
        wport.bits.data := enq.bits.data
        wport
    }
}

abstract class RegisterFile (
    numRegisters: Int,
    numReadPorts: Int,
    numWritePorts: Int,
    registerWidth: Int,
    bypassableArray: Seq[Boolean]
) extends CoreModule {
    val io = IO(new CoreBundle{
        val read_ports  = Vec(
            numReadPorts, new RegisterFileReadPortIO(pregSz, registerWidth)
        )
        val write_ports = Flipped(Vec(
            numWritePorts, Valid(new RegisterFileWritePort(pregSz, registerWidth))
        ))
    })
}

class RegisterFileSynthesizable (
    numRegisters : Int,
    numReadPorts : Int,
    numWritePorts: Int,
    registerWidth: Int,
    bypassableArray: Seq[Boolean]
) extends RegisterFile(
    numRegisters,
    numReadPorts,
    numWritePorts,
    registerWidth,
    bypassableArray
) {
    val regfile = Mem(numRegisters,UInt(registerWidth.W))

    val read_addrs = io.read_ports.map(p => RegNext(p.addr)) // delay 1 cycle
    val read_data  = Wire(Vec(numReadPorts,UInt(registerWidth.W)))

    for (i <- 0 until numReadPorts) {
        read_data(i) := regfile(read_addrs(i))
    }

    if (bypassableArray.reduce(_||_)) {
        val bypassableWports = ArrayBuffer[Valid[RegisterFileWritePort]]()
        io.write_ports zip bypassableArray map {
            case (wport,b) => if (b) { bypassableWports += wport }
        }

        for ( i <- 0 until numReadPorts){
            val bypassEns = bypassableWports.map(
                x => x.valid && x.bits.addr === read_addrs(i)
            )
            val bypassData = Mux1H(
                VecInit(bypassEns.toSeq),
                VecInit(bypassableWports.map(_.bits.data).toSeq)
            )

            io.read_ports(i).data := Mux(bypassEns.reduce(_|_), bypassData, read_data(i))
        }
    } else {
        for (i <- 0 until numReadPorts) {
            io.read_ports(i).data := read_data(i)
        }
    }

    for (wport <- io.write_ports) {
        when (wport.valid) {
            regfile(wport.bits.addr) := wport.bits.data
        }
    }
}
