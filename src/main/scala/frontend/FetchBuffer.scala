package iFu.frontend

import chisel3._
import chisel3.util._
import iFu.common.Consts.FPGAPlatform
import iFu.common._
import iFu.util.MaskUpper
import iFu.frontend.FrontendUtils._

class FetchBufferResp() extends CoreBundle {
    val uops = Vec(coreWidth,Valid(new MicroOp))
}

class FetchBuffer extends CoreModule {
    val io = IO(new CoreBundle{
        val clear = Input(Bool())
        val enq = Flipped(Decoupled(new FetchBundle()))     // Input
        val deq = new DecoupledIO(new FetchBufferResp())    // Output
    })
    //------------------------------------
    val numFetchBufferEntries = frontendParams.numFetchBufferEntries
    val fetchWidth = frontendParams.fetchWidth
    //------------------------------------

    val numEnt = numFetchBufferEntries
    require(numEnt % coreWidth == 0, "FetchBuffer size must be divisible by coreWidth")
    val numRow = numEnt / coreWidth     // dequeue 1 row of uops at a time

    val ram = Reg(Vec(numEnt, new MicroOp)) // physical implementation of the buffer
    val lram = Wire(Vec(numRow, Vec(coreWidth, new MicroOp))) // logical implementation of the buffer

    for(i <- 0 until numEnt){
        lram(i / coreWidth)(i % coreWidth) := ram(i)
    }

    val head = RegInit(1.U(numRow.W))   // pointer to the dequeue row
    val tail = RegInit(1.U(numEnt.W))   // pointer to the enqueue position

    val mayFull = RegInit(false.B)  // if enqueueing, set to true
                                    // note: mayFull indicates that there are uops present in the buffer

    // enqueue stage
    def rotateLeft(in : UInt, k : Int) = {
        val n = in.getWidth
        Cat(in(n-k-1,0),in(n-1,n-k))
    }

    val mayHitHead = (1 until frontendParams.fetchWidth).map(   // testing insert 1, 2, ..., fetchWidth uops
        k => VecInit(rotateLeft(tail, k).asBools.zipWithIndex.filter {
            case (bit,idx) => idx % coreWidth == 0  // the head is always aligned to a row boundary, so only check those
        }.map {case (bit,idx) => bit}).asUInt       // get the tail position which need to be check
    ).map(newTail => head & newTail).reduce(_|_).orR    // check if any of the insertions hit the head
    // if not hit head, indicate that there are at least 8 empty slots in the buffer or the buffer is full

    // now we check whether the second case is true
    // if the buffer is full, tail will be equal to head, and mayFull will be true
    val atHead = (
        VecInit(tail.asBools.zipWithIndex.filter {
            case (bit,idx) => idx % coreWidth == 0    // get the bits on position which need to be check
        }.map {case (bit,idx) => bit}).asUInt & head).orR   // check whether the tail is equal to head

    val doEnqueue = !(atHead && mayFull || mayHitHead)  // if the first case is true, we can enqueue

    io.enq.ready := doEnqueue

    val inMask = Wire(Vec(frontendParams.fetchWidth, Bool())) // which uops are valid
    val inUops = Wire(Vec(frontendParams.fetchWidth, new MicroOp()))

    for (i <- 0 until fetchWidth){

        val pc = (fetchAlign(io.enq.bits.pc) + (i << 2).U) | io.enq.bits.pc(1, 0)

        inUops(i) := DontCare   // set the value afterward
        inMask(i) := io.enq.valid && io.enq.bits.mask(i)

        inUops(i).xcpt_valid := io.enq.bits.exception.valid
        inUops(i).xcpt_cause := io.enq.bits.exception.bits.xcpt_cause

        inUops(i).pcLowBits := pc
        inUops(i).ftqIdx    := io.enq.bits.ftqIdx
        inUops(i).instr     := io.enq.bits.instrs(i)
        if (!FPGAPlatform) {
            inUops(i).debug_inst := io.enq.bits.instrs(i)
            inUops(i).debug_pc := pc
        }
        inUops(i).taken := io.enq.bits.cfiIdx.bits.asUInt === i.U && io.enq.bits.cfiIdx.valid
    }

    // the index of the uop which will be enqueued
    // note: the index is one-hot encoded
    val enqIdxOH = Wire(Vec(frontendParams.fetchWidth,UInt(numEnt.W)))

    def inc(ptr: UInt) = {  // the pointer is one-hot encoded, so simply shift it
        val n = ptr.getWidth
        Cat(ptr(n-2,0), ptr(n-1))
    }

    var enqIdx = tail
    for (i <- 0 until frontendParams.fetchWidth){
        enqIdxOH(i) := enqIdx
        enqIdx = Mux(inMask(i), inc(enqIdx), enqIdx) // if the uop is valid, enqueue it
    }

    // enqueue the uops
    for( i <- 0 until frontendParams.fetchWidth){   // for each uop
        for(j <- 0 until numEnt){   // for each entry in the buffer
            when (doEnqueue && inMask(i) && enqIdxOH(i)(j)){
                ram(j) := inUops(i)
            }
        }
    }

    // dequeue stage
    val mayHitTail = VecInit((0 until numEnt).map(
        idx => head(idx / coreWidth) && (!mayFull || (idx % coreWidth != 0).B)
    )).asUInt & tail
    // First, expand the head to the size of the tail by copying each bit of the head four times
    // Then, if shifting the head four times still does not reach the tail, it indicates that the buffer has at least 4 valid uops
    // If the head is equal to the tail, we need to check whether the buffer is full or empty
    // If mayFull is false, it means the buffer was dequeued the last time, and thus the buffer is empty

    val slotWillHitTail = (0 until numRow).map(
        i => mayHitTail((i + 1) * coreWidth - 1, i * coreWidth)
    ).reduce(_|_)   // 4 bits, indicate which slot will hit the tail, if not, equal to 0000
    val willHitTail = slotWillHitTail.orR   // if hit, slotWillHitTail will have one bit set to 1

    val doDequeue = io.deq.ready && !willHitTail

    val deqValid = (~MaskUpper(slotWillHitTail)).asBools    // the positions before the tail are valid


    (io.deq.bits.uops zip deqValid).map { case (d, valid) => d.valid := valid }  // connect the valid signal using the map function
    (io.deq.bits.uops zip Mux1H(head, lram)).map {case (d, uop) => d.bits := uop}
    io.deq.valid := deqValid.reduce(_||_)
    // note: here, we dequeue uops from the buffer if it's not empty, which means there may not be at least 4 uops in the buffer
    // however, there's no need to worry as we use the signal doDequeue to control the head, this means that if there are not 4 uops,
    // we should not modify the head, next time, we will still dequeue the same uops until we have dequeued the 4 uops
    // the repetitive uops will be handled in the next stage, the decode stage, in core.scala.

    // update registers
    // note: priority: clear > enqueue > dequeue
    when (doEnqueue){
        tail := enqIdx
        when (inMask.reduce(_||_)){
            mayFull := true.B
        }
    }

    when (doDequeue) {
        head := inc(head)
        mayFull := false.B
    }

    when (io.clear) {
        head := 1.U
        tail := 1.U
        mayFull := false.B
    }

    when(reset.asBool){
        io.deq.bits.uops map { u => u.valid := false.B}
    }

}