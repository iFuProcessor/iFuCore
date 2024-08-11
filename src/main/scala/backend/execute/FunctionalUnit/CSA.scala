package iFu.backend

import chisel3._
import chisel3.util._
import iFu.common._

abstract class CarrySaveAdderMToN(m: Int, n: Int)(len: Int) extends Module{
    val io = IO(new Bundle() {
        val in = Input(Vec(m, UInt(len.W)))
        val out = Output(Vec(n, UInt(len.W)))
    })
}

class CSA3_2(len: Int) extends CarrySaveAdderMToN(3, 2)(len){
    val temp = Wire(Vec(len, UInt(2.W)))
    for((t, i) <- temp.zipWithIndex){
        val (a, b, cin) = (io.in(0)(i), io.in(1)(i), io.in(2)(i))
        val a_xor_b = a ^ b
        val a_and_b = a & b
        val sum = a_xor_b ^ cin
        val cout = a_and_b | (a_xor_b & cin)
        t := Cat(cout, sum)
    }
    io.out.zipWithIndex.foreach({case(x, i) => x := Cat(temp.reverse map(_(i)))})
}