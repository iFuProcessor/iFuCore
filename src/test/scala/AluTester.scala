package iFu.backend

import scala.util.Random

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

trait AluTestFunc {
    val FN_ADD = 0
    val FN_SUB = 1
    val FN_AND = 2
    val FN_NOR = 3
    val FN_OR  = 4
    val FN_XOR = 5
    val FN_SL  = 6
    val FN_SRA = 7
    val FN_SRL = 8
    val FN_ANDN = 9
    val FN_ORN  = 10
    val FN_SLT = 11
    val FN_SLTU = 13
    // val funcs = Array(FN_ADD, FN_SUB, FN_AND, FN_NOR, FN_OR, FN_XOR, FN_SL, FN_SRA, FN_SRL, FN_SLT, FN_SLTU)
    val funcs = Array(FN_ADD, FN_SUB, FN_AND, FN_NOR, FN_OR, FN_XOR, FN_SL, FN_SRA, FN_SRL, FN_SLT, FN_SLTU, FN_ANDN, FN_ORN)

    def alu(fn: Int, op1: Int, op2: Int): Int = {
        fn match {
            case FN_ADD => op1 + op2
            case FN_SUB => op1 - op2
            case FN_AND => op1 & op2
            case FN_NOR => ~(op1 | op2)
            case FN_OR  => op1 | op2
            case FN_XOR => op1 ^ op2
            case FN_SL  => op1 << op2
            case FN_SRA => op1 >> op2
            case FN_SRL => (op1 >>> op2)
            case FN_SLT => if (op1 < op2) 1 else 0
            case FN_SLTU => if ((op1 & 0xFFFFFFFFL) < (op2 & 0xFFFFFFFFL)) 1 else 0
            case FN_ANDN => op1 & ~op2
            case FN_ORN  => op1 | ~op2
        }
    }

    def testOne(dut: Alu, fn: Int, op1: Int, op2: Int): Unit = {
        val refResult = alu(fn, op1, op2)
        dut.io.fn.poke(fn.U)
        dut.io.op1.poke(BigInt(op1 & 0xFFFFFFFFL).U)
        dut.io.op2.poke(BigInt(op2 & 0xFFFFFFFFL).U)
        dut.clock.step(1)
        dut.io.out.expect(BigInt(refResult & 0xFFFFFFFFL).U)
    }

    def testFn(dut: Alu): Unit = {
        val times = 25
        val random = new Random()
        val op1 = Array.fill(times)(random.nextInt())
        val op2 = Array.fill(times)(random.nextInt())
        for (a <- op1) {
            for (b <- op2) {
                for (fn <- funcs) {
                    testOne(dut, fn, a, a)
                    testOne(dut, fn, a, b)
                    testOne(dut, fn, a, -b)
                    testOne(dut, fn, -a, b)
                }
            }
        }
    }

}

class AluTester extends AnyFlatSpec with ChiselScalatestTester with AluTestFunc {
    "Alu" should "pass" in {
        test(new Alu()) { dut =>
            println("Testing Alu")
            testFn(dut)
        }
    }
}
