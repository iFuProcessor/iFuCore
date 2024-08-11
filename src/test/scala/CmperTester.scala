package iFu.backend

import scala.util.Random

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

trait CmperTestFunc {
    val FN_EQ   = 0
    val FN_NE   = 1
    val FN_LT   = 2
    val FN_GE   = 3
    val FN_LTU  = 6
    val FN_GEU  = 7
    val funcs = Array(FN_EQ, FN_NE, FN_LT, FN_GE, FN_LTU, FN_GEU)

    def cmper(fn: Int, op1: Int, op2: Int): Boolean = {
        fn match {
            case FN_EQ => if (op1 == op2) true else false
            case FN_NE => if (op1 != op2) true else false
            case FN_LT => if (op1 < op2) true else false
            case FN_GE => if (op1 >= op2) true else false
            case FN_LTU => if ((op1 & 0xFFFFFFFFL) < (op2 & 0xFFFFFFFFL)) true else false
            case FN_GEU => if ((op1 & 0xFFFFFFFFL) >= (op2 & 0xFFFFFFFFL)) true else false
        }
    }

    def testOne(dut: Comparer, fn: Int, op1: Int, op2: Int): Unit = {
        val refResult = cmper(fn, op1, op2)
        dut.io.fn.poke(fn.U)
        dut.io.op1.poke(BigInt(op1 & 0xFFFFFFFFL).U)
        dut.io.op2.poke(BigInt(op2 & 0xFFFFFFFFL).U)
        dut.clock.step(1)
        dut.io.out.expect(refResult.asBool)
    }

    def testFn(dut: Comparer): Unit = {
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

class CmperTester extends AnyFlatSpec with ChiselScalatestTester with CmperTestFunc {
    "Comparer" should "pass" in {
        test(new Comparer()) { dut =>
            println("Testing Coparer")
            testFn(dut)
        }
    }
}
