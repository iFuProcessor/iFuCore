package iFu.backend

import scala.util.Random

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

trait MultTestFunc {
    val FN_MUL  = 3
    val FN_MULH = 1
    val FN_MULHU= 0
    val funcs = Array(FN_MUL, FN_MULH, FN_MULHU)

    def mult(fn: Int, op1: Int, op2: Int): Int = {
        fn match {
            case FN_MUL => op1 * op2
            case FN_MULH => ((op1.toLong * op2.toLong) >> 32).toInt
            case FN_MULHU => (((op1 & 0xFFFFFFFFL) * (op2 & 0xFFFFFFFFL)) >> 32).toInt
        }
    }

    def testOne(dut: MultStar, fn: Int, op1: Int, op2: Int): Unit = {
        val refResult = mult(fn, op1, op2)
        dut.io.req.valid.poke(true.B)
        dut.io.req.bits.fn.poke(fn.U)
        dut.io.req.bits.op1.poke(BigInt(op1 & 0xFFFFFFFFL).U)
        dut.io.req.bits.op2.poke(BigInt(op2 & 0xFFFFFFFFL).U)
        dut.clock.step(1)
        dut.io.req.valid.poke(false.B)
        dut.clock.step(dut.latency - 1)
        dut.io.resp.valid.expect(true.B)
        dut.io.resp.bits.data.expect(BigInt(refResult & 0xFFFFFFFFL).U)
        dut.clock.step(1)
    }

    def testFn(dut: MultStar): Unit = {
        val times = 4
        val random = new Random()
        val op1 = Array.fill(times)(random.nextInt())
        val op2 = Array.fill(times)(random.nextInt())
        for (a <- op1) {
            for (b <- op2) {
                for (fn <- funcs) {
                    // println(s"Testing $fn $a $a")
                    testOne(dut, fn, a, a)
                    // println(s"Testing $fn $a $b")
                    testOne(dut, fn, a, b)
                    // println(s"Testing $fn $a ${-b}")
                    testOne(dut, fn, a, -b)
                    // println(s"Testing $fn ${-a} $b")
                    testOne(dut, fn, -a, b)
                }
            }
        }
    }

    def testPipe(dut: MultStar, fn: Int): Unit = {
        val times = 4
        val random = new Random()
        val op1 = Array.fill(times)(random.nextInt())
        val op2 = Array.fill(times)(random.nextInt())
        var cnt = 0
        for (i <- 0 until times) {
            for (j <- 0 until times) {
                dut.io.req.valid.poke(true.B)
                dut.io.req.bits.fn.poke(fn.U)
                dut.io.req.bits.op1.poke(BigInt(op1(i) & 0xFFFFFFFFL).U)
                dut.io.req.bits.op2.poke(BigInt(op2(j) & 0xFFFFFFFFL).U)
                dut.clock.step(1)

                if (dut.io.resp.valid.peek().litToBoolean) {
                    val refResult = mult(fn, op1(cnt / times), op2(cnt % times))
                    dut.io.resp.bits.data.expect(BigInt(refResult & 0xFFFFFFFFL).U)
                    cnt += 1
                }
            }
        }
        dut.io.req.valid.poke(false.B)
        for (i <- 0 until dut.latency - 1) {
            dut.clock.step(1)

            val refResult = mult(fn, op1(cnt / times), op2(cnt % times))
            dut.io.resp.bits.data.expect(BigInt(refResult & 0xFFFFFFFFL).U)
            cnt += 1
        }
    }

    def testFnPipe(dut: MultStar): Unit = {
        for (fn <- funcs) {
            testPipe(dut, fn)
        }
    }
}

class MultTester extends AnyFlatSpec with ChiselScalatestTester with MultTestFunc {
    "MultStar" should "pass" in {
        test(new MultStar()) { dut =>
            println("Testing MultStar")
            testFn(dut)
            testFnPipe(dut)
        }
    }
}
