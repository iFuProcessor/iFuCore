package iFu.backend

import scala.util.Random

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

trait DivTestFunc {
    val FN_DIV = 1
    val FN_DIVU = 0
    val FN_REM = 3
    val FN_REMU = 2
    def funcs = Array(FN_DIV, FN_REM, FN_REMU)

    def divider(fn: Int, op1: Int, op2: Int): Int = {
        if (op2 == 0) {
            return fn match {
                case FN_DIV => 0xFFFFFFFF
                case FN_DIVU => 0xFFFFFFFF
                case FN_REM => op1
                case FN_REMU => op1
            }
        }
        return fn match {
            case FN_DIV => op1 / op2
            case FN_DIVU => ((op1 & 0xFFFFFFFFL) / (op2 & 0xFFFFFFFFL)).toInt
            case FN_REM => op1 % op2
            case FN_REMU => ((op1 & 0xFFFFFFFFL) % (op2 & 0xFFFFFFFFL)).toInt
        }
    }

    def testOne(dut: SRT16Divider, fn: Int, op1: Int, op2: Int): Unit = {
        val op = fn match {
            case FN_DIV => "/"
            case FN_DIVU => "/u"
            case FN_REM => "%"
            case FN_REMU => "%u"
        }
        // println(s"<><><><><><><><><><> Testing $op1 $op $op2 <><><><><><><><><><>")
        val refResult = divider(fn, op1, op2)
        dut.io.resp.ready.poke(true.B)
        dut.io.req.valid.poke(true.B)
        dut.io.req.bits.fn.poke(fn.U)
        dut.io.req.bits.op1.poke(BigInt(op1 & 0xFFFFFFFFL).U)
        dut.io.req.bits.op2.poke(BigInt(op2 & 0xFFFFFFFFL).U)
        dut.clock.step(1)
        dut.io.req.valid.poke(false.B)

        while (dut.io.resp.valid.peek().litToBoolean == false) {
            dut.clock.step(1)
        }

        dut.io.resp.bits.data.expect(BigInt(refResult & 0xFFFFFFFFL).U)
        dut.clock.step(1)
        // println(s"<><><><><><><><><><> Testing $op1 $op $op2 <><><><><><><><><><>")
        // println("========================================================\n")
    }
    def getOneRef(dut: SRT16DividerRef, fn: Int, op1: Int, op2: Int): Unit = {
        val op = fn match {
            case FN_DIV => "/"
            case FN_DIVU => "/u"
            case FN_REM => "%"
            case FN_REMU => "%u"
        }
        // println(s"<><><><><><><><><><> Testing $op1 $op $op2 <><><><><><><><><><>")
        val refResult = divider(fn, op1, op2)
        dut.io.resp.ready.poke(true.B)
        dut.io.req.valid.poke(true.B)
        dut.io.req.bits.fn.poke(fn.U)
        dut.io.req.bits.op1.poke(BigInt(op1 & 0xFFFFFFFFL).U)
        dut.io.req.bits.op2.poke(BigInt(op2 & 0xFFFFFFFFL).U)
        dut.clock.step(1)
        dut.io.req.valid.poke(false.B)

        while (dut.io.resp.valid.peek().litToBoolean == false) {
            dut.clock.step(1)
        }
        dut.clock.step(2)
        // println(s"<><><><><><><><><><> Testing $op1 $op $op2 <><><><><><><><><><>")
        // println("========================================================\n")
    }

    def testFn(dut: SRT16Divider): Unit = {
        val times = 4
        val random = new Random()
        val op1 = Array.fill(times)(random.nextInt())
        val op2 = Array.fill(times)(random.nextInt())
        // val op1 = Array(2, 4)
        // val op2 = Array(1, 2)
        for (a <- op1) {
            for (b <- op2) {
                // val fn = FN_DIV
                for (fn <- funcs) {
                    // testOne(dut, fn, a, a)
                    // testOne(dut, fn, a, b)
                    // testOne(dut, fn, a, -b)
                    // testOne(dut, fn, -a, b)
                    testOne(dut, fn, a, 0)
                    testOne(dut, fn, 0, b)
                    testOne(dut, fn, 0, 0)
                    testOne(dut, fn, a, -1)
                    testOne(dut, fn, 1, b)
                    testOne(dut, fn, 1, -1)
                    testOne(dut, fn, 1, 0)
                    testOne(dut, fn, 0, -1)
                }
            }
        }
    }

    def getRef(dut: SRT16DividerRef): Unit = {
        val times = 4
        val random = new Random()
        // val op1 = Array.fill(times)(random.nextInt())
        // val op2 = Array.fill(times)(random.nextInt())
        val op1 = Array(2, 4)
        val op2 = Array(1, 2)
        for (a <- op1) {
            for (b <- op2) {
                val fn = FN_DIV
                // for (fn <- funcs) {
                    // testOne(dut, fn, a, a)
                    getOneRef(dut, fn, a, b)
                    // testOne(dut, fn, a, -b)
                    // testOne(dut, fn, -a, b)
                // }
            }
        }
    }
}

class DivTester extends AnyFlatSpec with ChiselScalatestTester with DivTestFunc {
    "SRT16Divider" should "pass" in {
        // test(new SRT16DividerRef(true)) { dut =>
        //     println("Testing SRT16Divider")
        //     getRef(dut)         
        // }
        test(new SRT16Divider()) { dut =>
            println("Testing SRT16Divider")
            testFn(dut)
        }
    }
}
