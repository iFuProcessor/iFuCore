package iFu.util

import chisel3._
import chisel3.util._
import iFu.backend.HasUop
import iFu.common.Consts._
import iFu.common._

import scala.language.implicitConversions

object IsEqual {
    def apply(a: UInt, b: UInt): Bool = {
        !(a ^ b).orR
    }
}

object MaskLower {
    def apply(in: UInt): UInt = {
        val n = in.getWidth
        (0 until n).map(i => in >> i.U).reduce(_ | _)
    }
}

object MaskUpper {
    def apply(in: UInt):UInt = { // 假设第i位初次为1，则(n-1,i)为1
        val n = in.getWidth
        (0 until n).map(i => (in << i.U)(n-1,0)).reduce(_|_)
    }
}

object WrapInc {
    def apply(value: UInt, n: Int): UInt = {
        if (isPow2(n)) {
            (value + 1.U)(log2Ceil(n) - 1, 0)
        } else {
            val wrap = value === (n - 1).U
            Mux(wrap, 0.U, value + 1.U)
        }
    }
}

object WrapDec {
    def apply(value: UInt, n: Int): UInt = {
        if (isPow2(n)) {
            (value - 1.U)(log2Ceil(n) - 1,0)
        } else {
            val wrap = value === 0.U
            Mux(wrap, (n-1).U, value - 1.U)
        }
    }
}

object WrapAdd {
    def apply(value: UInt, inc: UInt, n: Int): UInt = {
        if (isPow2(n)) {
            (value + inc)(log2Ceil(n) - 1, 0)
        } else {
            val value_padd = value.pad(value.getWidth + 1)
            val result = value_padd + inc
            val wrap = result >= n.U
            Mux(wrap, result - n.U, result)
        }
    }
}

object GetNewUopAndBrMask {
    def apply(uop: MicroOp, brupdate: BrUpdateInfo): MicroOp = {
        val newuop = WireInit(uop)
        newuop.brMask := uop.brMask & (~brupdate.b1.resolveMask).asUInt
        newuop
    }
}

object IsOlder
{
    def apply(i0: UInt, i1: UInt, head: UInt): Bool = (i0 < i1) ^ (i0 < head) ^ (i1 < head)
}

object maskMatch {
    def apply(msk1: UInt, msk2: UInt): Bool = (msk1 & msk2) =/= 0.U
}

object IsKilledByBranch{
    def apply(brupdate: BrUpdateInfo, uop: MicroOp): Bool = {
        maskMatch(brupdate.b1.mispredictMask, uop.brMask)
    }

    def apply(brupdate: BrUpdateInfo, uop_mask: UInt): Bool = {
        maskMatch(brupdate.b1.mispredictMask, uop_mask)
    }
}

object GetNewBrMask {
    def apply(brupdate: BrUpdateInfo, uop: MicroOp): UInt = {
        uop.brMask & (~brupdate.b1.resolveMask).asUInt
    }

    def apply(brupdate: BrUpdateInfo, br_mask: UInt): UInt = {
        br_mask & (~brupdate.b1.resolveMask).asUInt
    }
}

object UpdateBrMask {
    def apply(brupdate: BrUpdateInfo, uop: MicroOp): MicroOp = {
        val out = WireInit(uop)
        out.brMask := GetNewBrMask(brupdate, uop)
        out
    }

    def apply[T <: HasUop](brupdate: BrUpdateInfo, bundle: Valid[T]): Valid[T] = {
        val out = WireInit(bundle)
        out.bits.uop.brMask := GetNewBrMask(brupdate, bundle.bits.uop.brMask)
        out.valid := bundle.valid && !IsKilledByBranch(brupdate, bundle.bits.uop.brMask)
        out
    }
}

object AlignPCToBoundary
{
    def apply(pc: UInt, b: Int): UInt = {
        // Invert for scenario where pc longer than b
        //   (which would clear all bits above size(b)).
        (~((~pc).asUInt | (b - 1).U)).asUInt
    }
}

object immGen
{
    def apply(immPacked: UInt, immType: UInt): SInt = {
        val imm = WireInit(0.U(32.W))
        imm := MuxLookup(immType, 0.U)(Seq(
            immU5   -> Cat(0.U(27.W),immPacked(14,10)),
            immU12  -> Cat(0.U(20.W),immPacked(21,10)),
            immS12  -> Cat(Fill(20,immPacked(21)),immPacked(21,10)),
            immS14  -> Cat(Fill(16,immPacked(23)),immPacked(23,10),0.U(2.W)),
            immS16  -> Cat(Fill(14,immPacked(25)),immPacked(25,10),0.U(2.W)),
            immU20  -> Cat(immPacked(24,5),0.U(12.W)),
            immS20  -> Cat(Fill(10,immPacked(24)),immPacked(24,5),0.U(2.W)),
            immS26  -> Cat(Fill(4,immPacked(9)),immPacked(9,0),immPacked(25,10),0.U(2.W)),
            immCSR  -> Cat(0.U(18.W),immPacked(23,10))
        ))
        imm.asSInt
    }
}

object ImplicitCast {
    implicit def uintToBitPat(x: UInt): BitPat = BitPat(x)
}
