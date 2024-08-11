package iFu.common

import chisel3._
import chisel3.util._

abstract class ReplPolicy(nWays: Int) extends Module {
    require(isPow2(nWays))
    val io = IO(new Bundle {
        val access   = Flipped(Valid(UInt(log2Ceil(nWays).W)))
        val repl_way = Output(UInt(log2Ceil(nWays).W))
    })
}

class PseudoLRU(nWays: Int) extends ReplPolicy(nWays) {
    val tree = RegInit(0.U((nWays - 1).W))

    // update way
    def update_tree(_tree: UInt, access_way: UInt, num_ways: Int): UInt = {
        if (num_ways <= 2) {
            assert(num_ways == 2)
            return !access_way(0)
        } else {
            val left_subtree  = _tree(num_ways - 3, num_ways / 2 - 1)
            val right_subtree = _tree(num_ways / 2 - 2, 0)
            assert(left_subtree.getWidth == num_ways / 2 - 1)
            assert(right_subtree.getWidth == num_ways / 2 - 1)
            val direction = !access_way(log2Ceil(num_ways) - 1)
            return Cat(direction,
                Mux(direction,
                    update_tree(left_subtree, access_way(log2Ceil(num_ways) - 2, 0), num_ways / 2),
                    left_subtree
                ),
                Mux(direction,
                    right_subtree,
                    update_tree(right_subtree, access_way(log2Ceil(num_ways) - 2, 0), num_ways / 2)
                )
            )
        }
    }
    tree := Mux(io.access.valid, update_tree(tree, io.access.bits, nWays), tree)

    // replace way
    def calc_replace_way(_tree: UInt, num_ways: Int): UInt = {
        if (num_ways <= 2) {
            assert(num_ways == 2)
            return _tree(0)
        } else {
            val left_subtree  = _tree(num_ways - 3, num_ways / 2 - 1)
            val right_subtree = _tree(num_ways / 2 - 2, 0)
            assert(left_subtree.getWidth == num_ways / 2 - 1)
            assert(right_subtree.getWidth == num_ways / 2 - 1)
            val prio = _tree(num_ways - 2)
            return Cat(prio, Mux(prio,
                calc_replace_way(right_subtree, num_ways / 2),
                calc_replace_way(left_subtree , num_ways / 2)
            ))
        }
    }
    io.repl_way := calc_replace_way(tree, nWays)
}
