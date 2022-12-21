package zone.frog.advent.twentytwo

import java.io.File
import java.lang.IllegalArgumentException
import java.lang.Math.abs

object Day20 {
    private const val DECRYPTION_KEY = 811589153

    // Wrapper so == doesn't do numeric equality. We want to check identity only!
    class IdentityEqualLong(val long: Long) {
        override fun toString(): String {
            return long.toString()
        }
    }

    // Custom circular linked list.
    // Internally builds an arrayList of elements, so it can quickly jump via modulo.
    class CircularList {
        class Node(val data: IdentityEqualLong) {
            lateinit var last: Node
            lateinit var next: Node

            override fun toString(): String {
                return data.toString()
            }
        }

        private lateinit var head: Node
        private var size: Int = 0

        fun add(data: IdentityEqualLong) {
            ++size
            val newNode = Node(data)
            if (!this::head.isInitialized) {
                head = newNode
                head.last = head
                head.next = head
            } else {
                newNode.last = head.last
                newNode.next = head
                head.last.next = newNode
                head.last = newNode
            }
        }

        fun shift(num: IdentityEqualLong) {
            // Exit early if nothing to move
            if (num.long == 0L) {
                return
            }

            // Find our node
            var moving = head
            while (moving.data != num) {
                moving = moving.next
            }

            // Detach our node
            moving.last.next = moving.next
            moving.next.last = moving.last
            if (moving == head) {
                head = moving.next
            }

            // Build a jumpTable of remaining nodes. This lets use modulo to pick the node.
            val jumpTable = ArrayList<Node>(size)
            var iter = moving.last
            do {
                iter = iter.next
                if (!jumpTable.contains(iter)) {
                    jumpTable.add(iter)
                }
            } while (iter != moving.last)

            // Jump to the right node.
            iter = if (num.long > 0) {
                jumpTable[((num.long - 1) % jumpTable.size).toInt()]
            } else {
                jumpTable[(jumpTable.size - (abs(num.long) % jumpTable.size)).toInt() - 1]
            }

            // Reattach our node.
            moving.next = iter.next
            iter.next.last = moving
            iter.next = moving
            moving.last = iter
        }

        override fun toString(): String {
            if (!this::head.isInitialized) return "EMPTY, LOL"
            val out = StringBuilder()
            var iter = head
            do {
                out.append(iter.data).append(", ")
                iter = iter.next
            } while (iter != head)
            return out.toString()
        }

        operator fun get(index: Int): Long {
            if (!this::head.isInitialized || index < 0) throw IllegalArgumentException(index.toString())

            // Find the magic starting point "0"
            var iter = head
            while (iter.data.long != 0L) {
                iter = iter.next
            }

            // Build a jumpTable, so we can use modulo to get to the right node.
            val jumpTable = ArrayList<Node>(size)
            var toAdvance = iter
            do {
                toAdvance = toAdvance.next
                if (!jumpTable.contains(toAdvance)) {
                    jumpTable.add(toAdvance)
                }
            } while (toAdvance != iter)
            toAdvance = jumpTable[(jumpTable.indexOf(iter) + index) % jumpTable.size]
            return toAdvance.data.long
        }
    }

    private fun buildAndShiftNumbers(text: List<String>, shiftCount: Int, decryptionKey: Int): CircularList {
        val identityEqualNumbers = text.map { IdentityEqualLong(it.toLong() * decryptionKey) }

        val list = CircularList()
        identityEqualNumbers.forEach { list.add(it) }
        repeat(shiftCount) {
            println("$it - $list")
            for (num in identityEqualNumbers) {
                list.shift(num)
            }
        }
        println("10 - $list")
        return list
    }

    fun scenarioOne(textFile: String) =
        File(textFile).readLines()
            .let { buildAndShiftNumbers(it, 1, 1) }
            .let { it[1000] + it[2000] + it[3000] }


    fun scenarioTwo(textFile: String) =
        File(textFile).readLines()
            .let { buildAndShiftNumbers(it, 10, DECRYPTION_KEY) }
            .let { it[1000] + it[2000] + it[3000] }
}