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
        private lateinit var tail: Node
        private var size: Int = 0

        fun add(data: IdentityEqualLong) {
            ++size
            if (!this::head.isInitialized) {
                head = Node(data)
                tail = head
                tail.next = head
                head.last = tail
            } else {
                tail.next = Node(data)
                tail.next.last = tail
                tail = tail.next
                tail.next = head
                head.last = tail
            }
        }

        fun shift(num: IdentityEqualLong) {
            var moving = head
            // Find our node
            while (moving.data != num) {
                moving = moving.next
            }
            if (num.long > 0) {
                // Detach our node
                moving.last.next = moving.next
                moving.next.last = moving.last
                if(moving == head) {
                    head = moving.next
                }

                // Build a jumpTable of remaining nodes. This lets use modulo to pick the node.
                val jumpTable = ArrayList<Node>(size)
                var iter = moving.last
                do {
                    iter = iter.next
                    if(!jumpTable.contains(iter)) {
                        jumpTable.add(iter)
                    }
                } while(iter != moving.last)
                iter = jumpTable[((num.long-1) % jumpTable.size).toInt()]

                // Found it. Reattach our node.
                moving.next = iter.next
                iter.next.last = moving
                iter.next = moving
                moving.last = iter
            } else if (num.long < 0) {
                // Detach our node
                moving.last.next = moving.next
                moving.next.last = moving.last
                if(moving == head) {
                    head = moving.next
                }

                // Build a jumpTable of remaining nodes. This lets use modulo to pick the node.
                val jumpTable = ArrayList<Node>(size)
                var iter = moving.last
                do {
                    iter = iter.next
                    if(!jumpTable.contains(iter)) {
                        jumpTable.add(iter)
                    }
                } while(iter != moving.last)
                iter = jumpTable[(jumpTable.size - (abs(num.long) % jumpTable.size)).toInt()]

                // Found it. Reattach our node.
                moving.last = iter.last
                iter.last.next = moving
                iter.last = moving
                moving.next = iter
            }
        }

        override fun toString(): String {
            if(!this::head.isInitialized) return "EMPTY, LOL"
            val out = StringBuilder()
            var iter = head
            do {
                out.append(iter.data).append(", ")
                iter = iter.next
            } while(iter != head)
            return out.toString()
        }

        operator fun get(index: Int): Long {
            if(!this::head.isInitialized || index < 0) throw IllegalArgumentException(index.toString())

            // Find the magic starting point "0"
            var iter = head
            while(iter.data.long != 0L) {
                iter = iter.next
            }

            // Build a jumpTable, so we can use modulo to get to the right node.
            val jumpTable = ArrayList<Node>(size)
            var toAdvance = iter
            do {
                toAdvance = toAdvance.next
                if(!jumpTable.contains(toAdvance)) {
                    jumpTable.add(toAdvance)
                }
            } while(toAdvance != iter)
            toAdvance = jumpTable[(jumpTable.indexOf(iter)+index) % jumpTable.size]
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

    private fun sumIndices(list: CircularList): Long {
        val thousandIndex = 1000
        val twoThousandIndex = 2000
        val threeThousandIndex = 3000
        return list[thousandIndex] + list[twoThousandIndex] + list[threeThousandIndex]
    }

    fun scenarioOne(textFile: String) =
        File(textFile).readLines()
            .let { buildAndShiftNumbers(it, 1, 1) }
            .let { sumIndices(it) }


    fun scenarioTwo(textFile: String) =
        File(textFile).readLines()
            .let { buildAndShiftNumbers(it, 10, DECRYPTION_KEY) }
            .let { sumIndices(it) }
}