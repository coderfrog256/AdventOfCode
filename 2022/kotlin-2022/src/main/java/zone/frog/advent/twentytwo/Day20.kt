package zone.frog.advent.twentytwo

import java.io.File
import java.lang.IllegalArgumentException
import java.lang.Math.abs
import kotlin.collections.ArrayList

object Day20 {
    private const val DECRYPTION_KEY = 811589153
    class IdentityEqualLong(val long: Long) {
        override fun toString(): String {
            return long.toString()
        }
    }

    // Hacky circular linked list. Before using it, call seal() to lock it in place.
    // Note that "tail" is only used during adding elements.
    class CircularList {
        class Node(val data: IdentityEqualLong, var last: Node?, var next: Node?) {
            override fun toString(): String {
                return data.toString()
            }
        }

        var head: Node? = null
        var tailOnlyForAdd: Node? = null
        var listLength: Int = 0

        fun add(data: IdentityEqualLong) {
            ++listLength
            if (head == null) {
                head = Node(data, null, null)
                tailOnlyForAdd = head
            } else {
                tailOnlyForAdd!!.next = Node(data, tailOnlyForAdd, null)
                tailOnlyForAdd = tailOnlyForAdd!!.next
            }
        }

        fun seal() {
            tailOnlyForAdd!!.next = head
            head!!.last = tailOnlyForAdd
        }

        fun shift(num: IdentityEqualLong) {
            var moving = head!!
            while (moving.data != num) {
                moving = moving.next!!
            }
            if (num.long > 0) {
                moving.last!!.next = moving.next
                moving.next!!.last = moving.last
                if(moving == head) {
                    head = moving.next
                }
                val jumpTable = mutableListOf<Node>()
                var iter = moving.last!!
                do {
                    iter = iter.next!!
                    if(!jumpTable.contains(iter)) {
                        jumpTable.add(iter)
                    }
                } while(iter != moving.last)
                iter = jumpTable[((num.long-1) % jumpTable.size).toInt()]

                moving.next = iter.next
                iter.next!!.last = moving
                iter.next = moving
                moving.last = iter
            } else if (num.long < 0) {
                moving.last!!.next = moving.next
                moving.next!!.last = moving.last
                if(moving == head) {
                    head = moving.next
                }
                val jumpTable = mutableListOf<Node>()
                var iter = moving.last!!
                do {
                    iter = iter.next!!
                    if(!jumpTable.contains(iter)) {
                        jumpTable.add(iter)
                    }
                } while(iter != moving.last)
                iter = jumpTable[(jumpTable.size - (abs(num.long) % jumpTable.size)).toInt()]

                moving.last = iter.last
                iter.last!!.next = moving
                iter.last = moving
                moving.next = iter
            }
        }

        fun toList(): List<Long> {
            if(head == null) return emptyList()
            val out = ArrayList<Long>()
            var iter = head
            do {
                out.add(iter!!.data.long)
                iter = iter.next
            } while(iter != head)
            return out
        }

        fun validateSanity(expectedContents: List<IdentityEqualLong>) {
            var iter = head
            val seen = mutableListOf<IdentityEqualLong>()
            do {
                assert(iter!!.last!!.next == iter)
                assert(iter.next!!.last == iter)
                seen.add(iter.data)
                iter = iter.next
            } while(iter != head)
            assert(expectedContents.size == seen.size)
            assert(seen.containsAll(expectedContents))
        }

        fun toStringFake(): String {
            if(head == null) return "EMPTY LOL"
            val out = StringBuilder()
            var iter = head
            do {
                out.append(iter!!.data).append(",")
                iter = iter.next
            } while(iter != head)
            return out.toString()
        }

        operator fun get(index: Int): Long {
            if(head == null || index < 0) throw IllegalArgumentException(index.toString())
            var iter = head
            while(iter!!.data.long != 0L) {
                iter = iter.next!!
            }

            var toAdvance = index
            while(toAdvance-- > 0) {
                iter = iter!!.next!!
            }
            return iter!!.data.long
        }
    }

    private fun buildShiftedString(text: List<String>, shiftCount: Int, decryptionKey: Int): CircularList {
        val fuckedUpText = text.map { IdentityEqualLong(it.toLong() * decryptionKey) }

        val list = CircularList()
        fuckedUpText.forEach { list.add(it) }
        list.seal()
        repeat(shiftCount) {
            println("$it - ${list.toStringFake()}")
            for (num in fuckedUpText) {
                list.shift(num)
            }
        }
        println("10 - ${list.toStringFake()}")
        list.validateSanity(fuckedUpText)
        return list
    }

    private fun sumIndices(string: CircularList): Long {
        val thousandIndex = 1000
        val twoThousandIndex = 2000
        val threeThousandIndex = 3000
        return string[thousandIndex] + string[twoThousandIndex] + string[threeThousandIndex]
    }

    fun scenarioOne(textFile: String) =
        File(textFile).readLines()
            .let { buildShiftedString(it, 1, 1) }
            .let { sumIndices(it) }


    fun scenarioTwo(textFile: String) =
        File(textFile).readLines()
            .let { buildShiftedString(it, 10, DECRYPTION_KEY) }
            .let { sumIndices(it) }
}