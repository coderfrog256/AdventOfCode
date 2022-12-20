package zone.frog.advent.twentytwo

import java.io.File
import java.lang.IllegalArgumentException
import kotlin.collections.ArrayList

object Day20 {
    class FuckedUpInteger(val int: Int)

    //FUCK IT
    class CircularList {
        class Node(val data: FuckedUpInteger, var last: Node?, var next: Node?)

        var head: Node? = null
        var tailOnlyForAdd: Node? = null

        fun add(data: FuckedUpInteger) {
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

        fun shift(num: FuckedUpInteger) {
            var moving = head!!
            while (moving.data != num) {
                moving = moving.next!!
            }
            if (num.int > 0) {
                moving.last!!.next = moving.next
                moving.next!!.last = moving.last
                if(moving == head) {
                    head = moving.next
                }

                var iter = moving
                var remaining = num.int
                while (remaining-- > 0) {
                    iter = iter.next!!
                }
                moving.next = iter.next
                iter.next!!.last = moving
                iter.next = moving
                moving.last = iter
            } else if (num.int < 0) {
                moving.last!!.next = moving.next
                moving.next!!.last = moving.last
                if(moving == head) {
                    head = moving.next
                }

                var iter = moving
                var remaining = num.int
                while (remaining++ < 0) {
                    iter = iter.last!!
                }
                moving.last = iter.last
                iter.last!!.next = moving
                iter.last = moving
                moving.next = iter
            }
        }

        fun toList(): List<Int> {
            if(head == null) return emptyList()
            val out = ArrayList<Int>()
            var iter = head
            do {
                out.add(iter!!.data.int)
                iter = iter.next
            } while(iter != head)
            return out
        }

        fun validateSanity(expectedContents: List<FuckedUpInteger>) {
            var iter = head
            val seen = mutableListOf<FuckedUpInteger>()
            do {
                assert(iter!!.last!!.next == iter)
                assert(iter.next!!.last == iter)
                seen.add(iter.data)
                iter = iter.next
            } while(iter != head)
            assert(expectedContents.size == seen.size)
            assert(seen.containsAll(expectedContents))
        }

        operator fun get(index: Int): Int {
            if(head == null || index < 0) throw IllegalArgumentException(index.toString())
            var iter = head
            while(iter!!.data.int != 0) {
                iter = iter.next!!
            }

            var toAdvance = index
            while(toAdvance-- > 0) {
                iter = iter!!.next!!
            }
            return iter!!.data.int
        }
    }

    private fun buildShiftedString(text: List<String>, shiftCount: Int): CircularList {
        val fuckedUpText = text.map { FuckedUpInteger(it.toInt()) }

        val list = CircularList()
        fuckedUpText.forEach { list.add(it) }
        list.seal()
        repeat(shiftCount) {
            for (num in fuckedUpText) {
                list.shift(num)
            }
        }
        list.validateSanity(fuckedUpText)
        return list
    }

    private fun sumIndices(string: CircularList): Int {
        val thousandIndex = 1000
        val twoThousandIndex = 2000
        val threeThousandIndex = 3000
        return string[thousandIndex] + string[twoThousandIndex] + string[threeThousandIndex]
    }

    // Not -13279
    fun scenarioOne(textFile: String) =
        File(textFile).readLines()
            .let { buildShiftedString(it,) }
            .let { sumIndices(it) }


    fun scenarioTwo(textFile: String) =
        File(textFile)
            .let { buildShiftedString(it,) }
            .let { sumIndices(it) }
}