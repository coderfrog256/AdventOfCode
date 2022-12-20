package zone.frog.advent.twentytwo

import java.io.File
import java.lang.IllegalArgumentException
import kotlin.collections.ArrayList

object Day20 {
    //FUCK IT
    class CircularList {
        class Node(val data: Int, var last: Node?, var next: Node?)

        var head: Node? = null
        var tail: Node? = null

        fun add(data: Int) {
            if (head == null) {
                head = Node(data, null, null)
                tail = head
            } else {
                tail!!.next = Node(data, tail, null)
                tail = tail!!.next
            }
        }

        fun seal() {
            tail!!.next = head
            head!!.last = tail
        }

        fun shift(num: Int) {
            var moving = head!!
            while (moving.data != num) {
                moving = moving.next!!
            }
            if (num > 0) {
                moving.last!!.next = moving.next
                moving.next!!.last = moving.last
                if(moving == head) {
                    head = moving.next
                }

                var iter = moving
                var remaining = num
                while (remaining-- > 0) {
                    iter = iter.next!!
                }
                moving.next = iter.next
                iter.next!!.last = moving
                iter.next = moving
                moving.last = iter
            } else if (num < 0) {
                moving.last!!.next = moving.next
                moving.next!!.last = moving.last
                if(moving == head) {
                    head = moving.next
                }

                var iter = moving
                var remaining = num
                while (remaining++ < 0) {
                    iter = iter.last!!
                }
                moving.last = iter.last
                iter.last!!.next = moving
                iter.last = moving
                moving.next = iter
            }
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

        fun toList(): List<Int> {
            if(head == null) return emptyList()
            val out = ArrayList<Int>()
            var iter = head
            do {
                out.add(iter!!.data)
                iter = iter.next
            } while(iter != head)
            return out
        }

        operator fun get(index: Int): Int {
            if(head == null || index < 0) throw IllegalArgumentException(index.toString())
            var iter = head
            while(iter!!.data != 0) {
                iter = iter.next!!
            }

            var toAdvance = index
            while(toAdvance-- > 0) {
                iter = iter!!.next!!
            }
            return iter!!.data
        }
    }

    private fun buildShiftedString(text: List<String>): CircularList {
        val list = CircularList()
        text.forEach { list.add(it.toInt()) }
        list.seal()
        for (num in text.map { it.toInt() }) {
            list.shift(num)
        }
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
            .let { buildShiftedString(it) }
            .let { sumIndices(it) }


    fun scenarioTwo(textFile: String) =
        File(textFile)
}