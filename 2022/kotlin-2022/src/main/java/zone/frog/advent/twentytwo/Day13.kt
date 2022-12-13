package zone.frog.advent.twentytwo

import java.io.File
import java.lang.IllegalArgumentException

object Day13 {
    interface Packet {
        fun compareTo(other: Packet): Int
    }

    class NumericPacket(val number: Int) : Packet {
        override fun compareTo(other: Packet): Int {
            return when (other) {
                is NumericPacket -> number - other.number
                is ListPacket -> ListPacket(listOf(this)).compareTo(other)
                else -> throw IllegalArgumentException("Unknown type: ${other.javaClass}")
            }
        }

        override fun toString(): String {
            return "$number"
        }
    }

    class ListPacket(val packets: List<Packet>) : Packet {
        override fun compareTo(other: Packet): Int {
            return when (other) {
                is NumericPacket -> compareTo(ListPacket(listOf(other)))
                is ListPacket -> {
                    packets.forEachIndexed { index, thisItem ->
                        if (index >= other.packets.size) {
                            return 1
                        }
                        val otherItem = other.packets[index]
                        val result = thisItem.compareTo(otherItem)
                        when {
                            result < 0 -> return -1
                            result > 0 -> return 1
                        }
                    }
                    return if (packets.size <= other.packets.size) -1 else 1
                }

                else -> throw IllegalArgumentException("Unknown type: ${other.javaClass}")
            }
        }

        override fun toString(): String {
            return "[${packets.joinToString(",")}]"
        }
    }

    private fun parseItem(start: Int, inputs: String): Pair<Int, Packet> {
        var index = start
        val firstItem = inputs[index]
        val contents = mutableListOf<Packet>()

        if (firstItem.isDigit()) {
            val element = inputs.substring(index).takeWhile { it != ',' && it != ']' }
            return index + element.length to NumericPacket(element.toInt())
        } else while (index < inputs.length && inputs[index] != ']') {
            val parsed = parseItem(index + 1, inputs)
            index = parsed.first
            contents += parsed.second
        }
        return index + 1 to ListPacket(contents)
    }

    private fun inputsInRightOrder(index: Int, pair: List<Packet>): Triple<Int, Packet, Packet>? {
        val (left, right) = pair
        return Triple(index + 1, left, right).takeIf { left.compareTo(right) <= 0 }
    }

    fun scenarioOne(textFile: String) =
        File(textFile).readLines()
            .asSequence()
            .filter { it.isNotBlank() }
            .map { parseItem(0, it).second }
            .chunked(2)
            .mapIndexed { index, pair -> inputsInRightOrder(index, pair) }
            .filterNotNull()
            .map { it.first }
            .sum()

    fun scenarioTwo(textFile: String) =
        (File(textFile).readLines() + listOf("[[2]]", "[[6]]"))
            .asSequence()
            .filter { it.isNotBlank() }
            .map { parseItem(0, it).second }
            .sortedWith(Packet::compareTo)
            .withIndex()
            .filter { it.value.toString() == "[[2]]" || it.value.toString() == "[[6]]" }
            .map { it.index+1 }
            .fold(1) { acc, i -> acc * i }
}