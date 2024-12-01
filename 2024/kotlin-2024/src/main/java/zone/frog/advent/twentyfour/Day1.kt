package zone.frog.advent.twentyfour

import java.io.File
import kotlin.math.abs

object Day1 {
    fun scenarioOne(textFile: String): Int {
        val left = ArrayList<Int>()
        val right = ArrayList<Int>()
        File(textFile).readLines()
            .forEach {
                with(it.split(" ")) {
                    left.add(first().toInt())
                    right.add(last().toInt())
                }
            }
        left.sort()
        right.sort()
        return left.zip(right)
            .map { abs(it.first - it.second) }
            .sum()
    }

    fun scenarioTwo(textFile: String): Int {
        val left = ArrayList<Int>()
        val right = ArrayList<Int>()
        File(textFile).readLines()
            .forEach {
                with(it.split(" ")) {
                    left.add(first().toInt())
                    right.add(last().toInt())
                }
            }
        return left.map { it * right.count { b -> it == b } }
            .sum()
    }
}