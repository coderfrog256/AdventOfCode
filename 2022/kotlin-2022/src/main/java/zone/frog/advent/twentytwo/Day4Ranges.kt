package zone.frog.advent.twentytwo

import zone.frog.advent.twentytwo.Day4.buildElves
import java.io.File

// For fun, use IntRange as an iterable/pair, instead of just a Pair with two ints.
object Day4Ranges {
    private fun checkCompleteOverlap(left: IntRange, right: IntRange) =
        right.all { it in left } || left.all { it in right }

    private fun checkAnyOverlap(left: IntRange, right: IntRange) =
        left.any { it in right }

    fun scenarioOne(textFile: String) =
        File(textFile).readLines()
            .map { buildElves(it) }
            .count { checkCompleteOverlap(it[0], it[1]) }

    fun scenarioTwo(textFile: String) =
        File(textFile).readLines()
            .map { buildElves(it) }
            .count { checkAnyOverlap(it[0], it[1]) }
}