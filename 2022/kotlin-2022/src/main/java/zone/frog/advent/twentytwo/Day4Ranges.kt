package zone.frog.advent.twentytwo

import java.io.File

// For fun, use IntRange as an iterable/pair, instead of just a Pair with two ints.
object Day4Ranges {
    private fun checkCompleteOverlap(left: IntRange, right: IntRange) =
        right.all { it in left } || left.all { it in right }

    private fun checkAnyOverlap(left: IntRange, right: IntRange) =
        left.any { it in right }

    fun scenarioOne(textFile: String) =
        File(textFile).readLines()
            .map {
                it.split(",").map { elves ->
                    elves.split("-").let { range ->
                        IntRange(range[0].toInt(), range[1].toInt())
                    }
                }
            }
            .count { checkCompleteOverlap(it[0], it[1]) }

    fun scenarioTwo(textFile: String) =
        File(textFile).readLines()
            .map { line ->
                line.split(",").map { elves ->
                    elves.split("-").let { range ->
                        IntRange(range[0].toInt(), range[1].toInt())
                    }
                }
            }
            .count { checkAnyOverlap(it[0], it[1]) }
}