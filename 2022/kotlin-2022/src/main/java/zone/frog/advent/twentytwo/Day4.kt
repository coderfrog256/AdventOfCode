package zone.frog.advent.twentytwo

import java.io.File

object Day4 {
    private fun checkCompleteOverlap(left: IntRange, right: IntRange) =
        (left.first <= right.first && left.last >= right.last) ||
                (right.first <= left.first && right.last >= left.last)

    private fun checkAnyOverlap(left: IntRange, right: IntRange) =
        right.first <= left.last && left.first <= right.last

    fun buildElves(line: String) =
        line.split(",").map { elves ->
            elves.split("-").let { range ->
                IntRange(range[0].toInt(), range[1].toInt())
            }
        }

    fun scenarioOne(textFile: String) =
        File(textFile).readLines()
            .map { buildElves(it) }
            .count { checkCompleteOverlap(it[0], it[1]) }

    fun scenarioTwo(textFile: String) =
        File(textFile).readLines()
            .map { buildElves(it) }
            .count { checkAnyOverlap(it[0], it[1]) }
}