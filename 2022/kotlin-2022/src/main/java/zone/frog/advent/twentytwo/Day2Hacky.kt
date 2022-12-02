package zone.frog.advent.twentytwo

import java.io.File

// First attempt, going for speed of implementation. Iterating over map with composite keys is not pretty :p
object Day2Hacky {
    enum class Result(val value: Int) {
        WIN(6),
        LOSS(0),
        TIE(3)
    }

    private val matchups = mapOf(
        ("A" to "X") to Result.TIE,
        ("A" to "Y") to Result.WIN,
        ("A" to "Z") to Result.LOSS,

        ("B" to "X") to Result.LOSS,
        ("B" to "Y") to Result.TIE,
        ("B" to "Z") to Result.WIN,

        ("C" to "X") to Result.WIN,
        ("C" to "Y") to Result.LOSS,
        ("C" to "Z") to Result.TIE,
    )

    private val scenarioTwoMapping = mapOf(
        "X" to Result.LOSS,
        "Y" to Result.TIE,
        "Z" to Result.WIN,
    )

    private fun getPoints(opponent: String, you: String): Int {
        val result = matchups[opponent to you]!!.value
        return result + (you[0].code - "X"[0].code) + 1
    }

    private fun runMatch(opponent: String, result: String): Int {
        val desiredResult = scenarioTwoMapping[result]!!
        val choice = matchups.entries
            .first { it.key.first == opponent && it.value == desiredResult }
            .key
            .second
        return getPoints(opponent, choice)
    }

    fun scenarioOne(textFile: String) =
        File(textFile).readLines()
            .map { it.split(" ") }
            .sumOf { getPoints(it[0], it[1]) }

    fun scenarioTwo(textFile: String) =
        File(textFile).readLines()
            .map { it.split(" ") }
            .sumOf { runMatch(it[0], it[1]) }
}
