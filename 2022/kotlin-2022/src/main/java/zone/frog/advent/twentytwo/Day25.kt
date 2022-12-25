package zone.frog.advent.twentytwo

import java.io.File
import kotlin.math.pow


object Day25 {
    private val digitMap = mapOf(
        '2' to 2,
        '1' to 1,
        '0' to 0,
        '-' to -1,
        '=' to -2
    )
    private val reverseMap = digitMap.entries.associateBy({ it.value.toLong() }, { it.key })

    fun decimalToSnafu(number: Long): String {
        val digits = mutableListOf<Char>()
        var remaining = number
        while (remaining > 0) {
            val adjusted = remaining + 2
            digits += reverseMap[(adjusted % 5) - 2]!!
            remaining = adjusted / 5
        }
        return digits.reversed().joinToString("")
    }

    fun snafuToDecimal(snafu: String): Long {
        return snafu
            .reversed()
            .mapIndexed { index, c ->
                5.0.pow(index.toDouble()) * digitMap[c]!!
            }
            .sum()
            .toLong()
    }

    fun scenarioOne(textFile: String) =
        File(textFile).readLines()
            .sumOf { snafuToDecimal(it) }
            .let { decimalToSnafu(it) }
}
