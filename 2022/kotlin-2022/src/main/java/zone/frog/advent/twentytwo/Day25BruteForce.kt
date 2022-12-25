package zone.frog.advent.twentytwo

import java.io.File
import kotlin.math.pow


object Day25BruteForce {
    private val digitMap = mapOf(
        '2' to 2,
        '1' to 1,
        '0' to 0,
        '-' to -1,
        '=' to -2
    )

    private fun increment(builder: StringBuilder, location: Int) {
        if (location == builder.length) {
            builder.insert(0, '1')
            return
        }
        val char = builder.length - 1 - location
        when (builder[char]) {
            '=' -> builder[char] = '-'
            '-' -> builder[char] = '0'
            '0' -> builder[char] = '1'
            '1' -> builder[char] = '2'
            '2' -> {
                builder[char] = '='
                increment(builder, location + 1)
            }
        }
    }

    private fun decrementChar(char: Char): Char {
        return when (char) {
            '2' -> '1'
            '1' -> '0'
            '0' -> '-'
            '-' -> '='
            else -> 'X'
        }
    }

    private fun decrement(builder: StringBuilder, desiredValue: Long) {
        // Special case. If the value is < 5, just start from the beginning and increment.
        if (desiredValue < 5) {
            builder.clear()
            return
        }

        var index = 0
        while (true) {
            val decimal = snafuToDecimal(builder.toString())
            if (decimal <= desiredValue || index >= builder.length) return

            // Decrement the characters until we hit the desired value, starting at the leftmost.
            val char = builder[index]
            val decremented = decrementChar(char)
            if (index == 0 && decremented == '0') {
                // For the leftmost character, it can never go below 1.
                ++index
            } else if (decremented == 'X') {
                // For the rest, if it is below -2 (=), move on
                ++index
            } else {
                // Try to swap out the character. If we go below the desired value, reset it and move to the next char.
                builder[index] = decremented
                if (snafuToDecimal(builder.toString()) < desiredValue) {
                    builder[index] = char
                    ++index
                }
            }
        }
    }

    fun decimalToSnafu(number: Long): String {
        // Start with a super high number, bigger than the desired number.
        val remaining = StringBuilder("2")
        while (snafuToDecimal(remaining.toString()) < number) {
            remaining.append('2')
        }
        // Shrink it down to at or below the desiredValue
        decrement(remaining, number)

        // In case we overshot the number, increment it back.
        var toIncrement = number - snafuToDecimal(remaining.toString())
        while (toIncrement-- > 0) {
            increment(remaining, 0)
        }
        return remaining.toString()
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
