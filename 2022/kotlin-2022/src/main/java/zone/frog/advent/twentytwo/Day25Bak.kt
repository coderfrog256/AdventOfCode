package zone.frog.advent.twentytwo

import java.io.File
import kotlin.math.pow


object Day25Bak {
    private val digitMap = mapOf(
        '2' to 2,
        '1' to 1,
        '0' to 0,
        '-' to -1,
        '=' to -2
    )

    fun increment(builder: StringBuilder, location: Int) {
        if (location == builder.length) {
            builder.insert(0, '1')
            println("String now at: $builder")
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

    fun bruteForceNumbers() = sequence {
        val builder = StringBuilder()
        while (true) {
            increment(builder, 0)
            yield(builder.toString())
        }
    }

    fun decimalToSnafu(number: Long): String {
        // 4890 -> 2=-1=0
        // Math.pow(number.toDouble(), 1/5.0) -> 5.468418851893543

        // Brute Force
//        val percentage = number/100
//        val numbers = bruteForceNumbers().iterator()
//        var remaining = number
//        while(remaining-- > 1) {
//            numbers.next()
//            if(remaining % percentage == 0L) {
//                println("Currently at: $remaining")
//            }
//        }
//        return numbers.next()

        // Brute Force
        val percentage = number/1000
        val builder = StringBuilder()
        var remaining = number
        while(remaining-- > 0) {
            increment(builder, 0)
            if(remaining % percentage == 0L) {
                println("Currently at: $remaining")
            }
        }
        return builder.toString()
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

    // Not 1=-=10-1-11=
    fun scenarioOne(textFile: String) =
        File(textFile).readLines()
            .sumOf { snafuToDecimal(it) }
            .let { decimalToSnafu(it) }

    fun scenarioTwo(textFile: String) =
        File(textFile)
}
