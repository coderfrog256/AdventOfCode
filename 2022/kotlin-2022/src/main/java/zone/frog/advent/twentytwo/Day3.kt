package zone.frog.advent.twentytwo

import java.io.File

object Day3 {
    private fun priority(letter: Char): Int {
        return if (letter.isUpperCase())
            27 + (letter.code - 'A'.code)
        else
            1 + (letter.code - 'a'.code)
    }

    fun scenarioOne(textFile: String) =
        File(textFile).readLines()
            .map { it.subSequence(0, it.length / 2) to it.subSequence(it.length / 2, it.length) }
            .map { parts -> parts.first.first { first -> parts.second.any { second -> first == second } } }
            .sumOf { priority(it) }

    fun scenarioTwo(textFile: String): Int {
        var i = 0
        return File(textFile).readLines()
            .groupBy { (i++) / 3 }
            .values
            .sumOf { group ->
                group.asSequence()
                    .mapIndexed { index, chars -> chars.map { it to mutableSetOf(index) }.toMap() }
                    .fold(mutableMapOf<Char, MutableSet<Int>>()) { acc, map ->
                        map.forEach { (k, v) ->
                            acc.computeIfAbsent(k) { mutableSetOf() }.addAll(v)
                        }.let { acc }
                    }
                    .filter { it.value.size == 3 }
                    .keys
                    .sumOf { priority(it) }
            }
    }
}