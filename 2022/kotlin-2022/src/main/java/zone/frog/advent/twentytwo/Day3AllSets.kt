package zone.frog.advent.twentytwo

import java.io.File

//Implementation using sets. Assumes the input is well-formed (only one shared letter in scenarios one and two)
object Day3AllSets {
    private fun priority(item: Char) =
        if (item.isUpperCase())
            27 + (item.code - 'A'.code)
        else
            1 + (item.code - 'a'.code)

    fun scenarioOne(textFile: String) =
        File(textFile).readLines().asSequence()
            .flatMap { it.subSequence(0, it.length / 2).toSet().intersect(it.subSequence(it.length / 2, it.length).toSet()) }
            .sumOf { priority(it) }

    fun scenarioTwo(textFile: String) =
        File(textFile).readLines().withIndex()
            .groupBy({ it.index / 3 }, {it.value})
            .values
            .sumOf {
                it[0].toSet()
                    .intersect(it[1].toSet())
                    .intersect(it[2].toSet())
                    .sumOf { priority(it) }
            }
}