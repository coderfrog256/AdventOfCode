package zone.frog.advent.twentytwo

import java.io.File

object Day3 {
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
            .groupBy { it.index / 3 }
            .values
            .sumOf { groupOfThree ->
                groupOfThree.asSequence()
                    .flatMapIndexed { owner, items -> items.value.map { item -> item to owner } }
                    .groupBy({ it.first }, { it.second })
                    .filter { itemOwners -> itemOwners.value.distinct().size == 3 }
                    .keys
                    .sumOf { priority(it) }
            }
}