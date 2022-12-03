package zone.frog.advent.twentytwo

import java.io.File

object Day3 {
    private fun priority(item: Char): Int {
        return if (item.isUpperCase())
            27 + (item.code - 'A'.code)
        else
            1 + (item.code - 'a'.code)
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
            .sumOf { groupOfThree ->
                groupOfThree
                    .mapIndexed { owner, items -> items.map { item -> item to owner } }
                    .fold(mutableMapOf<Char, MutableSet<Int>>()) { itemOwners, itemAndOwner ->
                        itemAndOwner.forEach { (item, owner) -> itemOwners.computeIfAbsent(item) { mutableSetOf() }.add(owner) }
                        itemOwners
                    }
                    .filter { itemOwners -> itemOwners.value.size == 3 }
                    .keys
                    .sumOf { priority(it) }
            }
    }
}