package zone.frog.advent.twentytwo

import java.io.File

object Day1 {
    private fun buildElves(lines: List<String>): List<Int> {
        val elves = mutableListOf<Int>()
        var currentElf = 0
        for (line in lines) {
            if (line.isBlank()) {
                elves.add(currentElf)
                currentElf = 0
                continue
            }
            currentElf += line.toInt()
        }
        elves.add(currentElf)
        return elves
    }

    fun scenarioOne(textFile: String) =
        buildElves(File(textFile).readLines())
            .max()

    fun scenarioTwo(textFile: String) =
        buildElves(File(textFile).readLines())
            .sortedDescending()
            .take(3)
            .sum()
}