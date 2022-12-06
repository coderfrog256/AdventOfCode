package zone.frog.advent.twentytwo

import java.io.File

object Day6 {
    private fun allUnique(chars: String) = chars.toSet().size == chars.length

    private fun firstUnique(textFile: String, groupSize: Int) =
        File(textFile).readText()
            .windowed(groupSize, 1).withIndex()
            .firstNotNullOf { indexedChars -> indexedChars.index.takeIf { allUnique(indexedChars.value) } }
            .let { it + groupSize }

    fun scenarioOne(textFile: String) = firstUnique(textFile, 4)

    fun scenarioTwo(textFile: String) = firstUnique(textFile, 14)
}