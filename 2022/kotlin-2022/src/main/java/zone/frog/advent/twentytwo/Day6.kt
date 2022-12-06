package zone.frog.advent.twentytwo

import java.io.File

object Day6 {
    private fun allUnique(chars: String) =
        chars.toSet().size == chars.length

    fun scenarioOne(textFile: String) =
        File(textFile).readText()
            .windowed(4, 1).withIndex()
            .firstNotNullOf { indexedChars -> indexedChars.index.takeIf { allUnique(indexedChars.value) } }
            .let { it + 4 }

    fun scenarioTwo(textFile: String) =
        File(textFile).readText()
            .windowed(14, 1).withIndex()
            .firstNotNullOf { indexedChars -> indexedChars.index.takeIf { allUnique(indexedChars.value) } }
            .let { it + 14 }
}