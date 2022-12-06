package zone.frog.advent.twentytwo

import java.io.File
import java.util.regex.Pattern

object Day6 {
    private fun allUnique(indexedChars: IndexedValue<String>) =
        indexedChars.value.toSet().size == indexedChars.value.length

    fun scenarioOne(textFile: String) =
        File(textFile).readText()
            .windowed(4, 1).withIndex()
            .firstNotNullOf { indexedChars -> indexedChars.index.takeIf { allUnique(indexedChars) } }
            .let { it + 4 }

    fun scenarioTwo(textFile: String) =
        File(textFile).readText()
            .windowed(14, 1).withIndex()
            .firstNotNullOf { indexedChars -> indexedChars.index.takeIf { allUnique(indexedChars) } }
            .let { it + 14 }
}