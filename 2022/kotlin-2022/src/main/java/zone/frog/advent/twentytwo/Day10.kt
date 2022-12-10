package zone.frog.advent.twentytwo

import java.io.File

object Day10 {
    private fun buildCycleHistory(lines: List<String>): Pair<Int, List<Int>> {
        return lines.fold(1 to listOf(1)) { state, line ->
            val (regX, history) = state
            if (line == "noop") {
                regX to history + regX
            } else {
                regX + line.split(" ").last().toInt() to history + regX + regX
            }
        }
    }

    private fun applyCommands(lines: List<String>, checkIns: List<Int>): Int {
        val (regX, cycleHistory) = buildCycleHistory(lines)
        return checkIns.sumOf { it * (cycleHistory.getOrElse(it) { regX }) }
    }

    private fun renderAsciiArt(lines: List<String>): String {
        val (regX, cycleHistory) = buildCycleHistory(lines)

        return cycleHistory.indices.fold(StringBuilder()) { out, i ->
            if (i % 40 == 0) {
                out.append("\n")
            }

            val pixel = cycleHistory.getOrElse(i + 1) { regX }
            val pixelStart = pixel - 1
            val pixelEnd = pixel + 1
            if ((i % 40) in IntRange(pixelStart, pixelEnd)) {
                out.append("#")
            } else {
                out.append(".")
            }
            out
        }.toString()
    }

    fun scenarioOne(textFile: String, checkIns: List<Int>) = applyCommands(File(textFile).readLines(), checkIns)
    fun scenarioTwo(textFile: String) = renderAsciiArt(File(textFile).readLines())
}