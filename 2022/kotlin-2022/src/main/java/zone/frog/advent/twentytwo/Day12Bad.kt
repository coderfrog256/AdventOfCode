package zone.frog.advent.twentytwo

import java.io.File

object Day12Bad {

    fun buildElevationMap(lines: List<String>): Pair<Pair<IntPair, IntPair>, MutableMap<IntPair, Int>> {
        var start = -1 to -1
        var end = -1 to -1

        return lines.mapIndexed { yIndex, chars ->
            yIndex to chars.mapIndexed { xIndex, char ->
                xIndex to when (char) {
                    'S' -> {
                        start = xIndex to yIndex
                        'a'.code
                    }

                    'E' -> {
                        end = xIndex to yIndex
                        'z'.code - 'a'.code
                    }

                    else -> char.code - 'a'.code
                }
            }
        }.fold<Pair<Int, List<Pair<Int, Int>>>, MutableMap<IntPair, Int>>(mutableMapOf()) { acc, pairs ->
            pairs.second.forEach { pair ->
                acc += (pair.first to pairs.first) to pair.second
            }
            acc
        }.let { (start to end) to it }
    }

    fun distance(from: IntPair, to: IntPair): Int {
        return (to.first - from.first) + (to.second - from.second)
    }

    fun runSimulation(startPosition: IntPair, endPosition: IntPair, topography: Map<IntPair, Int>): Int {
        val visited = mutableSetOf(startPosition)
        fun makeSteps(position: IntPair, steps: Int): Sequence<Int> = sequence {
            if (position == endPosition) {
                println("Option: $steps")
                yield(steps)
            }
            val (thisX, thisY) = position
            val maxElevationChange = topography[position]!! + 1

            listOf(
                thisX - 1 to thisY,
                thisX + 1 to thisY,
                thisX to thisY - 1,
                thisX to thisY + 1
            )
                .map { it to topography[it] }
                .filter { it.second != null && it.second!! <= maxElevationChange && !visited.contains(it.first) }
                .sortedWith {l, r ->
                    val distanceDiff = distance(l.first, endPosition) - distance(r.first, endPosition)
                    if(distanceDiff == 0) l.second!! - r.second!! else distanceDiff
                }
                .forEach { move ->
                    visited.add(move.first)
                    yieldAll(makeSteps(move.first, steps + 1))
                    println("Exiting path: $position")
                    visited.remove(move.first)
                }
        }

        return makeSteps(startPosition, 0)
            .sorted()
            .first()
    }

    fun scenarioOne(textFile: String) =
        File(textFile)
            .let { buildElevationMap(it.readLines()) }
            .let { runSimulation(it.first.first, it.first.second, it.second) }

    fun scenarioTwo(textFile: String) =
        File(textFile)
}