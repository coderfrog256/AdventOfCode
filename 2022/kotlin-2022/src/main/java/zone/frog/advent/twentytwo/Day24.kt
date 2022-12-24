package zone.frog.advent.twentytwo

import java.io.File


object Day24 {
    enum class Move(val move: IntPair, val symbol: Char) {
        NORTH(0 to -1, '^'),
        SOUTH(0 to 1, 'v'),
        WEST(-1 to 0, '<'),
        EAST(1 to 0, '>'),
        NONE(0 to 0, 'X');

        fun reset(currentPosition: IntPair, xRange: IntRange, yRange: IntRange): IntPair {
            return when (this) {
                NORTH -> currentPosition.first to yRange.last
                SOUTH -> currentPosition.first to yRange.first
                WEST -> xRange.last to currentPosition.second
                EAST -> xRange.first to currentPosition.second
                NONE -> currentPosition
            }
        }

        companion object {
            fun fromSymbol(symbol: Char) = Move.values().first { it.symbol == symbol }
        }
    }

    data class Wind(val position: IntPair, val move: Move) {
        fun tick(xRange: IntRange, yRange: IntRange): Wind {
            val newPosition = position + move.move
            return if (newPosition.first !in xRange || newPosition.second !in yRange) {
                copy(position = move.reset(position, xRange, yRange))
            } else {
                copy(position = newPosition)
            }
        }
    }

    private fun parseInput(lines: List<String>): Triple<Pair<IntPair, IntPair>, List<Wind>, Pair<IntRange, IntRange>> {
        val wind = mutableListOf<Wind>()
        val playerPosition = lines[0].indexOf('.') to 0
        val endPosition = lines.last().indexOf('.') to lines.size - 1
        val xRange = 1..lines[0].length - 2
        val yRange = 1..lines.size - 2
        lines.forEachIndexed { y, line ->
            line.forEachIndexed { x, char ->
                if (char != '.' && char != '#') {
                    wind += Wind(x to y, Move.fromSymbol(char))
                }
            }
        }
        return Triple(playerPosition to endPosition, wind, xRange to yRange)
    }

    fun gridDistance(from: IntPair, to: IntPair): Int {
        return (to.first - from.first) + (to.second - from.second)
    }

    fun printState(xRange: IntRange, yRange: IntRange, position: IntPair, wind: List<Wind>) {
        val out = (0..yRange.last + 1).map { y ->
            (0..xRange.last + 1).map { x ->
                if (x == 0 || y == 0 || x == xRange.last + 1 || y == yRange.last + 1) '#' else '.'
            }.toMutableList()
        }.toMutableList()
        out[0][1] = '.'
        out.last()[out.last().size - 2] = '.'
        out[position.second][position.first] = 'E'
        wind.forEach { out[it.position.second][it.position.first] = it.move.symbol }

        println(out.joinToString("\n") { it.joinToString("") })
    }

    fun runSimulation(
        startPosition: IntPair,
        endPosition: IntPair,
        wind: List<Wind>,
        xRange: IntRange,
        yRange: IntRange
    ): Int {
        val windTicks = mutableListOf(wind to wind.map { it.position }.toSet())
        var minMoves = Int.MAX_VALUE

        val visited = mutableSetOf<Pair<IntPair, Set<IntPair>>>()
        fun checkMove(position: IntPair, move: Move, ticks: Int, windTick: Set<IntPair>): IntPair? {
            val newPosition = position + move.move
            val distance = gridDistance(newPosition, endPosition)
            if (distance + ticks >= minMoves) {
                return null
            }
            if ((newPosition == endPosition || newPosition == startPosition)
                || (newPosition.first in xRange && newPosition.second in yRange && newPosition !in windTick)
            ) {
                return newPosition
            }
            return null
        }

        //639 is too high
        //365 is too high (as is 366)
        fun makeMove(position: IntPair, ticks: Int) {
            if (position == endPosition) {
                if(ticks-1 < minMoves) {
                    minMoves = minOf(minMoves, ticks-1)
                    println("New min found: $minMoves")
                }
                return
            }
            if (ticks >= windTicks.size) {
                val currentWind = windTicks.last().first.map { it.tick(xRange, yRange) }
                windTicks.add(currentWind to currentWind.map { it.position }.toSet())
            }
            val windTick = windTicks[ticks].second
            if(position to windTick in visited) {
                return
            }


            checkMove(position, Move.EAST, ticks, windTick)?.let { makeMove(it, ticks + 1) }
            checkMove(position, Move.SOUTH, ticks, windTick)?.let { makeMove(it, ticks + 1) }
            checkMove(position, Move.NONE, ticks, windTick)?.let { makeMove(it, ticks + 1) }
            checkMove(position, Move.NORTH, ticks, windTick)?.let { makeMove(it, ticks + 1) }
            checkMove(position, Move.WEST, ticks, windTick)?.let { makeMove(it, ticks + 1) }
            visited += position to windTick
        }
        makeMove(startPosition, 0)
        return minMoves
    }

    fun scenarioOne(textFile: String) =
        File(textFile).readLines()
            .let { parseInput(it) }
            .let { runSimulation(it.first.first, it.first.second, it.second, it.third.first, it.third.second) }

    fun scenarioTwo(textFile: String) =
        File(textFile)

}
