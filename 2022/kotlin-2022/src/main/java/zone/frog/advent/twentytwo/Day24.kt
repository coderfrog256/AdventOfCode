package zone.frog.advent.twentytwo

import java.io.File
import java.lang.Math.abs


object Day24 {
    enum class Move(val move: IntPair, val symbol: Char) {
        NORTH(0 to -1, '^'),
        SOUTH(0 to 1, 'v'),
        WEST(-1 to 0, '<'),
        EAST(1 to 0, '>'),
        NONE(0 to 0, 'X');

        // Called when blizzard needs to wrap.
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

    data class Blizzard(val position: IntPair, val move: Move) {
        fun tick(xRange: IntRange, yRange: IntRange): Blizzard {
            val newPosition = position + move.move
            return if (newPosition.first !in xRange || newPosition.second !in yRange) {
                copy(position = move.reset(position, xRange, yRange))
            } else {
                copy(position = newPosition)
            }
        }
    }

    // I could make an output type, but it's Christmas Eve. Tuples all the way down!
    private fun parseInput(lines: List<String>): Triple<Pair<IntPair, IntPair>, List<Blizzard>, Pair<IntRange, IntRange>> {
        val blizzard = mutableListOf<Blizzard>()
        val playerPosition = lines[0].indexOf('.') to 0
        val endPosition = lines.last().indexOf('.') to lines.size - 1
        val xRange = 1..lines[0].length - 2
        val yRange = 1..lines.size - 2
        lines.forEachIndexed { y, line ->
            line.forEachIndexed { x, char ->
                if (char != '.' && char != '#') {
                    blizzard += Blizzard(x to y, Move.fromSymbol(char))
                }
            }
        }
        return Triple(playerPosition to endPosition, blizzard, xRange to yRange)
    }

    private fun gridDistance(from: IntPair, to: IntPair): Int {
        return abs(to.first - from.first) + abs(to.second - from.second)
    }

    private fun runSimulation(
        startPosition: IntPair,
        endPosition: IntPair,
        blizzardTiles: List<Blizzard>,
        xRange: IntRange,
        yRange: IntRange,
        phasesToRun: Int,
        moves: List<Move> = listOf(Move.EAST, Move.SOUTH, Move.NONE, Move.NORTH, Move.WEST)
    ): Int {
        val blizzardPerTick = mutableListOf(blizzardTiles to blizzardTiles.map { it.position }.toSet())
        var minMoves = Int.MAX_VALUE

        val visited = mutableSetOf<Pair<IntPair, Int>>()
        fun checkMove(position: IntPair, move: Move, ticks: Int, blizzardTiles: Set<IntPair>): IntPair? {
            val newPosition = position + move.move
            val distance = gridDistance(newPosition, endPosition)
            // Only check the move if it's worthwhile (possible to get to the goal with fewer steps than the current min)
            // And do not allow the actor to walk back into the starting position (checking is not needed, you could just wait N turns)
            if (distance + ticks >= minMoves || (move != Move.NONE && newPosition == startPosition)) {
                return null
            }
            // Allow walking into the end position, staying in the current position, or walking if it's in bounds.
            // Do not walk into a tile that has a blizzard.
            if ((newPosition == endPosition || newPosition == startPosition)
                || (newPosition.first in xRange && newPosition.second in yRange && newPosition !in blizzardTiles)
            ) {
                return newPosition
            }
            return null
        }

        fun makeMove(position: IntPair, ticks: Int) {
            if (position == endPosition) {
                // We're at the end. Have we improved the min number of moves?
                if (ticks - 1 < minMoves) {
                    minMoves = minOf(minMoves, ticks - 1)
                    println("New min found: $minMoves")
                }
                return
            }
            if (ticks >= blizzardPerTick.size) {
                // Add new blizzard ticks, if we don't have any cached.
                val currentWind = blizzardPerTick.last().first.map { it.tick(xRange, yRange) }
                blizzardPerTick.add(currentWind to currentWind.map { it.position }.toSet())
            }
            val windTick = blizzardPerTick[ticks].second
            if (position to ticks in visited) {
                // We already visited this tile at this tick. Don't bother simulating further.
                return
            }

            // Check each move. If it's possible, continue the DFS.
            moves.forEach {
                checkMove(position, it, ticks, windTick)?.let { makeMove(it, ticks + 1) }
            }
            visited += position to ticks
        }
        makeMove(startPosition, 0)

        if (phasesToRun > 1) {
            minMoves += runSimulation(
                startPosition = endPosition,
                endPosition = startPosition,
                blizzardTiles = blizzardPerTick[minMoves].first,
                xRange = xRange,
                yRange = yRange,
                phasesToRun = phasesToRun - 1,
                moves = moves.reversed()
            )
        }
        return minMoves
    }

    fun scenarioOne(textFile: String) =
        File(textFile).readLines()
            .let { parseInput(it) }
            .let { runSimulation(it.first.first, it.first.second, it.second, it.third.first, it.third.second, 1) }

    fun scenarioTwo(textFile: String) =
        File(textFile).readLines()
            .let { parseInput(it) }
            .let { runSimulation(it.first.first, it.first.second, it.second, it.third.first, it.third.second, 3) }
}
