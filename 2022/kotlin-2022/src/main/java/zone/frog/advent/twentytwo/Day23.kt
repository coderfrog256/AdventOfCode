package zone.frog.advent.twentytwo

import java.io.File


object Day23 {
    val ROUND_COUNT = 10
    val NORTH_OFFSETS = listOf(
        -1 to -1,
        0 to -1,
        1 to -1,
    )
    val EAST_OFFSETS = listOf(
        1 to -1,
        1 to 0,
        1 to 1,
    )
    val SOUTH_OFFSETS = listOf(
        -1 to 1,
        0 to 1,
        1 to 1,
    )
    val WEST_OFFSETS = listOf(
        -1 to -1,
        -1 to 0,
        -1 to 1,
    )
    val ALL_OFFSETS = (NORTH_OFFSETS + SOUTH_OFFSETS + EAST_OFFSETS + WEST_OFFSETS).toSet().toList()

    data class Grid(var graph: Map<IntPair, Elf>, val xRange: IntRange, val yRange: IntRange)

    enum class Move(val move: IntPair, val offsets: List<IntPair>) {
        NORTH(0 to -1, NORTH_OFFSETS),
        SOUTH(0 to 1, SOUTH_OFFSETS),
        WEST(-1 to 0, WEST_OFFSETS),
        EAST(1 to 0, EAST_OFFSETS);

        private fun inRange(position: IntPair, xRange: IntRange, yRange: IntRange): Boolean {
//            val move = position + move
//            return move.first in xRange && move.second in yRange
            return true
        }

        fun canMove(position: IntPair, grid: Grid): Boolean {
            return inRange(position, grid.xRange, grid.yRange)
                    && offsets.map { it + position }.none { it in grid.graph }
        }
    }

    data class Elf(
        var position: IntPair,
        var proposedPosition: IntPair,
        val moves: MutableList<Move> = mutableListOf(*Move.values())
    ) {
        fun pickNextPosition(grid: Grid) {
            proposedPosition = position
            if (ALL_OFFSETS.map { it + position }.none { it in grid.graph }) {
                moves += moves.removeFirst()
                return
            }
            val move = moves.firstOrNull { it.canMove(position, grid) }
            if(move != null) {
                proposedPosition = position + move.move
            }
            moves += moves.removeFirst()
        }

        fun settle() {
            position = proposedPosition
        }
    }

    private fun buildGraph(lines: List<String>): Grid {
        val graph = mutableMapOf<IntPair, Elf>()
        lines.forEachIndexed { y, line ->
            line.forEachIndexed { x, char ->
                if (char == '#') {
                    graph[x to y] = Elf(x to y, x to y)
                }
            }
        }
        return Grid(graph, lines[0].indices, lines.indices)
    }

    private fun runSimulation(grid: Grid, runToCompletion: Boolean): Int {
        repeat(if(runToCompletion) Int.MAX_VALUE else ROUND_COUNT) {
            //Part 1
            grid.graph.values.forEach { it.pickNextPosition(grid) }
            if(runToCompletion && grid.graph.values.all { it.position == it.proposedPosition }) {
                return it+1
            }

            val newGraph = mutableMapOf<IntPair, Elf>()
            var settled = false
            while (!settled) {
                settled = grid.graph.values.all {
                    val proposedOccupant = newGraph[it.proposedPosition]
                    if (proposedOccupant == null) {
                        newGraph[it.proposedPosition] = it
                        true
                    } else {
                        it.proposedPosition = it.position
                        proposedOccupant.proposedPosition = proposedOccupant.position
                        newGraph.clear()
                        false
                    }
                }
            }
            grid.graph = newGraph
            grid.graph.values.forEach { it.settle() }
        }
        val xRange = grid.graph.minOf { it.key.first } .. grid.graph.maxOf { it.key.first }
        val yRange = grid.graph.minOf { it.key.second } .. grid.graph.maxOf { it.key.second }
        return (xRange.count() * yRange.count()) - grid.graph.size
    }

    fun scenarioOne(textFile: String) =
        File(textFile).readLines()
            .let { buildGraph(it) }
            .let { runSimulation(it, false) }

    fun scenarioTwo(textFile: String) =
        File(textFile).readLines()
            .let { buildGraph(it) }
            .let { runSimulation(it, true) }
}
