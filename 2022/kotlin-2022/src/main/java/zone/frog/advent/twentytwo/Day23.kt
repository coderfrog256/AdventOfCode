package zone.frog.advent.twentytwo

import java.io.File


object Day23 {
    private const val TEST_ROUND_COUNT = 10
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

    enum class Move(val move: IntPair, val offsets: List<IntPair>) {
        NORTH(0 to -1, NORTH_OFFSETS),
        SOUTH(0 to 1, SOUTH_OFFSETS),
        WEST(-1 to 0, WEST_OFFSETS),
        EAST(1 to 0, EAST_OFFSETS);

        fun canMove(position: IntPair, grid: Map<IntPair, Elf>): Boolean {
            return offsets.map { it + position }.none { it in grid }
        }
    }

    data class Elf(
        var position: IntPair,
        var proposedPosition: IntPair,
        val moves: MutableList<Move> = mutableListOf(*Move.values())
    ) {
        fun pickNextPosition(grid: Map<IntPair, Elf>) {
            proposedPosition = position
            if (Move.values().flatMap { it.offsets }.map { it + position }.any { it in grid }) {
                val move = moves.firstOrNull { it.canMove(position, grid) }
                if (move != null) {
                    proposedPosition = position + move.move
                }
            }
            moves += moves.removeFirst()
        }

        fun settle() {
            position = proposedPosition
        }

        fun hasNoMoves() = position == proposedPosition
    }

    private fun buildGraph(lines: List<String>): Map<IntPair, Elf> {
        val graph = mutableMapOf<IntPair, Elf>()
        lines.forEachIndexed { y, line ->
            line.forEachIndexed { x, char ->
                if (char == '#') {
                    graph[x to y] = Elf(x to y, x to y)
                }
            }
        }
        return graph
    }

    private fun runSimulation(startingGraph: Map<IntPair, Elf>, runToCompletion: Boolean): Int {
        var graph = HashMap(startingGraph)
        val roundsToSimulate = if (runToCompletion) Int.MAX_VALUE else TEST_ROUND_COUNT
        repeat(roundsToSimulate) { rounds ->
            //Part 1
            graph.values.forEach { it.pickNextPosition(graph) }
            if (runToCompletion && graph.all { it.value.hasNoMoves() }) {
                return rounds + 1
            }

            val newGraph = HashMap<IntPair, Elf>()
            var settled = false
            while (!settled) {
                settled = graph.values.all {
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
            graph = newGraph
            graph.values.forEach { it.settle() }
        }
        val xRange = graph.minOf { it.key.first }..graph.maxOf { it.key.first }
        val yRange = graph.minOf { it.key.second }..graph.maxOf { it.key.second }
        return (xRange.count() * yRange.count()) - graph.size
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
