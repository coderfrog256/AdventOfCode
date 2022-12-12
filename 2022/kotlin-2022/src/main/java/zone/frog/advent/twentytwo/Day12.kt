package zone.frog.advent.twentytwo

import java.io.File
import java.lang.IllegalArgumentException

typealias IntPair = Pair<Int, Int>

object Day12 {
    class Node(
        val letter: Char,
        var goodness: Int = Int.MAX_VALUE,
        private var left: Node? = null,
        private var right: Node? = null,
        private var up: Node? = null,
        private var down: Node? = null,
        private val incomingNodes: MutableList<Node> = mutableListOf()
    ) {
        val height = when (letter) {
            'S' -> 0
            'E' -> 'z'.code - 'a'.code
            else -> letter.code - 'a'.code
        }

        fun tryAdd(other: Node, position: String) {
            if (other.height <= height + 1) {
                when (position) {
                    "left" -> left = other
                    "right" -> right = other
                    "up" -> up = other
                    "down" -> down = other
                    else -> throw IllegalArgumentException("Invalid position: $position")
                }
                other.incomingNodes.add(this)
            }
        }

        private fun relations() = listOfNotNull(left, right, up, down)

        fun traverse(visited: Set<Node>, end: Node, steps: Int): Int? {
            if (this@Node == end) {
                println(steps)
                return steps
            }
            return relations()
                .asSequence()
                .filter { !visited.contains(it) }
                .sortedBy { it.goodness }
                .firstNotNullOf { it.traverse(visited + it, end, steps + 1) }
        }

        fun populateGoodness(goodness: Int, nodes: Map<IntPair, Node>) {
            this.goodness = goodness
            val childGoodness = goodness + 1
            incomingNodes
                .filter { it.goodness > childGoodness }
                .forEach { it.populateGoodness(childGoodness, nodes) }
        }
    }

    private fun buildElevationMapWithStart(lines: List<String>): Pair<Node, Node> {
        val (start, end, _) = buildMapCommon(lines)
        return start to end
    }

    private fun buildElevationMapFromLowest(lines: List<String>): Pair<List<Node>, Node> {
        val (_, end, nodeMap) = buildMapCommon(lines)
        return nodeMap.values.filter { it.height == 0 } to end
    }

    private fun buildMapCommon(lines: List<String>): Triple<Node, Node, Map<IntPair, Node>> {
        val nodeMap: Map<IntPair, Node> = lines
            .flatMapIndexed { yIndex, chars ->
                chars.mapIndexed { xIndex, char -> (xIndex to yIndex) to Node(char) }
            }
            .fold(mapOf()) { acc, pair -> acc + pair }

        fun tryAddPosition(from: IntPair, to: Node, position: String) {
            if (from in nodeMap) {
                val fromNode = nodeMap[from]!!
                to.tryAdd(fromNode, position)
            }
        }

        nodeMap.forEach { (position, node) ->
            tryAddPosition(position.first - 1 to position.second, node, "left")
            tryAddPosition(position.first + 1 to position.second, node, "right")
            tryAddPosition(position.first to position.second - 1, node, "up")
            tryAddPosition(position.first to position.second + 1, node, "down")
        }

        val startNode = nodeMap.values.first { it.letter == 'S' }
        val endNode = nodeMap.values.first { it.letter == 'E' }
        endNode.populateGoodness(0, nodeMap)
        return Triple(startNode, endNode, nodeMap)
    }

    private fun runSimulation(startPosition: Node, endPosition: Node): Int? {
        return startPosition.traverse(mutableSetOf(startPosition), endPosition, 0)
    }

    fun scenarioOne(textFile: String) =
        buildElevationMapWithStart(File(textFile).readLines())
            .let { runSimulation(it.first, it.second) }

    fun scenarioTwo(textFile: String) =
        buildElevationMapFromLowest(File(textFile).readLines())
            .let { mapParts ->
                mapParts.first
                    .minBy { it.goodness }
                    .let { runSimulation(it, mapParts.second) }
            }
}