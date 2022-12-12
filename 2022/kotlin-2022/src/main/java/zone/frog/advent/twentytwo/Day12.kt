package zone.frog.advent.twentytwo

import java.io.File
import java.lang.IllegalArgumentException

typealias IntPair = Pair<Int, Int>

object Day12 {
    class Node(
        val height: Int,
        var goodness: Int = Int.MAX_VALUE,
        var left: Node? = null,
        var right: Node? = null,
        var up: Node? = null,
        var down: Node? = null,
        val incomingNodes: MutableList<Node> = mutableListOf()
    ) {
        fun canAdd(other: Node) = other.height <= height + 1
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
                .map { it.traverse(visited + it, end, steps + 1) }
                .filter { it != null }
                .firstOrNull()
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
        val (start, end, nodeMap) = buildMapCommon(lines)
        return nodeMap[start]!! to nodeMap[end]!!
    }

    private fun buildElevationMapFromLowest(lines: List<String>): Pair<List<Node>, Node> {
        val (_, end, nodeMap) = buildMapCommon(lines)
        return nodeMap.values.filter { it.height == 0 } to nodeMap[end]!!
    }

    private fun buildMapCommon(lines: List<String>): Triple<IntPair, IntPair, MutableMap<IntPair, Node>> {
        var start: IntPair? = null
        var end: IntPair? = null

        val coordinateMap = lines.mapIndexed { yIndex, chars ->
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
        }
        val nodeMap = mutableMapOf<IntPair, Node>()

        coordinateMap.forEach { (position, height) ->
            val node = nodeMap.computeIfAbsent(position) { Node(height) }
            val left = position.first - 1 to position.second
            val right = position.first + 1 to position.second
            val up = position.first to position.second - 1
            val down = position.first to position.second + 1

            if (left in coordinateMap) {
                val leftNode = nodeMap.computeIfAbsent(left) { Node(coordinateMap[left]!!) }
                if (node.canAdd(leftNode)) {
                    node.left = leftNode
                    leftNode.incomingNodes += node
                }
            }
            if (right in coordinateMap) {
                val rightNode = nodeMap.computeIfAbsent(right) { Node(coordinateMap[right]!!) }
                if (node.canAdd(rightNode)) {
                    node.right = rightNode
                    rightNode.incomingNodes += node
                }
            }
            if (up in coordinateMap) {
                val upNode = nodeMap.computeIfAbsent(up) { Node(coordinateMap[up]!!) }
                if (node.canAdd(upNode)) {
                    node.up = upNode
                    upNode.incomingNodes += node
                }
            }
            if (down in coordinateMap) {
                val downNode = nodeMap.computeIfAbsent(down) { Node(coordinateMap[down]!!) }
                if (node.canAdd(downNode)) {
                    node.down = downNode
                    downNode.incomingNodes += node
                }
            }
        }

        nodeMap[end]!!.populateGoodness(0, nodeMap)
        return Triple(start!!, end!!, nodeMap)
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