package zone.frog.advent.twentytwo

import java.io.File

typealias TreeHeightGrid = Map<Pair<Int, Int>, Int>
object Day8 {
    private fun isVisible(x: Int, y: Int, grid: TreeHeightGrid): Boolean {
        val rowCount = grid.keys.maxBy { it.first }.first
        val columnCount = grid.keys.maxBy { it.second }.second
        val height = grid[x to y]!!

        return (0 until x).none { height <= grid[it to y]!! }
                || (0 until y).none { height <= grid[x to it]!! }
                || (x + 1..rowCount).none { height <= grid[it to y]!! }
                || (y + 1..columnCount).none { height <= grid[x to it]!! }
    }

    private fun treeScore(x: Int, y: Int, grid: TreeHeightGrid): Int {
        val maxWidth = grid.keys.maxBy { it.first }.first
        val maxHeight = grid.keys.maxBy { it.second }.second
        val height = grid[x to y]!!

        // Checks if a filtered line is full, or if it stopped early. Lines that stop early are +1.
        fun lineScore(range: IntProgression, operation: (index: Int) -> Boolean): Int {
            val filtered = range.takeWhile(operation)
            return filtered.count() + if (range.count() != filtered.count()) 1 else 0
        }

        val leftCount = lineScore((0 until x).reversed()) { height > grid[it to y]!! }
        val upCount = lineScore((0 until y).reversed()) { height > grid[x to it]!! }
        val rightCount = lineScore((x + 1..maxWidth)) { height > grid[it to y]!! }
        val downCount = lineScore((y + 1..maxHeight)) { height > grid[x to it]!! }
        return (leftCount * upCount * rightCount * downCount)
    }

    private fun buildGrid(lines: List<String>) = lines
        .mapIndexed { y, line ->
            line.withIndex().associate { (it.index to y) to it.value.digitToInt() }
        }
        .fold(mapOf<Pair<Int, Int>, Int>()) { acc, row -> acc + row }

    fun scenarioOne(textFile: String) =
        File(textFile)
            .readLines()
            .let { buildGrid(it) }
            .let { grid ->
                grid.entries.filter { isVisible(it.key.first, it.key.second, grid) }
            }
            .count()

    fun scenarioTwo(textFile: String) =
        File(textFile)
            .readLines()
            .let { buildGrid(it) }
            .let { grid ->
                grid.entries.map { treeScore(it.key.first, it.key.second, grid) }
            }
            .max()
}