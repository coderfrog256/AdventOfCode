package zone.frog.advent.twentytwo

import java.io.File

typealias TreeHeightGrid = Array<IntArray>

object Day8 {
    private fun isVisible(x: Int, y: Int, grid: TreeHeightGrid): Boolean {
        val rowCount = grid.size - 1
        val columnCount = grid.first().size - 1
        val height = grid[x][y]

        return (0 until x).none { height <= grid[it][y] }
                || (0 until y).none { height <= grid[x][it] }
                || (x + 1..rowCount).none { height <= grid[it][y] }
                || (y + 1..columnCount).none { height <= grid[x][it] }
    }

    private fun treeScore(x: Int, y: Int, grid: TreeHeightGrid): Int {
        val rowCount = grid.size - 1
        val columnCount = grid.first().size - 1
        val height = grid[x][y]

        // Checks if a filtered line is full, or if it stopped early. Lines that stop early are +1.
        fun lineScore(range: IntProgression, operation: (index: Int) -> Boolean): Int {
            val filtered = range.takeWhile(operation)
            return filtered.count() + if (range.count() != filtered.count()) 1 else 0
        }

        val leftCount = lineScore((0 until x).reversed()) { height > grid[it][y] }
        val upCount = lineScore((0 until y).reversed()) { height > grid[x][it] }
        val rightCount = lineScore((x + 1..rowCount)) { height > grid[it][y] }
        val downCount = lineScore((y + 1..columnCount)) { height > grid[x][it] }
        return (leftCount * upCount * rightCount * downCount)
    }

    private fun buildGrid(lines: List<String>): TreeHeightGrid {
        return lines
            .map { row -> row.map { column -> column.digitToInt() }.toIntArray() }
            .toTypedArray()
    }

    fun scenarioOne(textFile: String) =
        File(textFile)
            .readLines()
            .let { buildGrid(it) }
            .let { grid ->
                grid.flatMapIndexed { x, gridRow -> gridRow.filterIndexed { y, _ -> isVisible(x, y, grid) } }
            }
            .count()

    fun scenarioTwo(textFile: String) =
        File(textFile)
            .readLines()
            .let { buildGrid(it) }
            .let { grid ->
                grid.flatMapIndexed { x, gridRow -> gridRow.mapIndexed { y, _ -> treeScore(x, y, grid) } }
            }
            .max()
}