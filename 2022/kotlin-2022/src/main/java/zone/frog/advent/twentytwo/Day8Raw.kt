package zone.frog.advent.twentytwo

import java.io.File

object Day8Raw {
    fun isVisible(x: Int, y: Int, grid: Map<Pair<Int, Int>, Int>): Boolean {
        val maxWidth = grid.keys.maxBy { it.first }.first
        val maxHeight = grid.keys.maxBy { it.second }.second

        val height = grid[x to y]!!

        var visible = true
        for (i in 0 until x) {
            val otherHeight = grid[i to y]!!
            if (height <= otherHeight) {
                visible = false
                break
            }
        }
        if (visible) return true

        visible = true
        for (j in 0 until y) {
            val otherHeight = grid[x to j]!!
            if (height <= otherHeight) {
                visible = false
                break
            }
        }
        if (visible) return true

        visible = true
        for (i in x + 1..maxWidth) {
            val otherHeight = grid[i to y]!!
            if (height <= otherHeight) {
                visible = false
                break
            }
        }
        if (visible) return true

        visible = true
        for (j in y + 1..maxHeight) {
            val otherHeight = grid[x to j]!!
            if (height <= otherHeight) {
                visible = false
                break
            }
        }
        if (visible) return true

        return false
    }

    fun treeScore(x: Int, y: Int, grid: Map<Pair<Int, Int>, Int>): Int {
        val maxWidth = grid.keys.maxBy { it.first }.first
        val maxHeight = grid.keys.maxBy { it.second }.second

        val height = grid[x to y]!!

        var runningSum = 1

        var count = 0
        for (i in (0 until x).reversed()) {
            val otherHeight = grid[i to y]!!
            ++count
            if (height <= otherHeight) {
                break
            }
        }
        runningSum *= count
        count = 0

        for (j in (0 until y).reversed()) {
            val otherHeight = grid[x to j]!!
            ++count
            if (height <= otherHeight) {
                break
            }
        }
        runningSum *= count
        count = 0
        for (i in (x + 1..maxWidth)) {
            val otherHeight = grid[i to y]!!
            ++count
            if (height <= otherHeight) {
                break
            }
        }
        runningSum *= count
        count = 0

        for (j in (y + 1..maxHeight)) {
            val otherHeight = grid[x to j]!!
            ++count
            if (height <= otherHeight) {
                break
            }
        }
        runningSum *= count
        count = 0

        return runningSum
    }


    fun buildGrid(grid: MutableMap<Pair<Int, Int>, Int>, line: String, y: Int): MutableMap<Pair<Int, Int>, Int> {
        var x = 0
        for (char in line) {
            grid[x to y] = char.digitToInt()
            ++x
        }
        return grid
    }

    fun scenarioOne(textFile: String) =
        File(textFile)
            .readLines()
            .foldIndexed(mutableMapOf<Pair<Int, Int>, Int>()) { i, acc, line ->
                buildGrid(acc, line, i)
            }
            .let { grid ->
                grid.entries.filter { isVisible(it.key.first, it.key.second, grid) }
            }
            .count()

    fun scenarioTwo(textFile: String) =
        File(textFile)
            .readLines()
            .foldIndexed(mutableMapOf<Pair<Int, Int>, Int>()) { i, acc, line ->
                buildGrid(acc, line, i)
            }
            .let { grid ->
                grid.entries.map { treeScore(it.key.first, it.key.second, grid) }
            }
            .max()
}