package zone.frog.advent.twentytwo

import java.io.File
import java.lang.IllegalStateException

object Day14 {
    // y,x coordinates.
    private val startPoint = 0 to 500

    private fun parseLines(input: String): List<IntPair> {
        return input.split(" -> ")
            .map { positionPair -> positionPair.split(",").let { it[1].toInt() to it[0].toInt() } }
    }

    // For troubleshooting. Call in a debugger to print the current grid.
    private fun printGrid(grid: MutableGrid<Char>) {
        println(grid.first().joinToString("") { "=" })
        println(grid.joinToString("\n") { it.joinToString("") })
        println("\n\n")
    }

    private fun buildGrid(lines: List<String>, addFloor: Boolean): MutableGrid<Char> {
        val gridPoints = lines.map { parseLines(it) }
        val height = gridPoints.flatMap { it.map { it.first + 1 } }.max()
        val width = gridPoints.flatMap { it.map { (it.second + 1) * 2 } }.max() // * 2 to widen the width for part 2.

        //Initialize grid with `.`
        val grid = mutableListOf<MutableList<Char>>()
        repeat(height) {
            grid.add((0 until width).map { '.' }.toMutableList())
        }

        gridPoints.forEach { points ->
            points.take(points.size - 1)
                .forEachIndexed { i, start ->
                    val end = points[i + 1]
                    for (y in (start.first bidirectionalRange end.first)) {
                        grid[y][start.second] = '#'
                    }
                    for (x in (start.second bidirectionalRange end.second)) {
                        grid[start.first][x] = '#'
                    }
                }
        }

        if (addFloor) {
            grid.add((0 until width).map { '.' }.toMutableList())
            grid.add((0 until width).map { '~' }.toMutableList())
        }

        printGrid(grid)
        return grid
    }

    private fun dropSand(grid: MutableGrid<Char>, scenarioTwo: Boolean): Int {
        var grains = 0
        while (true) {
            ++grains
            var sand = startPoint
            var settled = false
            while (!settled) {
                //Look directly down, then down-left, then down-right. If all are taken, mark the grain as settled and drop a new one.
                val down = sand.first + 1 to sand.second
                val downLeft = sand.first + 1 to sand.second - 1
                val downRight = sand.first + 1 to sand.second + 1
                if (!grid.inRange(down) && !grid.inRange(downLeft) && !grid.inRange(downRight)) {
                    if (!scenarioTwo) {
                        return grains - 1 //-1 as the prompt wants to know when grains _start_ falling off, not the first grain to do so...
                    } else {
                        throw IllegalStateException("Ran out of space. Should not happen in scenario two!")
                    }
                }

                if (grid[down] == '.') {
                    sand = down
                } else if (grid[downLeft] == '.') {
                    sand = downLeft
                } else if (grid[downRight] == '.') {
                    sand = downRight
                } else {
                    grid[sand] = 'o'
                    settled = true
                    if (scenarioTwo && sand == startPoint) {
                        return grains
                    }
                }
            }
        }
    }

    fun scenarioOne(textFile: String) =
        File(textFile).readLines()
            .let { buildGrid(it, false) }
            .let { dropSand(it, false) }

    fun scenarioTwo(textFile: String) =
        File(textFile).readLines()
            .let { buildGrid(it, true) }
            .let { dropSand(it, true) }
}