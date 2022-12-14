package zone.frog.advent.twentytwo

import java.io.File
import java.lang.IllegalStateException

object Day14 {
    // y,x coordinates.
    private val startPoint = 0 to 500

    private fun parseLines(input: String): List<IntPair> {
        return input.split(" -> ")
            .map { it.split(",").let { it[1].toInt() to it[0].toInt() } }
    }

    // For troubleshooting. Call in a debugger to print the current grid.
    private fun printGrid(grid: MutableList<MutableList<Char>>) {
        println(grid.first().joinToString("") { "=" })
        println(grid.joinToString("\n") { it.joinToString("") })
        println("\n\n")
    }

    private fun buildGrid(lines: List<String>, addFloor: Boolean): MutableList<MutableList<Char>> {
        val gridPoints = lines.map { parseLines(it) }
        val height = gridPoints.flatMap { it.map { it.first + 1 } }.max()
        val width = gridPoints.flatMap { it.map { (it.second + 1)*2 } }.max()

        //Initialize grid with .
        val grid = mutableListOf<MutableList<Char>>()
        repeat(height) {
            grid.add((0 until width).map { '.' }.toMutableList())
        }

        //Draw lines. Take a starting point, move one unit drawing a # at each step until you reach the end.
        //Repeat as needed.
        gridPoints.forEach { points ->
            var current = points[0]
            for (point in points) {
                grid[current.first][current.second] = '#'
                while (current != point) {
                    if (current.first < point.first) {
                        current = current.first + 1 to current.second
                        grid[current.first][current.second] = '#'
                    } else if (current.first > point.first) {
                        current = current.first - 1 to current.second
                        grid[current.first][current.second] = '#'
                    }

                    if (current.second < point.second) {
                        current = current.first to current.second + 1
                        grid[current.first][current.second] = '#'
                    } else if (current.second > point.second) {
                        current = current.first to current.second - 1
                        grid[current.first][current.second] = '#'
                    }
                }
                current = point
            }
        }

        if (addFloor) {
            grid.add((0 until width).map { '.' }.toMutableList())
            grid.add((0 until width).map { '~' }.toMutableList())
        }

        return grid
    }

    private fun dropSand(grid: MutableList<MutableList<Char>>, scenarioTwo: Boolean): Int {
        var grains = 0
        while (true) {
            ++grains
            var sand = startPoint
            var settled = false
            do {
                if (sand.first + 1 >= grid.size || sand.second + 1 >= grid[sand.first + 1].size || sand.second - 1 < 0) {
                    if (!scenarioTwo) {
                        return grains - 1 //-1 as the prompt wants to know when grains _start_ falling off, not the first grain to do so...
                    } else {
                        throw IllegalStateException("Ran out of space. Should not happen in scenario two!")
                    }
                }

                //Look directly down, then down-left, then down-right. If all are take, mark the grain as settled and drop a new one.
                if (grid[sand.first + 1][sand.second] == '.') {
                    sand = sand.first + 1 to sand.second
                } else if (grid[sand.first + 1][sand.second - 1] == '.') {
                    sand = sand.first + 1 to sand.second - 1
                } else if (grid[sand.first + 1][sand.second + 1] == '.') {
                    sand = sand.first + 1 to sand.second + 1
                } else {
                    grid[sand.first][sand.second] = 'o'
                    settled = true
                    if(scenarioTwo && sand == startPoint) {
                        return grains
                    }
                }
            } while (!settled)
        }
    }

    fun scenarioOne(textFile: String) =
        File(textFile).readLines()
            .let { buildGrid(it, false) }
            .let { dropSand(it,false) }

    fun scenarioTwo(textFile: String) =
        File(textFile).readLines()
            .let { buildGrid(it, true) }
            .let { dropSand(it, true) }
}