package zone.frog.advent.twentytwo

import java.io.File
import java.lang.IllegalArgumentException


object Day22 {
    //TODO: Grid class to deal with wraps better.
    enum class TileType {
        EMPTY,
        WALL
    }

    enum class Direction(val step: IntPair, val answerValue: Int) {
        UP(0 to -1, 3),
        RIGHT(1 to 0, 0),
        DOWN(0 to 1, 1),
        LEFT(-1 to 0, 2);

        fun spin(direction: String): Direction {
            if (direction == "Z") return this
            return when (direction to this) {
                "R" to UP -> RIGHT
                "R" to RIGHT -> DOWN
                "R" to DOWN -> LEFT
                "R" to LEFT -> UP

                "L" to UP -> LEFT
                "L" to RIGHT -> UP
                "L" to DOWN -> RIGHT
                "L" to LEFT -> DOWN

                else -> throw IllegalArgumentException(direction)
            }
        }
    }

    data class Instruction(val amount: Int, val spin: String)

    fun parseMapAndDirections(text: List<String>): Pair<Map<IntPair, TileType>, List<Instruction>> {
        val grid = mutableMapOf<IntPair, TileType>()
        for (y in 0 until text.size - 2) {
            for (x in 0 until text[y].length) {
                when (text[y][x]) {
                    // Final answer is 1-indexed. We start at one!
                    '.' -> grid[x + 1 to y + 1] = TileType.EMPTY
                    '#' -> grid[x + 1 to y + 1] = TileType.WALL
                }
            }
        }
        val instructionString = text.last() + "Z"
        val commandRegex = Regex("(\\d+)(\\w)")

        val instructions = mutableListOf<Instruction>()
        var i = 0
        while (i < instructionString.length) {
            val match = commandRegex.matchAt(instructionString, i) ?: break
            i += match.value.length
            instructions += Instruction(match.groupValues[1].toInt(), match.groupValues[2])
        }
        return grid to instructions
    }

    private fun parseMapCubeAndDirections(
        text: List<String>,
        cubeSideSize: Int
    ): Pair<Map<IntPair, TileType>, List<Instruction>> {
        val grid = mutableMapOf<IntPair, TileType>()
        for (y in 0 until text.size - 2) {
            for (x in 0 until text[y].length) {
                when (text[y][x]) {
                    // Final answer is 1-indexed. We start at one!
                    '.' -> grid[x + 1 to y + 1] = TileType.EMPTY
                    '#' -> grid[x + 1 to y + 1] = TileType.WALL
                }
            }
        }
        //Determine ranges for each side.
        var sides = mutableListOf<Pair<IntRange, IntRange>>()
        var i = 1
        while (i < grid.maxOf { it.key.second }) {
            val line = grid.filter { it.key.second == i }
            val lineStart = line.minOf { it.key.first }
            val lineEnd = line.maxOf { it.key.first }

            for (j in 0..(lineEnd - lineStart) / cubeSideSize) {
                sides += (lineStart + (cubeSideSize*j)) until (lineStart + (cubeSideSize * (j + 1))) to i..i + (cubeSideSize - 1)
            }
            i += cubeSideSize
        }

        val lineOne = grid.filter { it.key.second == 1 }
        val sideOneStart = lineOne.minOf { it.key.second }


        val instructionString = text.last() + "Z"
        val commandRegex = Regex("(\\d+)(\\w)")

        val instructions = mutableListOf<Instruction>()
        i = 0
        while (i < instructionString.length) {
            val match = commandRegex.matchAt(instructionString, i) ?: break
            i += match.value.length
            instructions += Instruction(match.groupValues[1].toInt(), match.groupValues[2])
        }
        return grid to instructions
    }

    private fun step(
        playerPosition: IntPair,
        playerDirection: Direction,
        newPosition: IntPair,
        grid: Map<IntPair, TileType>,
    ): IntPair {
        return when (grid[newPosition]) {
            null -> when (playerDirection) {
                Direction.UP -> step(
                    playerPosition,
                    playerDirection,
                    playerPosition.first to grid.filter { it.key.first == playerPosition.first }
                        .maxOf { it.key.second },
                    grid
                )

                Direction.RIGHT -> step(
                    playerPosition,
                    playerDirection,
                    grid.filter { it.key.second == playerPosition.second }
                        .minOf { it.key.first } to playerPosition.second,
                    grid)

                Direction.DOWN -> step(
                    playerPosition,
                    playerDirection,
                    playerPosition.first to grid.filter { it.key.first == playerPosition.first }
                        .minOf { it.key.second },
                    grid
                )

                Direction.LEFT -> step(
                    playerPosition,
                    playerDirection,
                    grid.filter { it.key.second == playerPosition.second }
                        .maxOf { it.key.first } to playerPosition.second,
                    grid)
            }

            TileType.EMPTY -> newPosition
            TileType.WALL -> playerPosition
        }
    }

    private fun getCoordinateValue(grid: Map<IntPair, TileType>, instructions: List<Instruction>): Int {
        var playerPosition = grid.filter { it.key.second == 1 }.minOf { it.key.first } to 1
        var playerDirection = Direction.RIGHT

        for (instruction in instructions) {
            repeat(instruction.amount) {
                playerPosition = step(playerPosition, playerDirection, playerPosition + playerDirection.step, grid)
            }
            playerDirection = playerDirection.spin(instruction.spin)
        }
        return (1000 * playerPosition.second) + (4 * playerPosition.first) + playerDirection.answerValue
    }

    fun scenarioOne(textFile: String) =
        File(textFile).readLines()
            .let { parseMapAndDirections(it) }
            .let { getCoordinateValue(it.first, it.second) }

    fun scenarioTwo(textFile: String, cubeSideSize: Int) =
        File(textFile).readLines()
            .let { parseMapCubeAndDirections(it, cubeSideSize) }
}
