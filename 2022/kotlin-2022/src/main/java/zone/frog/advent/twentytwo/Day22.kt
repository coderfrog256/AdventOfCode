package zone.frog.advent.twentytwo

import java.io.File
import java.lang.IllegalArgumentException


object Day22 {
    //TODO: 2D Grid class to deal with wraps better.
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

    data class CubeMap(val cubeSides: List<Pair<IntRange, IntRange>>, val grid: Map<IntPair, TileType>)

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
    ): Pair<CubeMap, MutableList<Instruction>> {
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
        //Determine ranges for each side. (With a dummy to keep our side numbers in sync with the prompt.)
        var sides = mutableListOf<Pair<IntRange, IntRange>>(0..0 to 0..0)
        var i = 1
        while (i < grid.maxOf { it.key.second }) {
            val line = grid.filter { it.key.second == i }
            val lineStart = line.minOf { it.key.first }
            val lineEnd = line.maxOf { it.key.first }

            for (j in 0..(lineEnd - lineStart) / cubeSideSize) {
                sides += (lineStart + (cubeSideSize * j)) until (lineStart + (cubeSideSize * (j + 1))) to i..i + (cubeSideSize - 1)
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
        return CubeMap(sides, grid) to instructions
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

    private fun stepWithinSide(
        playerPosition: IntPair,
        newPosition: IntPair,
        side: Pair<IntRange, IntRange>,
        grid: Map<IntPair, TileType>,
    ): IntPair {
        val resolvedPosition = newPosition.first + side.first.first to newPosition.second + side.second.first
        return when (grid[resolvedPosition]) {
            null -> throw IllegalArgumentException("Position not in range! $newPosition")
            TileType.EMPTY -> newPosition
            TileType.WALL -> playerPosition
        }
    }

    // 30589 too low
    private fun getCoordinateValueOnCube(cubeMap: CubeMap, instructions: List<Instruction>, cubeSideSize: Int): Int {
        val grid = cubeMap.grid
        val sides = cubeMap.cubeSides

        // Side+Direction -> New Side after making the move.
        val sideAdjustments = mutableMapOf<Pair<Int, Direction>, (IntPair) -> Pair<Pair<Int, Direction>, IntPair>>()
        // TODO: I'm sure you can figure out a formula for this. For now fuck it.
        // Note that the x and y are offsets from start within the range, NOT the coordinates from part 1.
        // This means 0-indexed.
        if (sides[1].first.count() == 4) {
            // Top of 1 is top of 2, transpose x
            sideAdjustments[1 to Direction.UP] = { (x, y) -> (2 to Direction.DOWN) to (cubeSideSize - 1 - x to 0) }
            // Down of 1 is just the top of 4
            sideAdjustments[1 to Direction.DOWN] = { (x, y) -> (4 to Direction.DOWN) to (x to 0) }
            // Left of 1 is top of 3
            sideAdjustments[1 to Direction.LEFT] = { (x, y) -> (3 to Direction.DOWN) to (y to 0) }
            // Right of 1 is right-side of 6
            sideAdjustments[1 to Direction.RIGHT] =
                { (x, y) -> (6 to Direction.LEFT) to (cubeSideSize - 1 to cubeSideSize - 1 - y) }

            // Top of 2 is top of 1, transpose x
            sideAdjustments[2 to Direction.UP] = { (x, y) -> (1 to Direction.DOWN) to (cubeSideSize - 1 - x to 0) }
            // Down of 2 is bottom of 5
            sideAdjustments[2 to Direction.DOWN] =
                { (x, y) -> (5 to Direction.UP) to (cubeSideSize - 1 - x to cubeSideSize - 1) }
            // Left of 2 is bottom of 6
            sideAdjustments[2 to Direction.LEFT] =
                { (x, y) -> (6 to Direction.UP) to (cubeSideSize - 1 - y to cubeSideSize - 1) }
            // Right of 2 is left of 3
            sideAdjustments[2 to Direction.RIGHT] = { (x, y) -> (3 to Direction.RIGHT) to (0 to y) }

            // Up of 3 is left of 1
            sideAdjustments[3 to Direction.UP] = { (x, y) -> (1 to Direction.RIGHT) to (0 to x) }
            // Down of 3 is left of 5
            sideAdjustments[3 to Direction.DOWN] = { (x, y) -> (5 to Direction.RIGHT) to (0 to cubeSideSize - 1 - x) }
            // Left of 3 is right of 2
            sideAdjustments[3 to Direction.LEFT] = { (x, y) -> (2 to Direction.LEFT) to (cubeSideSize - 1 to y) }
            // Right of 3 is left of 4
            sideAdjustments[3 to Direction.RIGHT] = { (x, y) -> (4 to Direction.RIGHT) to (0 to y) }

            // Up of 4 is just the bottom of 1
            sideAdjustments[4 to Direction.UP] = { (x, y) -> (1 to Direction.UP) to (x to cubeSideSize - 1) }
            // Down of 4 is just the top of 5
            sideAdjustments[4 to Direction.DOWN] = { (x, y) -> (5 to Direction.DOWN) to (x to 0) }
            // Left of 4 is right of 3
            sideAdjustments[4 to Direction.LEFT] = { (x, y) -> (3 to Direction.LEFT) to (cubeSideSize - 1 to y) }
            // Right of 4 is top of 6
            // 12,6 -> 4, 2 becomes (2, 0). 2+side[6].first.first, 0+side[6].second.first = 15,9 (as intended
            sideAdjustments[4 to Direction.RIGHT] = { (x, y) -> (6 to Direction.DOWN) to (cubeSideSize - 1 - y to 0) }

            // Up of 5 is just the bottom of 4
            sideAdjustments[5 to Direction.UP] = { (x, y) -> (4 to Direction.UP) to (x to cubeSideSize - 1) }
            // Down of 5 is transposed bottom of 2
            sideAdjustments[5 to Direction.DOWN] =
                { (x, y) -> (2 to Direction.UP) to (cubeSideSize - 1 - x to cubeSideSize - 1) }
            // Left of 5 is bottom of 3
            sideAdjustments[5 to Direction.LEFT] =
                { (x, y) -> (3 to Direction.UP) to (cubeSideSize - 1 - y to cubeSideSize - 1) }
            // Right of 5 is left of 6
            sideAdjustments[5 to Direction.RIGHT] = { (x, y) -> (6 to Direction.RIGHT) to (0 to y) }

            // Up of 6 is right of 4
            sideAdjustments[6 to Direction.UP] =
                { (x, y) -> (4 to Direction.LEFT) to (cubeSideSize - 1 to cubeSideSize - 1 - x) }
            // Down of 6 is left of 2
            sideAdjustments[6 to Direction.DOWN] = { (x, y) -> (2 to Direction.RIGHT) to (0 to cubeSideSize - 1 - x) }
            // Left of 6 is right of 5
            sideAdjustments[6 to Direction.LEFT] = { (x, y) -> (5 to Direction.LEFT) to (cubeSideSize - 1 to y) }
            // Right of 6 is right of 1
            sideAdjustments[6 to Direction.RIGHT] =
                { (x, y) -> (1 to Direction.LEFT) to (cubeSideSize - 1 to cubeSideSize - 1 - y) }

            /*
            pos: 12,6 -> 15, 9
            side: 4 -> 6 (3->5 in my side counting due to 0-index.)

            12,6
            X: 4 from start, 0 from end
            Y: 1 from start, 3 from end

            15,9
            RelativeX: 2 from start, 1 from end
            RelativeY: 0 from start, 4 from end
             */
        } else {
            sideAdjustments[1 to Direction.UP] = { (x, y) -> (2 to Direction.DOWN) to (cubeSideSize - 1 - x to 0) }
            // Down of 1 is top of 3
            sideAdjustments[1 to Direction.DOWN] = { (x, y) -> (3 to Direction.DOWN) to (x to 0) }
            sideAdjustments[1 to Direction.LEFT] = { (x, y) -> (3 to Direction.DOWN) to (y to 0) }
            // Right of 1 is left of 2
            sideAdjustments[1 to Direction.RIGHT] =
                { (x, y) -> (2 to Direction.RIGHT) to (0 to y) }

            sideAdjustments[2 to Direction.UP] = { (x, y) -> (1 to Direction.DOWN) to (cubeSideSize - 1 - x to 0) }
            sideAdjustments[2 to Direction.DOWN] =
                { (x, y) -> (5 to Direction.UP) to (cubeSideSize - 1 - x to cubeSideSize - 1) }
            // Left of 2 is right of 1
            sideAdjustments[2 to Direction.LEFT] =
                { (x, y) -> (1 to Direction.LEFT) to (cubeSideSize - 1 to y) }
            sideAdjustments[2 to Direction.RIGHT] = { (x, y) -> (3 to Direction.RIGHT) to (0 to y) }

            // Top of 3 is bottom of 1
            sideAdjustments[3 to Direction.UP] = { (x, y) -> (1 to Direction.UP) to (x to cubeSideSize-1) }
            // Bottom of 3 is top of 5
            sideAdjustments[3 to Direction.DOWN] = { (x, y) -> (5 to Direction.DOWN) to (x to 0) }
            sideAdjustments[3 to Direction.LEFT] = { (x, y) -> (2 to Direction.LEFT) to (cubeSideSize - 1 to y) }
            sideAdjustments[3 to Direction.RIGHT] = { (x, y) -> (4 to Direction.RIGHT) to (0 to y) }

            sideAdjustments[4 to Direction.UP] = { (x, y) -> (3 to Direction.UP) to (x to cubeSideSize - 1) }
            // Bottom of 4 is top of 6
            sideAdjustments[4 to Direction.DOWN] = { (x, y) -> (6 to Direction.DOWN) to (x to 0) }
            sideAdjustments[4 to Direction.LEFT] = { (x, y) -> (3 to Direction.LEFT) to (cubeSideSize - 1 to y) }
            // Right of 4 is left of 5
            sideAdjustments[4 to Direction.RIGHT] = { (x, y) -> (5 to Direction.RIGHT) to (0 to y) }

            //Top of 5 is bottom of 3
            sideAdjustments[5 to Direction.UP] = { (x, y) -> (3 to Direction.UP) to (x to cubeSideSize - 1) }
            sideAdjustments[5 to Direction.DOWN] =
                { (x, y) -> (2 to Direction.UP) to (cubeSideSize - 1 - x to cubeSideSize - 1) }
            // Left of 5 is right of 4
            sideAdjustments[5 to Direction.LEFT] =
                { (x, y) -> (4 to Direction.LEFT) to (cubeSideSize - 1 to y) }
            sideAdjustments[5 to Direction.RIGHT] = { (x, y) -> (6 to Direction.RIGHT) to (0 to y) }

            // Top of 6 is bottom of 4
            sideAdjustments[6 to Direction.UP] =
                { (x, y) -> (4 to Direction.UP) to (x to cubeSideSize - 1) }
            sideAdjustments[6 to Direction.DOWN] = { (x, y) -> (2 to Direction.RIGHT) to (0 to cubeSideSize - 1 - x) }
            sideAdjustments[6 to Direction.LEFT] = { (x, y) -> (5 to Direction.LEFT) to (cubeSideSize - 1 to y) }
            sideAdjustments[6 to Direction.RIGHT] =
                { (x, y) -> (1 to Direction.LEFT) to (cubeSideSize - 1 to cubeSideSize - 1 - y) }
        }

        var playerPosition = 0 to 0
        var playerSide = 1
        var playerDirection = Direction.RIGHT

        for (instruction in instructions) {
            repeat(instruction.amount) {
                var newPosition = playerPosition + playerDirection.step
                if (newPosition.first < 0 || newPosition.second < 0 || newPosition.first >= cubeSideSize || newPosition.second >= cubeSideSize) {
                    val adjusted = sideAdjustments[playerSide to playerDirection]!!(playerPosition)

                    val newPosition = adjusted.second
                    val newSide = adjusted.first.first
                    val newDirection = adjusted.first.second
                    playerPosition = stepWithinSide(
                        playerPosition,
                        newPosition,
                        sides[newSide],
                        grid
                    )
                    if (playerPosition == newPosition) {
                        playerSide = newSide
                        playerDirection = newDirection
                    }
                } else {
                    playerPosition = stepWithinSide(
                        playerPosition,
                        newPosition,
                        sides[playerSide],
                        grid
                    )
                }
            }
            playerDirection = playerDirection.spin(instruction.spin)
        }
        val side = sides[playerSide]
        val resolvedPosition = playerPosition.first + side.first.first to playerPosition.second + side.second.first
        return (1000 * resolvedPosition.second) + (4 * resolvedPosition.first) + playerDirection.answerValue
    }

    fun scenarioOne(textFile: String) =
        File(textFile).readLines()
            .let { parseMapAndDirections(it) }
            .let { getCoordinateValue(it.first, it.second) }

    fun scenarioTwo(textFile: String, cubeSideSize: Int) =
        File(textFile).readLines()
            .let { parseMapCubeAndDirections(it, cubeSideSize) }
            .let { getCoordinateValueOnCube(it.first, it.second, cubeSideSize) }
}
