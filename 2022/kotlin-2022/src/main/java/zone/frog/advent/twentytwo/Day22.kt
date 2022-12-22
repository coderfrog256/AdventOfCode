package zone.frog.advent.twentytwo

import java.io.File
import java.lang.IllegalArgumentException


object Day22 {
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
            if (direction == "X") return this // "X" for the end of instructions.
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

    data class Map2D(val tiles: Map<IntPair, TileType>) {
        fun newPlayer(): Player {
            return Player(startOfRow(1), Direction.RIGHT)
        }

        private fun startOfRow(row: Int) = tiles.filter { it.key.second == row }.minOf { it.key.first } to row
        private fun endOfRow(row: Int) = tiles.filter { it.key.second == row }.maxOf { it.key.first } to row
        private fun topOfColumn(column: Int) = column to tiles.filter { it.key.first == column }.minOf { it.key.second }
        private fun endOfColumn(column: Int) = column to tiles.filter { it.key.first == column }.maxOf { it.key.second }

        fun attemptMove(player: Player, newPosition: IntPair): IntPair {
            return when (tiles[newPosition]) {
                null -> when (player.direction) {
                    Direction.UP -> attemptMove(player, endOfColumn(player.position.first))
                    Direction.RIGHT -> attemptMove(player, startOfRow(player.position.second))
                    Direction.DOWN -> attemptMove(player, topOfColumn(player.position.first))
                    Direction.LEFT -> attemptMove(player, endOfRow(player.position.second))
                }

                TileType.EMPTY -> newPosition
                TileType.WALL -> player.position
            }
        }

        companion object {
            fun parse(lines: List<String>): Map2D {
                val grid = mutableMapOf<IntPair, TileType>()
                for (y in lines.indices) {
                    for (x in 0 until lines[y].length) {
                        when (lines[y][x]) {
                            // Final answer is 1-indexed. We start at one!
                            '.' -> grid[x + 1 to y + 1] = TileType.EMPTY
                            '#' -> grid[x + 1 to y + 1] = TileType.WALL
                        }
                    }
                }
                return Map2D(grid)
            }
        }
    }

    data class Map3D(
        val cubeSideSize: Int,
        val cubeSides: List<Pair<IntRange, IntRange>>,
        val grid: Map<IntPair, TileType>
    ) {
        val sideAdjustments = mutableMapOf<Pair<Int, Direction>, (Player) -> Player>()

        fun registerSideAdjustments(side: Int, direction: Direction, adjustment: (Player) -> Player) {
            sideAdjustments[side to direction] = adjustment
        }

        fun newPlayer() = Player(0 to 0, Direction.RIGHT, 1)

        companion object {
            fun parse(lines: List<String>, cubeSideSize: Int): Map3D {
                val grid = Map2D.parse(lines).tiles

                //Determine ranges for each side. (With a dummy to keep our side numbers in sync with the prompt.)
                val sides = mutableListOf(0..0 to 0..0)
                var i = 1
                while (i < grid.maxOf { it.key.second }) {
                    val line = grid.filter { it.key.second == i }
                    val lineStart = line.minOf { it.key.first }
                    val lineEnd = line.maxOf { it.key.first }

                    // Handle multiple sides side-by-side.
                    for (j in 0..(lineEnd - lineStart) / cubeSideSize) {
                        sides += (lineStart + (cubeSideSize * j)) until (lineStart + (cubeSideSize * (j + 1))) to i..i + (cubeSideSize - 1)
                    }
                    i += cubeSideSize
                }
                return Map3D(cubeSideSize, sides, grid)
            }
        }
    }

    data class Player(var position: IntPair, var direction: Direction, var side: Int = 0) {
        fun step(map: Map2D) {
            position = map.attemptMove(this, position + direction.step)
        }

        fun spin(spinDirection: String) {
            direction = direction.spin(spinDirection)
        }

        fun getPointValue(): Int {
            return (1000 * position.second) + (4 * position.first) + direction.answerValue
        }
    }

    data class Instruction(val amount: Int, val spin: String) {
        companion object {
            fun parse(instructionString: String): List<Instruction> {
                val commands = instructionString + "X"
                val commandRegex = Regex("(\\d+)(\\w)")

                val instructions = mutableListOf<Instruction>()
                var i = 0
                while (i < commands.length) {
                    val match = commandRegex.matchAt(commands, i) ?: break
                    i += match.value.length
                    instructions += Instruction(match.groupValues[1].toInt(), match.groupValues[2])
                }
                return instructions
            }
        }
    }

    private fun parse2DMapAndInstructions(text: List<String>): Pair<Map2D, List<Instruction>> {
        return Map2D.parse(text.subList(0, text.size - 2)) to Instruction.parse(text.last())
    }

    private fun parse3DMapAndInstructions(text: List<String>, cubeSideSize: Int): Pair<Map3D, List<Instruction>> {
        return Map3D.parse(text.subList(0, text.size - 2), cubeSideSize) to Instruction.parse(text.last())
    }

    private fun getCoordinateValue(map: Map2D, instructions: List<Instruction>): Int {
        val player = map.newPlayer()
        for (instruction in instructions) {
            repeat(instruction.amount) { player.step(map) }
            player.spin(instruction.spin)
        }
        return player.getPointValue()
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

    // 116308 is wrong
    // 125324 is wrong.
    // 131026 is wrong
    // 111030 is too low
    // 30589 too low
    // 27571 too low
    private fun getCoordinateValueOnCube(cubeMap: Map3D, instructions: List<Instruction>): Int {
        val cubeSideSize = cubeMap.cubeSideSize
        val grid = cubeMap.grid
        val sides = cubeMap.cubeSides

        // TODO: I'm sure you can figure out a formula for this. For now this is done offline by intently staring at cubes.
        if (sides[1].first.count() == 4) {
            // Top of 1 is top of 2
            cubeMap.registerSideAdjustments(1, Direction.UP)
            { player -> Player((cubeSideSize - 1 - player.position.first to 0), Direction.DOWN, 2) }
            // Down of 1 is just the top of 4
            cubeMap.registerSideAdjustments(1, Direction.DOWN)
            { player -> Player((player.position.first to 0), Direction.DOWN, 4) }
            // Left of 1 is top of 3
            cubeMap.registerSideAdjustments(1, Direction.LEFT)
            { player -> Player((player.position.second to 0), Direction.DOWN, 3) }
            // Right of 1 is right-side of 6
            cubeMap.registerSideAdjustments(1, Direction.RIGHT)
            { player -> Player((cubeSideSize - 1 to cubeSideSize - 1 - player.position.second), Direction.LEFT, 6) }

            // Top of 2 is top of 1
            cubeMap.registerSideAdjustments(2, Direction.UP)
            { player -> Player((cubeSideSize - 1 - player.position.first to 0), Direction.DOWN, 1) }
            // Down of 2 is bottom of 5
            cubeMap.registerSideAdjustments(2, Direction.DOWN)
            { player -> Player((cubeSideSize - 1 - player.position.first to cubeSideSize - 1), Direction.UP, 5) }
            // Left of 2 is bottom of 6
            cubeMap.registerSideAdjustments(2, Direction.LEFT)
            { player -> Player((cubeSideSize - 1 - player.position.second to cubeSideSize - 1), Direction.UP, 6) }
            // Right of 2 is left of 3
            cubeMap.registerSideAdjustments(2, Direction.RIGHT)
            { player -> Player((0 to player.position.second), Direction.RIGHT, 3) }

            // Up of 3 is left of 1
            cubeMap.registerSideAdjustments(3, Direction.UP)
            { player -> Player((0 to player.position.first), Direction.RIGHT, 1) }
            // Down of 3 is left of 5
            cubeMap.registerSideAdjustments(3, Direction.DOWN)
            { player -> Player((0 to cubeSideSize - 1 - player.position.first), Direction.RIGHT, 5) }
            // Left of 3 is right of 2
            cubeMap.registerSideAdjustments(3, Direction.LEFT)
            { player -> Player((cubeSideSize - 1 to player.position.second), Direction.LEFT, 2) }
            // Right of 3 is left of 4
            cubeMap.registerSideAdjustments(3, Direction.RIGHT)
            { player -> Player((0 to player.position.second), Direction.RIGHT, 4) }

            // Up of 4 is just the bottom of 1
            cubeMap.registerSideAdjustments(4, Direction.UP)
            { player -> Player((player.position.first to cubeSideSize - 1), Direction.UP, 1) }
            // Down of 4 is just the top of 5
            cubeMap.registerSideAdjustments(4, Direction.DOWN)
            { player -> Player((player.position.first to 0), Direction.DOWN, 5) }
            // Left of 4 is right of 3
            cubeMap.registerSideAdjustments(4, Direction.LEFT)
            { player -> Player((cubeSideSize - 1 to player.position.second), Direction.LEFT, 3) }
            // Right of 4 is top of 6
            // 12,6 -> 4, 2 becomes (2, 0). 2+side[6].first.first, 0+side[6].second.first = 15,9 (as intended
            cubeMap.registerSideAdjustments(4, Direction.RIGHT)
            { player -> Player((cubeSideSize - 1 - player.position.second to 0), Direction.DOWN, 6) }

            // Up of 5 is just the bottom of 4
            cubeMap.registerSideAdjustments(5, Direction.UP)
            { player -> Player((player.position.first to cubeSideSize - 1), Direction.UP, 4) }
            // Down of 5 is transposed bottom of 2
            cubeMap.registerSideAdjustments(5, Direction.DOWN)
            { player -> Player((cubeSideSize - 1 - player.position.first to cubeSideSize - 1), Direction.UP, 2) }
            // Left of 5 is bottom of 3
            cubeMap.registerSideAdjustments(5, Direction.LEFT)
            { player -> Player((cubeSideSize - 1 - player.position.second to cubeSideSize - 1), Direction.UP, 3) }
            // Right of 5 is left of 6
            cubeMap.registerSideAdjustments(5, Direction.RIGHT)
            { player -> Player((0 to player.position.second), Direction.RIGHT, 6) }

            // Up of 6 is right of 4
            cubeMap.registerSideAdjustments(6, Direction.UP)
            { player -> Player((cubeSideSize - 1 to cubeSideSize - 1 - player.position.first), Direction.LEFT, 4) }
            // Down of 6 is left of 2
            cubeMap.registerSideAdjustments(6, Direction.DOWN)
            { player -> Player((0 to cubeSideSize - 1 - player.position.first), Direction.RIGHT, 2) }
            // Left of 6 is right of 5
            cubeMap.registerSideAdjustments(6, Direction.LEFT)
            { player -> Player((cubeSideSize - 1 to player.position.second), Direction.LEFT, 5) }
            // Right of 6 is right of 1
            cubeMap.registerSideAdjustments(6, Direction.RIGHT)
            { player -> Player((cubeSideSize - 1 to cubeSideSize - 1 - player.position.second), Direction.LEFT, 1) }
        } else {
            //Top of 1 is left of 6
            cubeMap.registerSideAdjustments(1, Direction.UP)
            { player -> Player((0 to player.position.first), Direction.RIGHT, 6) }
            // Down of 1 is top of 3
            cubeMap.registerSideAdjustments(1, Direction.DOWN)
            { player -> Player((player.position.first to 0), Direction.DOWN, 3) }
            // Left of 1 is left of 4
            cubeMap.registerSideAdjustments(1, Direction.LEFT)
            { player -> Player((0 to cubeSideSize - 1 - player.position.second), Direction.RIGHT, 4) }
            // Right of 1 is left of 2
            cubeMap.registerSideAdjustments(1, Direction.RIGHT)
            { player -> Player((0 to player.position.second), Direction.RIGHT, 2) }

            // Top of 2 is bottom of 6.
            cubeMap.registerSideAdjustments(2, Direction.UP)
            { player -> Player((player.position.first to cubeSideSize - 1), Direction.UP, 6) }
            // Bottom of 2 is the Right of 3
            cubeMap.registerSideAdjustments(2, Direction.DOWN)
            { player -> Player((cubeSideSize - 1 to player.position.first), Direction.LEFT, 3) }
            // Left of 2 is right of 1
            cubeMap.registerSideAdjustments(2, Direction.LEFT)
            { player -> Player((cubeSideSize - 1 to player.position.second), Direction.LEFT, 1) }
            // Right of 2 is the right of 5
            cubeMap.registerSideAdjustments(2, Direction.RIGHT)
            { player -> Player((cubeSideSize - 1 to cubeSideSize - 1 - player.position.second), Direction.LEFT, 5) }

            // Top of 3 is bottom of 1
            cubeMap.registerSideAdjustments(3, Direction.UP)
            { player -> Player((player.position.first to cubeSideSize - 1), Direction.UP, 1) }
            // Bottom of 3 is top of 5
            cubeMap.registerSideAdjustments(3, Direction.DOWN)
            { player -> Player((player.position.first to 0), Direction.DOWN, 5) }
            // Left of 3 is top of 4
            cubeMap.registerSideAdjustments(3, Direction.LEFT)
            { player -> Player((player.position.second to 0), Direction.DOWN, 4) }
            // Right of 3 is bottom of 2
            cubeMap.registerSideAdjustments(3, Direction.RIGHT)
            { player -> Player((player.position.second to cubeSideSize - 1), Direction.UP, 2) }

            // Top of 4 is left of 3
            cubeMap.registerSideAdjustments(4, Direction.UP)
            { player -> Player((0 to player.position.first), Direction.RIGHT, 3) }
            // Bottom of 4 is top of 6
            cubeMap.registerSideAdjustments(4, Direction.DOWN)
            { player -> Player((player.position.first to 0), Direction.DOWN, 6) }
            // Left of 4 is left of 1
            cubeMap.registerSideAdjustments(4, Direction.LEFT)
            { player -> Player((0 to cubeSideSize - 1 - player.position.second), Direction.RIGHT, 1) }
            // Right of 4 is left of 5
            cubeMap.registerSideAdjustments(4, Direction.RIGHT)
            { player -> Player((0 to player.position.second), Direction.RIGHT, 5) }

            //Top of 5 is bottom of 3
            cubeMap.registerSideAdjustments(5, Direction.UP)
            { player -> Player((player.position.first to cubeSideSize - 1), Direction.UP, 3) }
            //Bottom of 5 is right of 6
            cubeMap.registerSideAdjustments(5, Direction.DOWN)
            { player -> Player((cubeSideSize - 1 to player.position.first), Direction.LEFT, 6) }
            // Left of 5 is right of 4
            cubeMap.registerSideAdjustments(5, Direction.LEFT)
            { player -> Player((cubeSideSize - 1 to player.position.second), Direction.LEFT, 4) }
            // Right of 5 is right of 2
            cubeMap.registerSideAdjustments(5, Direction.RIGHT)
            { player -> Player((cubeSideSize - 1 to cubeSideSize - 1 - player.position.second), Direction.LEFT, 2) }

            // Top of 6 is bottom of 4
            cubeMap.registerSideAdjustments(6, Direction.UP)
            { player -> Player((player.position.first to cubeSideSize - 1), Direction.UP, 4) }
            // Bottom of 6 is top of 2
            cubeMap.registerSideAdjustments(6, Direction.DOWN)
            { player -> Player((player.position.first to 0), Direction.DOWN, 2) }
            // Left of 6 is top of 1
            cubeMap.registerSideAdjustments(6, Direction.LEFT)
            { player -> Player((player.position.second to 0), Direction.DOWN, 1) }
            // Right of 6 is bottom of 5
            cubeMap.registerSideAdjustments(6, Direction.RIGHT)
            { player -> Player((player.position.second to cubeSideSize - 1), Direction.UP, 5) }
        }

        val player = cubeMap.newPlayer()

        for (instruction in instructions) {
            repeat(instruction.amount) {
                var newPosition = player.position + player.direction.step
                if (newPosition.first < 0 || newPosition.second < 0 || newPosition.first >= cubeSideSize || newPosition.second >= cubeSideSize) {
                    val adjustedPlayer = cubeMap.sideAdjustments[player.side to player.direction]!!(player)

                    player.position = stepWithinSide(
                        player.position,
                        adjustedPlayer.position,
                        sides[adjustedPlayer.side],
                        grid
                    )
                    if (player.position == adjustedPlayer.position) {
                        player.side = adjustedPlayer.side
                        player.direction = adjustedPlayer.direction
                    }
                } else {
                    player.position = stepWithinSide(
                        player.position,
                        newPosition,
                        sides[player.side],
                        grid
                    )
                }
            }
            player.direction = player.direction.spin(instruction.spin)
        }
        val side = sides[player.side]
        val resolvedPosition = player.position.first + side.first.first to player.position.second + side.second.first
        return (1000 * resolvedPosition.second) + (4 * resolvedPosition.first) + player.direction.answerValue
    }

    fun scenarioOne(textFile: String) =
        File(textFile).readLines()
            .let { parse2DMapAndInstructions(it) }
            .let { getCoordinateValue(it.first, it.second) }

    fun scenarioTwo(textFile: String, cubeSideSize: Int) =
        File(textFile).readLines()
            .let { parse3DMapAndInstructions(it, cubeSideSize) }
            .let { getCoordinateValueOnCube(it.first, it.second) }
}
