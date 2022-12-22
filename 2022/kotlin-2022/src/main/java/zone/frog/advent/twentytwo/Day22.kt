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
        val sideSize: Int,
        val sides: List<Pair<IntRange, IntRange>>,
        val tiles: Map<IntPair, TileType>
    ) {
        fun newPlayer() = Player(0 to 0, Direction.RIGHT, 1)

        private val sideAdjustments = mutableMapOf<Pair<Int, Direction>, (Player) -> Player>()
        private fun registerSideAdjustments(side: Int, direction: Direction, adjustment: (Player) -> Player) {
            sideAdjustments[side to direction] = adjustment
        }

        private fun canMoveToTile(player: Player, newPosition: IntPair): Boolean {
            val side = sides[player.side]
            val resolvedPosition = newPosition.first + side.first.first to newPosition.second + side.second.first
            return when (tiles[resolvedPosition]) {
                null -> throw IllegalArgumentException("Position not in range! $newPosition")
                TileType.EMPTY -> true
                TileType.WALL -> false
            }
        }

        fun attemptMove(player: Player, newPosition: IntPair): Player {
            return if (newPosition.first !in 0 until sideSize || newPosition.second !in 0 until sideSize) {
                val adjustedPlayer = sideAdjustments[player.side to player.direction]!!(player)
                if (canMoveToTile(adjustedPlayer, adjustedPlayer.position)) {
                    adjustedPlayer
                } else {
                    player
                }
            } else if (canMoveToTile(player, newPosition)) {
                player.copy(position = newPosition)
            } else {
                player
            }
        }

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
                val out = Map3D(cubeSideSize, sides, grid)

                // I'm sure you can figure out a formula for this. For now this is done offline by intently staring at cubes.
                // If you want to solve for a different map then you, too, will have to stare at cubes.
                if (sides[1].first.count() == 4) {
                    // Top of 1 is top of 2
                    out.registerSideAdjustments(1, Direction.UP)
                    { Player((cubeSideSize - 1 - it.position.first to 0), Direction.DOWN, 2) }
                    // Down of 1 is just the top of 4
                    out.registerSideAdjustments(1, Direction.DOWN)
                    { Player((it.position.first to 0), Direction.DOWN, 4) }
                    // Left of 1 is top of 3
                    out.registerSideAdjustments(1, Direction.LEFT)
                    { Player((it.position.second to 0), Direction.DOWN, 3) }
                    // Right of 1 is right-side of 6
                    out.registerSideAdjustments(1, Direction.RIGHT)
                    { Player((cubeSideSize - 1 to cubeSideSize - 1 - it.position.second), Direction.LEFT, 6) }

                    // Top of 2 is top of 1
                    out.registerSideAdjustments(2, Direction.UP)
                    { Player((cubeSideSize - 1 - it.position.first to 0), Direction.DOWN, 1) }
                    // Down of 2 is bottom of 5
                    out.registerSideAdjustments(2, Direction.DOWN)
                    { Player((cubeSideSize - 1 - it.position.first to cubeSideSize - 1), Direction.UP, 5) }
                    // Left of 2 is bottom of 6
                    out.registerSideAdjustments(2, Direction.LEFT)
                    { Player((cubeSideSize - 1 - it.position.second to cubeSideSize - 1), Direction.UP, 6) }
                    // Right of 2 is left of 3
                    out.registerSideAdjustments(2, Direction.RIGHT)
                    { Player((0 to it.position.second), Direction.RIGHT, 3) }

                    // Up of 3 is left of 1
                    out.registerSideAdjustments(3, Direction.UP)
                    { Player((0 to it.position.first), Direction.RIGHT, 1) }
                    // Down of 3 is left of 5
                    out.registerSideAdjustments(3, Direction.DOWN)
                    { Player((0 to cubeSideSize - 1 - it.position.first), Direction.RIGHT, 5) }
                    // Left of 3 is right of 2
                    out.registerSideAdjustments(3, Direction.LEFT)
                    { Player((cubeSideSize - 1 to it.position.second), Direction.LEFT, 2) }
                    // Right of 3 is left of 4
                    out.registerSideAdjustments(3, Direction.RIGHT)
                    { Player((0 to it.position.second), Direction.RIGHT, 4) }

                    // Up of 4 is just the bottom of 1
                    out.registerSideAdjustments(4, Direction.UP)
                    { Player((it.position.first to cubeSideSize - 1), Direction.UP, 1) }
                    // Down of 4 is just the top of 5
                    out.registerSideAdjustments(4, Direction.DOWN)
                    { Player((it.position.first to 0), Direction.DOWN, 5) }
                    // Left of 4 is right of 3
                    out.registerSideAdjustments(4, Direction.LEFT)
                    { Player((cubeSideSize - 1 to it.position.second), Direction.LEFT, 3) }
                    // Right of 4 is top of 6
                    // 12,6 -> 4, 2 becomes (2, 0). 2+side[6].first.first, 0+side[6].second.first = 15,9 (as intended
                    out.registerSideAdjustments(4, Direction.RIGHT)
                    { Player((cubeSideSize - 1 - it.position.second to 0), Direction.DOWN, 6) }

                    // Up of 5 is just the bottom of 4
                    out.registerSideAdjustments(5, Direction.UP)
                    { Player((it.position.first to cubeSideSize - 1), Direction.UP, 4) }
                    // Down of 5 is transposed bottom of 2
                    out.registerSideAdjustments(5, Direction.DOWN)
                    { Player((cubeSideSize - 1 - it.position.first to cubeSideSize - 1), Direction.UP, 2) }
                    // Left of 5 is bottom of 3
                    out.registerSideAdjustments(5, Direction.LEFT)
                    { Player((cubeSideSize - 1 - it.position.second to cubeSideSize - 1), Direction.UP, 3) }
                    // Right of 5 is left of 6
                    out.registerSideAdjustments(5, Direction.RIGHT)
                    { Player((0 to it.position.second), Direction.RIGHT, 6) }

                    // Up of 6 is right of 4
                    out.registerSideAdjustments(6, Direction.UP)
                    { Player((cubeSideSize - 1 to cubeSideSize - 1 - it.position.first), Direction.LEFT, 4) }
                    // Down of 6 is left of 2
                    out.registerSideAdjustments(6, Direction.DOWN)
                    { Player((0 to cubeSideSize - 1 - it.position.first), Direction.RIGHT, 2) }
                    // Left of 6 is right of 5
                    out.registerSideAdjustments(6, Direction.LEFT)
                    { Player((cubeSideSize - 1 to it.position.second), Direction.LEFT, 5) }
                    // Right of 6 is right of 1
                    out.registerSideAdjustments(6, Direction.RIGHT)
                    { Player((cubeSideSize - 1 to cubeSideSize - 1 - it.position.second), Direction.LEFT, 1) }
                } else {
                    //Top of 1 is left of 6
                    out.registerSideAdjustments(1, Direction.UP)
                    { Player((0 to it.position.first), Direction.RIGHT, 6) }
                    // Down of 1 is top of 3
                    out.registerSideAdjustments(1, Direction.DOWN)
                    { Player((it.position.first to 0), Direction.DOWN, 3) }
                    // Left of 1 is left of 4
                    out.registerSideAdjustments(1, Direction.LEFT)
                    { Player((0 to cubeSideSize - 1 - it.position.second), Direction.RIGHT, 4) }
                    // Right of 1 is left of 2
                    out.registerSideAdjustments(1, Direction.RIGHT)
                    { Player((0 to it.position.second), Direction.RIGHT, 2) }

                    // Top of 2 is bottom of 6.
                    out.registerSideAdjustments(2, Direction.UP)
                    { Player((it.position.first to cubeSideSize - 1), Direction.UP, 6) }
                    // Bottom of 2 is the Right of 3
                    out.registerSideAdjustments(2, Direction.DOWN)
                    { Player((cubeSideSize - 1 to it.position.first), Direction.LEFT, 3) }
                    // Left of 2 is right of 1
                    out.registerSideAdjustments(2, Direction.LEFT)
                    { Player((cubeSideSize - 1 to it.position.second), Direction.LEFT, 1) }
                    // Right of 2 is the right of 5
                    out.registerSideAdjustments(2, Direction.RIGHT)
                    { Player((cubeSideSize - 1 to cubeSideSize - 1 - it.position.second), Direction.LEFT, 5) }

                    // Top of 3 is bottom of 1
                    out.registerSideAdjustments(3, Direction.UP)
                    { Player((it.position.first to cubeSideSize - 1), Direction.UP, 1) }
                    // Bottom of 3 is top of 5
                    out.registerSideAdjustments(3, Direction.DOWN)
                    { Player((it.position.first to 0), Direction.DOWN, 5) }
                    // Left of 3 is top of 4
                    out.registerSideAdjustments(3, Direction.LEFT)
                    { Player((it.position.second to 0), Direction.DOWN, 4) }
                    // Right of 3 is bottom of 2
                    out.registerSideAdjustments(3, Direction.RIGHT)
                    { Player((it.position.second to cubeSideSize - 1), Direction.UP, 2) }

                    // Top of 4 is left of 3
                    out.registerSideAdjustments(4, Direction.UP)
                    { Player((0 to it.position.first), Direction.RIGHT, 3) }
                    // Bottom of 4 is top of 6
                    out.registerSideAdjustments(4, Direction.DOWN)
                    { Player((it.position.first to 0), Direction.DOWN, 6) }
                    // Left of 4 is left of 1
                    out.registerSideAdjustments(4, Direction.LEFT)
                    { Player((0 to cubeSideSize - 1 - it.position.second), Direction.RIGHT, 1) }
                    // Right of 4 is left of 5
                    out.registerSideAdjustments(4, Direction.RIGHT)
                    { Player((0 to it.position.second), Direction.RIGHT, 5) }

                    //Top of 5 is bottom of 3
                    out.registerSideAdjustments(5, Direction.UP)
                    { Player((it.position.first to cubeSideSize - 1), Direction.UP, 3) }
                    //Bottom of 5 is right of 6
                    out.registerSideAdjustments(5, Direction.DOWN)
                    { Player((cubeSideSize - 1 to it.position.first), Direction.LEFT, 6) }
                    // Left of 5 is right of 4
                    out.registerSideAdjustments(5, Direction.LEFT)
                    { Player((cubeSideSize - 1 to it.position.second), Direction.LEFT, 4) }
                    // Right of 5 is right of 2
                    out.registerSideAdjustments(5, Direction.RIGHT)
                    { Player((cubeSideSize - 1 to cubeSideSize - 1 - it.position.second), Direction.LEFT, 2) }

                    // Top of 6 is bottom of 4
                    out.registerSideAdjustments(6, Direction.UP)
                    { Player((it.position.first to cubeSideSize - 1), Direction.UP, 4) }
                    // Bottom of 6 is top of 2
                    out.registerSideAdjustments(6, Direction.DOWN)
                    { Player((it.position.first to 0), Direction.DOWN, 2) }
                    // Left of 6 is top of 1
                    out.registerSideAdjustments(6, Direction.LEFT)
                    { Player((it.position.second to 0), Direction.DOWN, 1) }
                    // Right of 6 is bottom of 5
                    out.registerSideAdjustments(6, Direction.RIGHT)
                    { Player((it.position.second to cubeSideSize - 1), Direction.UP, 5) }
                }
                return out
            }
        }
    }

    data class Player(var position: IntPair, var direction: Direction, var side: Int = 0) {
        fun step(map: Map2D) {
            position = map.attemptMove(this, position + direction.step)
        }

        fun step(map: Map3D) {
            val attemptedPosition = position + direction.step

            val moveResult = map.attemptMove(this, attemptedPosition)
            position = moveResult.position
            direction = moveResult.direction
            side = moveResult.side
        }

        fun spin(spinDirection: String) {
            direction = direction.spin(spinDirection)
        }

        fun getPointValue(): Int {
            return (1000 * position.second) + (4 * position.first) + direction.answerValue
        }

        fun getPointValue(cubeMap: Map3D): Int {
            val side = cubeMap.sides[side]
            val resolvedPosition = position.first + side.first.first to position.second + side.second.first
            return (1000 * resolvedPosition.second) + (4 * resolvedPosition.first) + direction.answerValue
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

    private fun getCoordinateValue(map: Map3D, instructions: List<Instruction>): Int {
        val player = map.newPlayer()
        for (instruction in instructions) {
            repeat(instruction.amount) { player.step(map) }
            player.spin(instruction.spin)
        }
        return player.getPointValue(map)
    }

    fun scenarioOne(textFile: String) =
        File(textFile).readLines()
            .let { parse2DMapAndInstructions(it) }
            .let { getCoordinateValue(it.first, it.second) }

    fun scenarioTwo(textFile: String, cubeSideSize: Int) =
        File(textFile).readLines()
            .let { parse3DMapAndInstructions(it, cubeSideSize) }
            .let { getCoordinateValue(it.first, it.second) }
}
