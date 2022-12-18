package zone.frog.advent.twentytwo

import java.io.File
import java.util.stream.Collectors


object Day17 {
    const val CHAMBER_WIDTH = 7

    data class MemoKey(val windIndex: Int, val rockIndex: Int, val remaining: Set<LongPair>)
    data class MemoValue(
        val windIndex: Int,
        val rockIndex: Int,
        val rocksDropped: Long,
        val floorOffset: Long,
        val remaining: Set<LongPair>,
        val nextKey: MemoKey
    )

    interface Piece {
        fun shift(direction: Char, pieces: Set<LongPair>)
        fun drop(pieces: Set<LongPair>): Boolean
        fun settle(pieces: MutableSet<LongPair>): Collection<Long>
    }

    data class HorizontalLine(var x: Long = 2, var y: Long) : Piece {
        override fun shift(direction: Char, pieces: Set<LongPair>) {
            val movingRight = direction == '>'
            val toCheck = if (movingRight) x + 4 else x - 1
            if (toCheck in 0 until CHAMBER_WIDTH && !pieces.contains(toCheck to y)) {
                x += 1 * if (movingRight) 1 else -1
            }
        }

        override fun drop(pieces: Set<LongPair>): Boolean {
            val newY = y - 1
            if (newY == 0L) {
                return false
            }

            for (downX in (x until x + 4)) {
                if (pieces.contains(downX to newY)) {
                    return false
                }
            }
            y = newY
            return true
        }

        override fun settle(pieces: MutableSet<LongPair>): List<Long> {
            for (location in x until x + 4) {
                pieces.add(location to y)
            }
            return listOf(y)
        }
    }

    data class Cross(var x: Long = 2, var y: Long) : Piece {
        override fun shift(direction: Char, pieces: Set<LongPair>) {
            val movingRight = direction == '>'
            val topCheck = (x + 1 + if (movingRight) 1 else -1) to y + 2
            val middleCheck = (x + if (movingRight) 3 else -1) to y + 1
            val bottomCheck = (x + 1 + if (movingRight) 1 else -1) to y

            if (middleCheck.first !in 0 until CHAMBER_WIDTH) {
                return
            }

            if (listOf(topCheck, middleCheck, bottomCheck).none { pieces.contains(it) }) {
                x += 1 * if (movingRight) 1 else -1
            }
        }

        override fun drop(pieces: Set<LongPair>): Boolean {
            val newY = y - 1
            if (newY == 0L) {
                return false
            }

            val leftCheck = x to y
            val middleCheck = x + 1 to newY
            val rightCheck = x + 2 to y
            if (listOf(leftCheck, middleCheck, rightCheck).none { pieces.contains(it) }) {
                y = newY
                return true
            }
            return false
        }

        override fun settle(pieces: MutableSet<LongPair>): List<Long> {
            pieces.add(x to y + 1)
            pieces.add(x + 1 to y)
            // Small optimization. Skip the middle piece as it has no bearing on collision handling.
            // pieces.add(x + 1 to y + 1)
            pieces.add(x + 1 to y + 2)
            pieces.add(x + 2 to y + 1)

            return listOf(y, y + 1, y + 2)
        }
    }

    data class ReverseL(var x: Long = 2, var y: Long) : Piece {
        override fun shift(direction: Char, pieces: Set<LongPair>) {
            val movingRight = direction == '>'
            if (!movingRight) {
                val leftCheck = x - 1 to y
                if (leftCheck.first >= 0 && !pieces.contains(leftCheck)) {
                    x -= 1
                }
            } else {
                val rightWall = x + 3
                if (rightWall >= CHAMBER_WIDTH) {
                    return
                }

                for (yCheck in (y until y + 3)) {
                    if (pieces.contains(rightWall to yCheck)) {
                        return
                    }
                }
                x += 1
            }
        }

        override fun drop(pieces: Set<LongPair>): Boolean {
            val newY = y - 1
            if (newY == 0L) {
                return false
            }

            for (downX in (x until x + 3)) {
                if (pieces.contains(downX to newY)) {
                    return false
                }
            }
            y = newY
            return true
        }

        override fun settle(pieces: MutableSet<LongPair>): List<Long> {
            for (location in x until x + 3) {
                pieces.add(location to y)
            }
            for (location in y until y + 3) {
                pieces.add(x + 2 to location)
            }
            return (y until y + 3).toList()
        }
    }

    data class VerticalLine(var x: Long = 2, var y: Long) : Piece {
        override fun shift(direction: Char, pieces: Set<LongPair>) {
            val movingRight = direction == '>'
            val sideCheck = x + 1 * if (movingRight) 1 else -1
            if (sideCheck !in 0 until CHAMBER_WIDTH) {
                return
            }

            for (yCheck in (y until y + 4)) {
                if (pieces.contains(sideCheck to yCheck)) {
                    return
                }
            }
            x += 1 * if (movingRight) 1 else -1
        }

        override fun drop(pieces: Set<LongPair>): Boolean {
            val newY = y - 1
            if (newY == 0L || pieces.contains(x to newY)) {
                return false
            }
            y = newY
            return true
        }

        override fun settle(pieces: MutableSet<LongPair>): List<Long> {
            for (location in y until y + 4) {
                pieces.add(x to location)
            }
            return (y until y + 4).toList()
        }
    }

    data class Square(var x: Long = 2, var y: Long) : Piece {
        override fun shift(direction: Char, pieces: Set<LongPair>) {
            val movingRight = direction == '>'
            val sideCheck = if (movingRight) x + 2 else x - 1
            if (sideCheck !in 0 until CHAMBER_WIDTH) {
                return
            }

            for (yCheck in (y until y + 2)) {
                if (pieces.contains(sideCheck to yCheck)) {
                    return
                }
            }
            x += 1 * if (movingRight) 1 else -1
        }

        override fun drop(pieces: Set<LongPair>): Boolean {
            val newY = y - 1
            if (newY == 0L) {
                return false
            }

            for (downX in (x until x + 2)) {
                if (pieces.contains(downX to newY)) {
                    return false
                }
            }
            y = newY
            return true
        }

        override fun settle(pieces: MutableSet<LongPair>): List<Long> {
            pieces.add(x + 1 to y)
            pieces.add(x + 1 to y + 1)
            pieces.add(x to y)
            pieces.add(x to y + 1)

            return listOf(y, y + 1)
        }
    }

    enum class RockPattern {
        HORIZONTAL_LINE,
        CROSS,
        REVERSE_L,
        VERTICAL_LINE,
        SQUARE;

        fun create(stackTop: Long): Piece {
            val y = stackTop + 3 + 1
            return when (this) {
                HORIZONTAL_LINE -> HorizontalLine(y = y)
                CROSS -> Cross(y = y)
                REVERSE_L -> ReverseL(y = y)
                VERTICAL_LINE -> VerticalLine(y = y)
                SQUARE -> Square(y = y)
            }
        }
    }

    fun dropRocks(input: String, totalToDrop: Long): Long {
        var remainingToDrop = totalToDrop
        var rockOffset = 0
        var windOffset = 0

        fun nextRock(): RockPattern {
            rockOffset %= RockPattern.values().size
            return RockPattern.values()[rockOffset++]
        }

        fun nextWind(): Char {
            windOffset %= input.length
            return input[windOffset++]
        }

        var pieces = mutableSetOf<LongPair>()
        var floor = 0L

        val memo = mutableMapOf<MemoKey, MemoValue>()
        var rocksDropped = 0
        var activeKey = MemoKey(windOffset, rockOffset, HashSet(pieces))

        drop@ while (remainingToDrop-- > 0) {
            val rock = nextRock().create(pieces.maxOfOrNull { it.second } ?: 0)
            ++rocksDropped

            var settled = false
            while (!settled) {
                val direction = nextWind()

                // Check if there's a memoized update for this configuration.
                // Only use it if it won't push us above the number of rocks we need to drop...
                val memoized = memo[activeKey]
                if (memoized != null && memoized.rocksDropped < remainingToDrop) {
                    println("Jumping ${memoized.rocksDropped} rocks. Remaining=${remainingToDrop - memoized.rocksDropped}")

                    // Try to merge memos. If we know this and its next, make this point to next's result.
                    // Note once a memo merges into itself this grows VERY fast.
                    val nextMemo = memo[memoized.nextKey]
                    if (nextMemo != null && memoized.rocksDropped + nextMemo.rocksDropped < remainingToDrop) {
                        memo[activeKey] = MemoValue(
                            windIndex = nextMemo.windIndex,
                            rockIndex = nextMemo.rockIndex,
                            rocksDropped = memoized.rocksDropped + nextMemo.rocksDropped,
                            floorOffset = memoized.floorOffset + nextMemo.floorOffset,
                            remaining = nextMemo.remaining,
                            nextKey = nextMemo.nextKey
                        )
                    }

                    windOffset = memoized.windIndex
                    rockOffset = memoized.rockIndex
                    remainingToDrop -= memoized.rocksDropped - 1
                    floor += memoized.floorOffset
                    pieces = HashSet(memoized.remaining)
                    rocksDropped = 0
                    activeKey = memoized.nextKey
                    continue@drop
                }
                rock.shift(direction, pieces)
                settled = !rock.drop(pieces)
            }

            val newFloor = rock.settle(pieces)
                .filter { y -> (0L until CHAMBER_WIDTH).all { x -> pieces.contains(x to y) } }
                .maxOrNull()

            if (newFloor != null) {
                println("Raising floor to ${newFloor + floor} Remaining=${remainingToDrop}")
                pieces = pieces
                    .map { it.copy(second = it.second - newFloor) }
                    .filter { it.second >= 0L }
                    .toMutableSet()
                floor += newFloor

                val newKey = MemoKey(windOffset, rockOffset, HashSet(pieces))
                memo[activeKey] =
                    MemoValue(windOffset, rockOffset, rocksDropped.toLong(), newFloor, HashSet(pieces), newKey)
                activeKey = newKey
                rocksDropped = 0
            }
        }

        return pieces.maxOf { it.second } + floor
    }

    fun scenarioOne(textFile: String) =
        File(textFile).readText()
            .let { dropRocks(it, 2022L) }

    fun scenarioTwo(textFile: String) =
        File(textFile).readText()
            .let { dropRocks(it, 1000000000000L) }
}
