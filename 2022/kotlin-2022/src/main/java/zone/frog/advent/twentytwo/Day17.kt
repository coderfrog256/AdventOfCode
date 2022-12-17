package zone.frog.advent.twentytwo

import java.io.File
import java.lang.Long.max
import java.util.stream.Collectors


object Day17 {
    const val CHAMBER_WIDTH = 7

    data class MemoKey(val windOffset: Int, val rockType: RockPattern, val remaining: Set<LongPair>)
    data class MemoValue(val windTicks: Int, val rocksDropped: Int, val floorOffset: Long, val remaining: Set<LongPair>)

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

    private fun rockSequence() = sequence {
        while (true) {
            RockPattern.values().forEach {
                yield(it)
            }
        }
    }

    private fun windSequence(line: String) = sequence {
        while (true) {
            line.forEachIndexed { i, c ->
                yield(i to c)
            }
        }
    }
//
//    fun printState(falling: Piece, pieces: Set<LongPair>) {
//        val tempState = HashMap(pieces)
//        falling.settle(tempState)
//        for (y in 0..tempState.maxOf { it.key.second }) {
//            for (x in 0 until CHAMBER_WIDTH)
//        }
//    }

    fun dropRocks(wind: Sequence<Pair<Int,Char>>, toDrop: Long): Long {
        var toDrop = toDrop
        val wind = wind.iterator()
        var pieces = mutableSetOf<LongPair>()
        var floor = 0L
        var stackTop = 0L

        val memo = mutableMapOf<MemoKey, MemoValue>()
        var activeKey: MemoKey? = null
        var windTicks = 0
        var rocksToSkip = 0

        rockSequence()
            .takeWhile { toDrop-- > 0 }
            .forEach { rockType ->
                if(rocksToSkip > 0) {
                    rocksToSkip--
                    return@forEach
                }
                var settled = false
                val rock = rockType.create(stackTop)
                while (!settled) {
                    ++windTicks
                    val (windOffset, direction) = wind.next()
                    if(activeKey == null) {
                        activeKey = MemoKey(windOffset, rockType, HashSet(pieces))

                        val memoized = memo[activeKey]
                        if(memoized != null) {
                            repeat(memoized.windTicks-1) {wind.next()}
                            rocksToSkip = memoized.rocksDropped-1
                            floor += memoized.floorOffset
                            pieces = HashSet(memoized.remaining)
                        }
                    }

                    rock.shift(direction, pieces)
                    settled = !rock.drop(pieces)
                }
                val impactedRows = rock.settle(pieces)
                val newFloor = impactedRows
                    .filter { y ->
                        (0L until CHAMBER_WIDTH).all { x -> pieces.contains(x to y) }
                    }
                    .maxOrNull()
                stackTop = max(stackTop, impactedRows.max())

                if (newFloor != null) {
                    println("Raising floor to ${newFloor + floor}")
                    pieces = pieces.parallelStream()
                        .map { it.copy(second=it.second - newFloor) }
                        .filter { it.second >= 0L }
                        .collect(Collectors.toSet())
                    floor += newFloor
                    stackTop -= newFloor
                }
            }

        return pieces.maxOf { it.second } + floor
    }

    fun scenarioOne(textFile: String) =
        File(textFile).readText()
            .let { dropRocks(windSequence(it), 2022L) }

    fun scenarioTwo(textFile: String) =
        File(textFile).readText()
            .let { dropRocks(windSequence(it), 1000000000000L) }
}
