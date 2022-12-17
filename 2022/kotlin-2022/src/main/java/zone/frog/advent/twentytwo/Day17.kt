package zone.frog.advent.twentytwo

import java.io.File


object Day17 {
    const val CHAMBER_WIDTH = 7

    interface Piece {
        fun shift(direction: Char, pieces: Map<LongPair, Piece>)
        fun drop(pieces: Map<LongPair, Piece>, floor: Long): Boolean
        fun settle(pieces: MutableMap<LongPair, Piece>): Collection<Long>
    }

    data class HorizontalLine(var x: Long = 2, var y: Long) : Piece {
        override fun shift(direction: Char, pieces: Map<LongPair, Piece>) {
            val movingRight = direction == '>'
            val toCheck = if (movingRight) x + 4 else x - 1
            if (toCheck in 0 until CHAMBER_WIDTH && !pieces.contains(toCheck to y)) {
                x += 1 * if (movingRight) 1 else -1
            }
        }

        override fun drop(pieces: Map<LongPair, Piece>, floor: Long): Boolean {
            val newY = y - 1
            if (newY == floor) {
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

        override fun settle(pieces: MutableMap<LongPair, Piece>): List<Long> {
            for (location in x until x + 4) {
                pieces[location to y] = this
            }
            return listOf(y)
        }
    }

    data class Cross(var x: Long = 2, var y: Long) : Piece {
        override fun shift(direction: Char, pieces: Map<LongPair, Piece>) {
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

        override fun drop(pieces: Map<LongPair, Piece>, floor: Long): Boolean {
            val newY = y - 1
            if (newY == floor) {
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

        override fun settle(pieces: MutableMap<LongPair, Piece>): List<Long> {
            pieces[x to y + 1] = this
            pieces[x + 1 to y] = this
            // Small optimization. Skip the middle piece as it has no bearing on collision handling.
            // pieces[x + 1 to y + 1] = this
            pieces[x + 1 to y + 2] = this
            pieces[x + 2 to y + 1] = this

            return listOf(y, y+1, y+2)
        }
    }

    data class ReverseL(var x: Long = 2, var y: Long) : Piece {
        override fun shift(direction: Char, pieces: Map<LongPair, Piece>) {
            val movingRight = direction == '>'
            if (!movingRight) {
                val leftCheck = x - 1 to y
                if (leftCheck.first >= 0 && !pieces.containsKey(leftCheck)) {
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

        override fun drop(pieces: Map<LongPair, Piece>, floor: Long): Boolean {
            val newY = y - 1
            if (newY == floor) {
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

        override fun settle(pieces: MutableMap<LongPair, Piece>): List<Long> {
            for (location in x until x + 3) {
                pieces[location to y] = this
            }
            for (location in y until y + 3) {
                pieces[x + 2 to location] = this
            }
            return (y until y + 3).toList()
        }
    }

    data class VerticalLine(var x: Long = 2, var y: Long) : Piece {
        override fun shift(direction: Char, pieces: Map<LongPair, Piece>) {
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

        override fun drop(pieces: Map<LongPair, Piece>, floor: Long): Boolean {
            val newY = y - 1
            if (newY == floor || pieces.contains(x to newY)) {
                return false
            }
            y = newY
            return true
        }

        override fun settle(pieces: MutableMap<LongPair, Piece>): List<Long> {
            for (location in y until y + 4) {
                pieces[x to location] = this
            }
            return (y until y + 4).toList()
        }
    }

    data class Square(var x: Long = 2, var y: Long) : Piece {
        override fun shift(direction: Char, pieces: Map<LongPair, Piece>) {
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

        override fun drop(pieces: Map<LongPair, Piece>, floor: Long): Boolean {
            val newY = y - 1
            if (newY == floor) {
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

        override fun settle(pieces: MutableMap<LongPair, Piece>): List<Long> {
            pieces[x + 1 to y] = this
            pieces[x + 1 to y + 1] = this
            pieces[x to y] = this
            pieces[x to y + 1] = this

            return listOf(y, y+1)
        }
    }

    enum class RockPattern {
        HORIZONTAL_LINE,
        CROSS,
        REVERSE_L,
        VERTICAL_LINE,
        SQUARE;

        fun create(pieces: Map<LongPair, Piece>): Piece {
            val y = (pieces.maxOfOrNull { it.key.second } ?: 0) + 3 + 1
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
            line.forEach {
                yield(it)
            }
        }
    }
//
//    fun printState(falling: Piece, pieces: Map<LongPair, Piece>) {
//        val tempState = HashMap(pieces)
//        falling.settle(tempState)
//        for (y in 0..tempState.maxOf { it.key.second }) {
//            for (x in 0 until CHAMBER_WIDTH)
//        }
//    }

    fun dropRocks(wind: Sequence<Char>, toDrop: Long): Long {
        var toDrop = toDrop
        val wind = wind.iterator()
        val pieces = mutableMapOf<LongPair, Piece>()
        var floor = 0L

        rockSequence()
            .takeWhile { toDrop-- > 0 }
            .forEach { rockType ->
                var settled = false
                val rock = rockType.create(pieces)
                while (!settled) {
                    rock.shift(wind.next(), pieces)
                    settled = !rock.drop(pieces, floor)
                }
                val floorRange = rock.settle(pieces)
//                val newFloor = floorRange
//                    .filter { y ->
//                        (0L until CHAMBER_WIDTH).all { x -> pieces.contains(x to y) }
//                    }
//                    .maxOrNull()
//
//                val newFloor = pieces.keys
//                    .mapNotNull { it.second.takeIf { it in floorRange } }
//                    .distinct()
//                    .filter { y ->
//                        (0L until CHAMBER_WIDTH).all { x -> pieces.contains(x to y) }
//                    }
//                    .max().orElse(-1)
//                if(newFloor != null) {
//                    println("Raising floor to $newFloor")
//                    pieces.keys.filter { it.second <= newFloor }.toSet().forEach {
//                        pieces.remove(it)
//                    }
//                    floor = newFloor
//                }
            }

        return pieces.maxOf { it.key.second }
    }

    fun scenarioOne(textFile: String) =
        File(textFile).readText()
            .let { dropRocks(windSequence(it), 2022L) }

    fun scenarioTwo(textFile: String) =
        File(textFile).readText()
            .let { dropRocks(windSequence(it), 1000000000000L) }
}
