package zone.frog.advent.twentytwo

import java.io.File
import java.lang.Math.abs

object Day9 {
    data class Knot(val x: Int = 0, val y: Int = 0, val next: Knot? = null) {
        fun move(direction: String): Knot {
            val newX = when (direction) {
                "R", "UR", "DR" -> x + 1
                "L", "UL", "DL" -> x - 1
                else -> x
            }
            val newY = when (direction) {
                "U", "UR", "UL" -> y + 1
                "D", "DR", "DL" -> y - 1
                else -> y
            }
            val newNext = next?.let {
                val xDiff = abs(next.x - newX)
                val yDiff = abs(next.y - newY)
                if (
                    (next.x == newX && next.y == newY) ||
                    (xDiff == 1 && next.y == newY) ||
                    (yDiff == 1 && next.x == newX) ||
                    (xDiff == 1 && yDiff == 1)
                ) {
                    next // No need to move. At most one space away.
                } else {
                    val verticalMove = when {
                        newY > next.y -> "U"
                        newY == next.y -> ""
                        else -> "D"
                    }
                    val horizontalMove = when {
                        newX > next.x -> "R"
                        newX == next.x -> ""
                        else -> "L"
                    }
                    next.move(verticalMove + horizontalMove)
                }
            }
            return copy(x = newX, y = newY, next = newNext)
        }

        fun last(): Knot = next?.last() ?: this
    }

    private fun runSteps(lines: List<String>, knots: Int): Int {
        val visited = mutableSetOf(0 to 0)
        var head = Knot()
        repeat(knots - 1) {
            head = head.copy(next = head)
        }

        for (line in lines) {
            val (direction, count) = line.split(" ")
            repeat(count.toInt()) {
                head = head.move(direction)

                val last = head.last()
                visited.add(last.x to last.y)
            }
        }
        return visited.size
    }

    fun scenarioOne(textFile: String) =
        File(textFile)
            .readLines()
            .let { runSteps(it, 2) }

    fun scenarioTwo(textFile: String) =
        File(textFile)
            .readLines()
            .let { runSteps(it, 10) }
}