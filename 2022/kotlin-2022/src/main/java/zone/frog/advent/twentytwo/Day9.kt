package zone.frog.advent.twentytwo

import java.io.File
import java.lang.Math.abs

object Day9 {
    data class Knot(val x: Int = 0, val y: Int = 0, val next: Knot? = null) {
        fun move(direction: String): Knot {
            val newX = when {
                direction.endsWith("R") -> x + 1
                direction.endsWith("L") -> x - 1
                else -> x
            }
            val newY = when {
                direction.startsWith("U") -> y + 1
                direction.startsWith("D") -> y - 1
                else -> y
            }
            val newNext = next?.let {
                val xDiff = abs(next.x - newX)
                val yDiff = abs(next.y - newY)
                if (xDiff < 2 && yDiff < 2) {
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

        private fun last(): Knot = next?.last() ?: this
        fun lastPosition(): Pair<Int, Int> = last().let { it.x to it.y }
    }

    private fun runSteps(lines: List<String>, knots: Int): Int {
        // Build up our nested list of knots.
        val head = (0 until knots - 1)
            .fold(Knot()) { knot, _ ->
                Knot(next = knot)
            }

        // Loop through the lines, keeping track of our list and where the last element has visited.
        return lines.fold(head to setOf(0 to 0)) { visitedHeads, line ->
            val (command, times) = line.split(" ")
            command.repeat(times.toInt())
                .fold(visitedHeads) { head, direction ->
                    val newHead = head.first.move(direction.toString())
                    newHead to head.second + newHead.lastPosition()
                }
        }.second.size
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