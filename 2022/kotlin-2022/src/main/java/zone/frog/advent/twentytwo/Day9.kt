package zone.frog.advent.twentytwo

import java.io.File
import java.lang.Math.abs

// NOT: 5168
typealias IntPair = Pair<Int,Int>
object Day9 {
    fun runSteps(lines: List<String>): Int {
        fun newPosition(tail: IntPair, head: IntPair): IntPair {
            val xDiff = abs(tail.first - head.first)
            val yDiff = abs(tail.second - head.second)
            return when {
                tail == head -> tail
                xDiff == 1 && tail.second == head.second -> tail
                yDiff == 1 && tail.first == head.first -> tail
                xDiff == 1 && yDiff == 1 -> tail
                tail.first < head.first && tail.second == head.second -> head.copy(first = head.first-1)
                tail.first > head.first && tail.second == head.second -> head.copy(first = head.first+1)
                tail.second < head.second && tail.first == head.first -> head.copy(second = head.second-1)
                tail.second > head.second && tail.first == head.first -> head.copy(second = head.second+1)
                yDiff == 2 && xDiff == 1 -> head.copy(second = head.second+(if (head.second > tail.second) - 1 else 1))
                xDiff == 2 && yDiff == 1 -> head.copy(first = head.first+(if (head.first > tail.first) - 1 else 1))
                else -> throw IllegalStateException()
            }
        }

        val visited = mutableSetOf(0 to 0)

        var headPosition = 0 to 0
        var tailPosition = 0 to 0

        for (line in lines) {
            val (direction, count) = line.split(" ")
            repeat(count.toInt()) {
                when(direction) {
                    "R" -> headPosition = headPosition.copy(first = headPosition.first+1)
                    "L" -> headPosition = headPosition.copy(first = headPosition.first-1)
                    "U" -> headPosition = headPosition.copy(second = headPosition.second+1)
                    "D" -> headPosition = headPosition.copy(second = headPosition.second-1)
                }
                tailPosition = newPosition(tailPosition, headPosition)
                visited.add(tailPosition)
            }
        }
        return visited.size
    }

    fun scenarioOne(textFile: String) =
        File(textFile)
            .readLines()
            .let { runSteps(it) }

    fun scenarioTwo(textFile: String) =
        File(textFile)
//            .readLines()
}