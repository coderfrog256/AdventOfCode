package zone.frog.advent.fifteen

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import java.io.File

class Day24Test {
    @Test
    fun example() {
        assertEquals(
            99,
            getGroupOneQEScenarioOne(
                weightsFromFile("../input/day24-test.txt")
            )
        )
    }

    @Test
    fun scenario1() {
        assertEquals(
            10723906903,
            getGroupOneQEScenarioOne(
                weightsFromFile("../input/day24.txt")
            )
        )
    }

    @Test
    fun scenario2() {
        assertEquals(
            74850409,
            getGroupOneQEScenarioTwo(
                weightsFromFile("../input/day24.txt")
            )
        )
    }

    private fun weightsFromFile(file: String) = File(file)
        .readText()
        .split("\n")
        .filter { it.isNotBlank() }
        .map { it.toLong() }
}