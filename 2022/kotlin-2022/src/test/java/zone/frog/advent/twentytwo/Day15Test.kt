package zone.frog.advent.twentytwo

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import zone.frog.advent.twentytwo.Day15.manhattanDistance
import zone.frog.advent.twentytwo.Day15.scenarioOne
import zone.frog.advent.twentytwo.Day15.scenarioTwo

class Day15Test {

    @Test
    fun testManhattan() {
        assertEquals(9, manhattanDistance(8L to 7L, 2L to 10L))
    }

    @Test
    internal fun testOne() {
        assertEquals(26, scenarioOne("../input/day15-test.txt", 10))
    }

    @Test
    internal fun testScenarioOne() {
        assertEquals(5100463, scenarioOne("../input/day15.txt", 2000000))
    }

    @Test
    internal fun testTwo() {
        assertEquals(56000011, scenarioTwo("../input/day15-test.txt", 20))
    }

    @Test
    internal fun testScenarioTwo() {
        assertEquals(11557863040754, scenarioTwo("../input/day15.txt", 4000000))
    }
}