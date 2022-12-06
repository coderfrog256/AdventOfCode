package zone.frog.advent.twentytwo

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import zone.frog.advent.twentytwo.Day6.scenarioOne
import zone.frog.advent.twentytwo.Day6.scenarioTwo

class Day6Test {
    @Test
    internal fun testOne() {
        assertEquals(7, scenarioOne("../input/day6-test.txt"))
    }

    @Test
    internal fun testScenarioOne() {
        assertEquals(1578, scenarioOne("../input/day6.txt"))
    }

    @Test
    internal fun testTwo() {
        assertEquals(19, scenarioTwo("../input/day6-test.txt"))
    }

    @Test
    internal fun testScenarioTwo() {
        assertEquals(2178, scenarioTwo("../input/day6.txt"))
    }
}