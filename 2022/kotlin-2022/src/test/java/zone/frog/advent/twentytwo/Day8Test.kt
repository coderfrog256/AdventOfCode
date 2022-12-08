package zone.frog.advent.twentytwo

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import zone.frog.advent.twentytwo.Day8.scenarioOne
import zone.frog.advent.twentytwo.Day8.scenarioTwo

class Day8Test {
    @Test
    internal fun testOne() {
        assertEquals(21, scenarioOne("../input/day8-test.txt"))
    }

    @Test
    internal fun testScenarioOne() {
        assertEquals(1700, scenarioOne("../input/day8.txt"))
    }

    @Test
    internal fun testTwo() {
        assertEquals(8, scenarioTwo("../input/day8-test.txt"))
    }

    @Test
    internal fun testScenarioTwo() {
        assertEquals(470596, scenarioTwo("../input/day8.txt"))
    }
}