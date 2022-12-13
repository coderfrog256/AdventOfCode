package zone.frog.advent.twentytwo

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import zone.frog.advent.twentytwo.Day13.scenarioOne
import zone.frog.advent.twentytwo.Day13.scenarioTwo

class Day13Test {
    @Test
    internal fun testOne() {
        assertEquals(13, scenarioOne("../input/day13-test.txt"))
    }

    @Test
    internal fun testScenarioOne() {
        assertEquals(6568, scenarioOne("../input/day13.txt"))
    }

    @Test
    internal fun testTwo() {
        assertEquals(140, scenarioTwo("../input/day13-test.txt"))
    }

    @Test
    internal fun testScenarioTwo() {
        assertEquals(19493, scenarioTwo("../input/day13.txt"))
    }
}