package zone.frog.advent.twentytwo

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import zone.frog.advent.twentytwo.Day14.scenarioOne
import zone.frog.advent.twentytwo.Day14.scenarioTwo

class Day14Test {
    @Test
    internal fun testOne() {
        assertEquals(24, scenarioOne("../input/day14-test.txt"))
    }

    @Test
    internal fun testScenarioOne() {
        assertEquals(808, scenarioOne("../input/day14.txt"))
    }

    @Test
    internal fun testTwo() {
        assertEquals(93, scenarioTwo("../input/day14-test.txt"))
    }

    @Test
    internal fun testScenarioTwo() {
        assertEquals(26625, scenarioTwo("../input/day14.txt"))
    }
}