package zone.frog.advent.twentytwo

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import zone.frog.advent.twentytwo.Day9.scenarioOne
import zone.frog.advent.twentytwo.Day9.scenarioTwo

class Day9Test {
    @Test
    internal fun testOne() {
        assertEquals(13, scenarioOne("../input/day9-test.txt"))
    }

    @Test
    internal fun testScenarioOne() {
        assertEquals(6023, scenarioOne("../input/day9.txt"))
    }

    @Test
    internal fun testTwo() {
        assertEquals(36, scenarioTwo("../input/day9-test2.txt"))
    }

    @Test
    internal fun testScenarioTwo() {
        assertEquals(2533, scenarioTwo("../input/day9.txt"))
    }
}