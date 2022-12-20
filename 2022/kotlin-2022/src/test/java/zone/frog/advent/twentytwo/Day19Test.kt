package zone.frog.advent.twentytwo

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import zone.frog.advent.twentytwo.Day19.scenarioOne
import zone.frog.advent.twentytwo.Day19.scenarioTwo

class Day19Test {
    @Test
    internal fun testOne() {
        assertEquals(33, scenarioOne("../input/day19-test.txt"))
    }

    @Test
    internal fun testScenarioOne() {
        assertEquals(1480, scenarioOne("../input/day19.txt"))
    }

    @Test
    internal fun testTwo() {
        assertEquals(62 * 56, scenarioTwo("../input/day19-test.txt", 2))
    }

    @Test
    internal fun testScenarioTwo() {
        assertEquals(3168, scenarioTwo("../input/day19.txt", 3))
    }
}