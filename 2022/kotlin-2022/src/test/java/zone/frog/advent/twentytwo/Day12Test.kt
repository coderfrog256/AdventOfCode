package zone.frog.advent.twentytwo

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import zone.frog.advent.twentytwo.Day12.scenarioOne
import zone.frog.advent.twentytwo.Day12.scenarioTwo

class Day12Test {
    @Test
    internal fun testOne() {
        assertEquals(31, scenarioOne("../input/day12-test.txt"))
    }

    @Test
    internal fun testScenarioOne() {
        assertEquals(472, scenarioOne("../input/day12.txt"))
    }

    @Test
    internal fun testTwo() {
        assertEquals(29, scenarioTwo("../input/day12-test.txt"))
    }

    @Test
    internal fun testScenarioTwo() {
        assertEquals(465, scenarioTwo("../input/day12.txt"))
    }
}