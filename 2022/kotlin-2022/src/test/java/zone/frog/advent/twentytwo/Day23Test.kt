package zone.frog.advent.twentytwo

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import zone.frog.advent.twentytwo.Day23.scenarioOne
import zone.frog.advent.twentytwo.Day23.scenarioTwo

class Day23Test {
    @Test
    internal fun testOne() {
        assertEquals(110, scenarioOne("../input/day23-test.txt"))
    }

    @Test
    internal fun testScenarioOne() {
        assertEquals(4109, scenarioOne("../input/day23.txt"))
    }

    @Test
    internal fun testTwo() {
        assertEquals(20, scenarioTwo("../input/day23-test.txt"))
    }

    @Test
    internal fun testScenarioTwo() {
        assertEquals(1055, scenarioTwo("../input/day23.txt"))
    }
}