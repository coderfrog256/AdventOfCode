package zone.frog.advent.twentytwo

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import zone.frog.advent.twentytwo.Day24.scenarioOne
import zone.frog.advent.twentytwo.Day24.scenarioTwo

class Day24Test {
    @Test
    internal fun testOne() {
        assertEquals(18, scenarioOne("../input/day24-test.txt"))
    }

    @Test
    internal fun testScenarioOne() {
        assertEquals(271, scenarioOne("../input/day24.txt"))
    }

    @Test
    internal fun testTwo() {
        assertEquals(54, scenarioTwo("../input/day24-test.txt"))
    }

    @Test
    internal fun testScenarioTwo() {
        assertEquals(813, scenarioTwo("../input/day24.txt"))
    }
}