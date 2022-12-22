package zone.frog.advent.twentytwo

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import zone.frog.advent.twentytwo.Day22.scenarioOne
import zone.frog.advent.twentytwo.Day22.scenarioTwo

class Day22Test {
    @Test
    internal fun testOne() {
        assertEquals(6032, scenarioOne("../input/day22-test.txt"))
    }

    @Test
    internal fun testScenarioOne() {
        assertEquals(77318, scenarioOne("../input/day22.txt"))
    }

    @Test
    internal fun testTwo() {
        assertEquals(5031, scenarioTwo("../input/day22-test.txt", 4))
    }

    @Test
    internal fun testScenarioTwo() {
        assertEquals(126017, scenarioTwo("../input/day22.txt", 50))
    }
}