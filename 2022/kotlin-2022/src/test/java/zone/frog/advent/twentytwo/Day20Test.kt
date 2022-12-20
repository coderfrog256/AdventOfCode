package zone.frog.advent.twentytwo

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import zone.frog.advent.twentytwo.Day20.scenarioOne
import zone.frog.advent.twentytwo.Day20.scenarioTwo

class Day20Test {
    @Test
    internal fun testOne() {
        assertEquals(3, scenarioOne("../input/day20-test.txt"))
    }

    @Test
    internal fun testScenarioOne() {
        assertEquals(14526, scenarioOne("../input/day20.txt"))
    }

    @Test
    internal fun testTwo() {
        assertEquals(1623178306, scenarioTwo("../input/day20-test.txt"))
    }

    @Test
    internal fun testScenarioTwo() {
        assertEquals(9738258246847, scenarioTwo("../input/day20.txt"))
    }
}