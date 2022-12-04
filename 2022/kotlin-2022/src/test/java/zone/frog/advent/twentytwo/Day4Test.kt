package zone.frog.advent.twentytwo

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import zone.frog.advent.twentytwo.Day4.scenarioOne
import zone.frog.advent.twentytwo.Day4.scenarioTwo

class Day4Test {
    @Test
    internal fun testOne() {
        assertEquals(2, scenarioOne("../input/day4-test.txt"))
    }

    @Test
    internal fun testScenarioOne() {
        assertEquals(453, scenarioOne("../input/day4.txt"))
    }

    @Test
    internal fun testTwo() {
        assertEquals(4, scenarioTwo("../input/day4-test.txt"))
    }

    @Test
    internal fun testScenarioTwo() {
        assertEquals(919, scenarioTwo("../input/day4.txt"))
    }
}