package zone.frog.advent.twentytwo

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import zone.frog.advent.twentytwo.Day2.scenarioOne
import zone.frog.advent.twentytwo.Day2.scenarioTwo

class Day2Test {
    @Test
    internal fun testOne() {
        assertEquals(15, scenarioOne("../input/day2-test.txt"))
    }

    @Test
    internal fun testScenarioOne() {
        assertEquals(11873, scenarioOne("../input/day2.txt"))
    }

    @Test
    internal fun testTwo() {
        assertEquals(12, scenarioTwo("../input/day2-test.txt"))
    }

    @Test
    internal fun testScenarioTwo() {
        assertEquals(12014, scenarioTwo("../input/day2.txt"))
    }
}