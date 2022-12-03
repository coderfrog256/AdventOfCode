package zone.frog.advent.twentytwo

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import zone.frog.advent.twentytwo.Day3.scenarioOne
import zone.frog.advent.twentytwo.Day3.scenarioTwo

class Day3Test {
    @Test
    internal fun testOne() {
        assertEquals(157,scenarioOne("../input/day3-test.txt"))
    }

    @Test
    internal fun testScenarioOne() {
        assertEquals(8394,scenarioOne("../input/day3.txt"))
    }

    @Test
    internal fun testTwo() {
        assertEquals(70, scenarioTwo("../input/day3-test.txt"))
    }

    @Test
    internal fun testScenarioTwo() {
        assertEquals(2413, scenarioTwo("../input/day3.txt"))
    }
}