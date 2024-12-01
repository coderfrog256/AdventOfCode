package zone.frog.advent.twentyfour

import org.junit.jupiter.api.Assertions
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import zone.frog.advent.twentyfour.Day1.scenarioOne
import zone.frog.advent.twentyfour.Day1.scenarioTwo

class Day1Test {
    @Test
    internal fun testOne() {
        assertEquals(11, scenarioOne("../input/day1-test.txt"))
    }

    @Test
    internal fun testScenarioOne() {
        println(scenarioOne("../input/day1.txt"))
    }

    @Test
    internal fun testTwo() {
        assertEquals(31, scenarioTwo("../input/day1-test.txt"))
    }

    @Test
    internal fun testScenarioTwo() {
        println(scenarioTwo("../input/day1.txt"))
    }
}