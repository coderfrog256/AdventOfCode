package zone.frog.advent.twentytwo

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import zone.frog.advent.twentytwo.Day18.scenarioOne
import zone.frog.advent.twentytwo.Day18.scenarioTwo
import java.io.File

class Day18Test {
    @Test
    internal fun testOneMini() {
        assertEquals(10, scenarioOne("../input/day18-test-mini.txt"))
    }

    @Test
    internal fun testOne() {
        assertEquals(64, scenarioOne("../input/day18-test.txt"))
    }

    @Test
    internal fun testScenarioOne() {
        assertEquals(3412, scenarioOne("../input/day18.txt"))
    }

    @Test
    internal fun testTwo() {
        assertEquals(58, scenarioTwo("../input/day18-test.txt"))
    }

    @Test
    internal fun testScenarioTwo() {
        assertEquals(2018, scenarioTwo("../input/day18.txt"))
    }
}