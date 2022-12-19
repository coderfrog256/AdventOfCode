package zone.frog.advent.twentytwo

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import zone.frog.advent.twentytwo.Day19.scenarioOne
import zone.frog.advent.twentytwo.Day19.scenarioTwo
import java.io.File

class Day19Test {
    @Test
    internal fun testOne() {
        assertEquals(33, scenarioOne("../input/day19-test.txt"))
    }

    @Test
    internal fun testScenarioOne() {
        println(scenarioOne("../input/day19.txt"))
    }

    @Test
    internal fun testTwo() {
        println(scenarioTwo("../input/day19-test.txt"))
    }

    @Test
    internal fun testScenarioTwo() {
        println(scenarioTwo("../input/day19.txt"))
    }
}