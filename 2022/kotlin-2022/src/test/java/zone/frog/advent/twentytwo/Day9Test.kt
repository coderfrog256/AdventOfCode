package zone.frog.advent.twentytwo

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import zone.frog.advent.twentytwo.Day9.scenarioOne
import zone.frog.advent.twentytwo.Day9.scenarioTwo

class Day9Test {
    @Test
    internal fun testOne() {
        println(scenarioOne("../input/day9-test.txt"))
    }

    @Test
    internal fun testScenarioOne() {
        println(scenarioOne("../input/day9.txt"))
    }

    @Test
    internal fun testTwo() {
        println(scenarioTwo("../input/day9-test.txt"))
    }

    @Test
    internal fun testScenarioTwo() {
        println(scenarioTwo("../input/day9.txt"))
    }
}