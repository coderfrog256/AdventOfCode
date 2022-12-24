package zone.frog.advent.twentytwo

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import zone.frog.advent.twentytwo.Day24.scenarioOne
import zone.frog.advent.twentytwo.Day24.scenarioTwo

class Day24Test {
    @Test
    internal fun testOne() {
        println(scenarioOne("../input/day24-test.txt"))
    }

    @Test
    internal fun testScenarioOne() {
        println(scenarioOne("../input/day24.txt"))
    }

    @Test
    internal fun testTwo() {
        println(scenarioTwo("../input/day24-test.txt"))
    }

    @Test
    internal fun testScenarioTwo() {
        println(scenarioTwo("../input/day24.txt"))
    }
}