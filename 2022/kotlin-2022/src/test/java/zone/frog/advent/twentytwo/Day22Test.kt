package zone.frog.advent.twentytwo

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import zone.frog.advent.twentytwo.Day22.scenarioOne
import zone.frog.advent.twentytwo.Day22.scenarioTwo

class Day22Test {
    @Test
    internal fun testOne() {
        println(scenarioOne("../input/day22-test.txt"))
    }

    @Test
    internal fun testScenarioOne() {
        println(scenarioOne("../input/day22.txt"))
    }

    @Test
    internal fun testTwo() {
        println(scenarioTwo("../input/day22-test.txt"))
    }

    @Test
    internal fun testScenarioTwo() {
        println(scenarioTwo("../input/day22.txt"))
    }
}