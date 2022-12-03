package zone.frog.advent.twentytwo

import org.junit.jupiter.api.Test
import zone.frog.advent.twentytwo.Day3.scenarioOne
import zone.frog.advent.twentytwo.Day3.scenarioTwo

class Day3Test {
    @Test
    internal fun testOne() {
        println(scenarioOne("../input/day3-test.txt"))
    }

    @Test
    internal fun testScenarioOne() {
        println(scenarioOne("../input/day3.txt"))
    }

    @Test
    internal fun testTwo() {
        println(scenarioTwo("../input/day3-test.txt"))
    }

    @Test
    internal fun testScenarioTwo() {
        println(scenarioTwo("../input/day3.txt"))
    }
}