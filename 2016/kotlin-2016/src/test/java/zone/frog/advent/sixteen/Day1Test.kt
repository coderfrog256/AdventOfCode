package zone.frog.advent.sixteen

import org.junit.jupiter.api.Test
import zone.frog.advent.sixteen.Day1.scenarioOne
import zone.frog.advent.sixteen.Day1.scenarioTwo

class Day1Test {
    @Test
    internal fun testOne() {
        println(scenarioOne("../input/day1-test.txt"))
    }

    @Test
    internal fun testScenarioOne() {
        println(scenarioOne("../input/day1.txt"))
    }

    @Test
    internal fun testTwo() {
        println(scenarioTwo("../input/day1-test.txt"))
    }

    @Test
    internal fun testScenarioTwo() {
        println(scenarioTwo("../input/day1.txt"))
    }
}