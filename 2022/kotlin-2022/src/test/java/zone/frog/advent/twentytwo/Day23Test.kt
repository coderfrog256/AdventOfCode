package zone.frog.advent.twentytwo

import org.junit.jupiter.api.Test
import zone.frog.advent.twentytwo.Day23.scenarioOne
import zone.frog.advent.twentytwo.Day23.scenarioTwo

class Day23Test {
    @Test
    internal fun testOneMini() {
        println(scenarioOne("../input/day23-test-mini.txt"))
    }

    //4192 is too high!
    //3781 is too low?
    @Test
    internal fun testOne() {
        println(scenarioOne("../input/day23-test.txt"))
    }

    @Test
    internal fun testScenarioOne() {
        println(scenarioOne("../input/day23.txt"))
    }

    @Test
    internal fun testTwo() {
        println(scenarioTwo("../input/day23-test.txt"))
    }

    @Test
    internal fun testScenarioTwo() {
        println(scenarioTwo("../input/day23.txt"))
    }
}