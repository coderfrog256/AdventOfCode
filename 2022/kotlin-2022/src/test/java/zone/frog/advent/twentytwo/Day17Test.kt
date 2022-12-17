package zone.frog.advent.twentytwo

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import zone.frog.advent.twentytwo.Day17.scenarioOne
import zone.frog.advent.twentytwo.Day17.scenarioTwo
import java.io.File

class Day17Test {

    @Test
    internal fun testOne() {
        println(scenarioOne("../input/day17-test.txt"))
    }

    // 3059 too low
    @Test
    internal fun testScenarioOne() {
        println(scenarioOne("../input/day17.txt"))
    }

    @Test
    internal fun testTwo() {
        println(scenarioTwo("../input/day17-test.txt"))
    }

    @Test
    internal fun testScenarioTwo() {
        println( scenarioTwo("../input/day17.txt"))
    }
}