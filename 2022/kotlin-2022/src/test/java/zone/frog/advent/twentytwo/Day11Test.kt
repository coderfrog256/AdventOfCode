package zone.frog.advent.twentytwo

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import zone.frog.advent.twentytwo.Day11.scenarioOne
import zone.frog.advent.twentytwo.Day11.scenarioTwo

class Day11Test {
    @Test
    internal fun testOne() {
        assertEquals(10605, scenarioOne("../input/day11-test.txt"))
    }

    @Test
    internal fun testScenarioOne() {
        assertEquals(112815, scenarioOne("../input/day11.txt"))
    }

    @Test
    internal fun testTwo() {
        assertEquals(2713310158, scenarioTwo("../input/day11-test.txt"))
    }

    @Test
    internal fun testScenarioTwo() {
        assertEquals(25738411485, scenarioTwo("../input/day11.txt"))
    }
}