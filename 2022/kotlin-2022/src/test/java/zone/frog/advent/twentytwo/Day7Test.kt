package zone.frog.advent.twentytwo

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import zone.frog.advent.twentytwo.Day7.scenarioOne
import zone.frog.advent.twentytwo.Day7.scenarioTwo

class Day7Test {
    @Test
    internal fun testOne() {
        assertEquals(95437, scenarioOne("../input/day7-test.txt"))
    }

    @Test
    internal fun testScenarioOne() {
        assertEquals(1644735, scenarioOne("../input/day7.txt"))
    }

    @Test
    internal fun testTwo() {
        assertEquals(24933642, scenarioTwo("../input/day7-test.txt"))
    }

    @Test
    internal fun testScenarioTwo() {
        assertEquals(1300850, scenarioTwo("../input/day7.txt"))
    }
}