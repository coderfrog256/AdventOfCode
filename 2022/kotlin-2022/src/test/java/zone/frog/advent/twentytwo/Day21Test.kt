package zone.frog.advent.twentytwo

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import zone.frog.advent.twentytwo.Day21.scenarioOne
import zone.frog.advent.twentytwo.Day21.scenarioTwo

class Day21Test {
    @Test
    internal fun testOne() {
        assertEquals(152, scenarioOne("../input/day21-test.txt"))
    }

    @Test
    internal fun testScenarioOne() {
        assertEquals(158731561459602, scenarioOne("../input/day21.txt"))
    }

    @Test
    internal fun testTwo() {
        assertEquals(301, scenarioTwo("../input/day21-test.txt"))
    }

    @Test
    internal fun testScenarioTwo() {
        assertEquals(3769668716709, scenarioTwo("../input/day21.txt"))
    }
}