package zone.frog.advent.twentytwo

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import zone.frog.advent.twentytwo.Day5.scenarioOne
import zone.frog.advent.twentytwo.Day5.scenarioTwo

class Day5Test {
    @Test
    internal fun testOne() {
        assertEquals("CMZ", scenarioOne("../input/day5-test.txt"))
    }

    @Test
    internal fun testScenarioOne() {
        assertEquals("ZRLJGSCTR", scenarioOne("../input/day5.txt"))
    }

    @Test
    internal fun testTwo() {
        assertEquals("MCD", scenarioTwo("../input/day5-test.txt"))
    }

    @Test
    internal fun testScenarioTwo() {
        assertEquals("PRTTGRFPB", scenarioTwo("../input/day5.txt"))
    }
}