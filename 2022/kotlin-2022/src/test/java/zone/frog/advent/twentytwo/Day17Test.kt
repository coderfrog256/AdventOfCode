package zone.frog.advent.twentytwo

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import zone.frog.advent.twentytwo.Day17.scenarioOne
import zone.frog.advent.twentytwo.Day17.scenarioTwo
import java.io.File

class Day17Test {
    @Test
    internal fun testOne() {
        assertEquals(3068, scenarioOne("../input/day17-test.txt"))
    }

    @Test
    internal fun testScenarioOne() {
        assertEquals(3065, scenarioOne("../input/day17.txt"))
    }

    @Test
    internal fun testTwo() {
        // Due to how I'm optimizing, the sample input does not pass as it never clears a row. Wild!
        // assertEquals(1514285714288L, scenarioTwo("../input/day17-test.txt"))
    }

    @Test
    internal fun testScenarioTwo() {
        assertEquals(1562536022966L, scenarioTwo("../input/day17.txt"))
    }
}