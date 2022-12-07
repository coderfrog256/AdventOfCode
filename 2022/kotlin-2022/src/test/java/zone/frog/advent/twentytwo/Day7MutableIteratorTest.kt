package zone.frog.advent.twentytwo

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import zone.frog.advent.twentytwo.Day7MutableIterator.scenarioOne
import zone.frog.advent.twentytwo.Day7MutableIterator.scenarioTwo

class Day7MutableIteratorTest {
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