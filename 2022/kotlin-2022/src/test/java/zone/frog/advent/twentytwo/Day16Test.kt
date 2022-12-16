package zone.frog.advent.twentytwo

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import zone.frog.advent.twentytwo.Day16.parseValves
import zone.frog.advent.twentytwo.Day16.scenarioOne
import zone.frog.advent.twentytwo.Day16.scenarioTwo
import java.io.File

class Day16Test {

    @Test
    internal fun sanityTest() {
        //Are our individualTurnValues correct for the example scenario?
        val valves = parseValves(File("../input/day16-test.txt").readLines())
        assertEquals(
            1651,
            valves["DD"]!!.individualTurnValue[2]!!
                    + valves["BB"]!!.individualTurnValue[5]!!
                    + valves["JJ"]!!.individualTurnValue[9]!!
                    + valves["HH"]!!.individualTurnValue[17]!!
                    + valves["EE"]!!.individualTurnValue[21]!!
                    + valves["CC"]!!.individualTurnValue[24]!!
        )
    }

    @Test
    internal fun sanityTest2() {
        //Are our individualTurnValues correct for the example scenario?
        val valves = parseValves(File("../input/day16-test.txt").readLines())
        assertEquals(
            1707,
            valves["DD"]!!.individualTurnValuePart2[2]!!
                    + valves["JJ"]!!.individualTurnValuePart2[3]!!
                    + valves["HH"]!!.individualTurnValuePart2[7]!!
                    + valves["BB"]!!.individualTurnValuePart2[7]!!
                    + valves["CC"]!!.individualTurnValuePart2[9]!!
                    + valves["EE"]!!.individualTurnValuePart2[11]!!
        )
    }

    @Test
    internal fun testOne() {
        assertEquals(1651, scenarioOne("../input/day16-test.txt"))
    }

    @Test
    internal fun testScenarioOne() {
        assertEquals(1775, scenarioOne("../input/day16.txt"))
    }

    @Test
    internal fun testTwo() {
        assertEquals(1707, scenarioTwo("../input/day16-test.txt"))
    }

    @Test
    internal fun testScenarioTwo() {
        assertEquals(2351, scenarioTwo("../input/day16.txt"))
    }
}