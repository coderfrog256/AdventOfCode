package zone.frog.advent.twentytwo

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import zone.frog.advent.twentytwo.Day10.scenarioOne
import zone.frog.advent.twentytwo.Day10.scenarioTwo

class Day10Test {
    @Test
    internal fun testOne() {
        assertEquals(13140, scenarioOne("../input/day10-test.txt", listOf(20,60,100,140,180,220)))
    }

    @Test
    internal fun testScenarioOne() {
        assertEquals(12460, scenarioOne("../input/day10.txt", listOf(20,60,100,140,180,220)))
    }

    @Test
    internal fun testTwo() {
        assertEquals("""

##..##..##..##..##..##..##..##..##..##..
###...###...###...###...###...###...###.
####....####....####....####....####....
#####.....#####.....#####.....#####.....
######......######......######......####
#######.......#######.......#######.....
.
        """.trimIndent(), scenarioTwo("../input/day10-test.txt"))
    }

    @Test
    internal fun testScenarioTwo() {
        assertEquals("""

####.####.####.###..###...##..#..#.#....
#.......#.#....#..#.#..#.#..#.#.#..#....
###....#..###..#..#.#..#.#..#.##...#....
#.....#...#....###..###..####.#.#..#....
#....#....#....#....#.#..#..#.#.#..#....
####.####.#....#....#..#.#..#.#..#.####.
.
        """.trimIndent(), scenarioTwo("../input/day10.txt"))
    }
}