package zone.frog.advent.fifteen

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import java.io.File

class Day2Test {
    @Test
    fun testSquareFeet() {
        println(getSquareFeet(2, 3, 4))
        println(getSquareFeet(1, 1, 10))
    }

    @Test
    internal fun scenario1() {
        assertEquals(
            1588178,
            File("../input/day2.txt").readLines()
                .map { it.split("x").map { it.toInt() } }
                .sumOf { getSquareFeet(it[0], it[1], it[2]) }
        )
    }

    @Test
    internal fun example2() {
        println(getRibbonLength(2,3,4))
        println(getRibbonLength(1,1,10))
    }

    @Test
    internal fun scenario2() {
        assertEquals(
            3783758,
            File("../input/day2.txt").readLines()
                .map { it.split("x").map { it.toInt() } }
                .sumOf { getRibbonLength(it[0], it[1], it[2]) }
        )
    }
}