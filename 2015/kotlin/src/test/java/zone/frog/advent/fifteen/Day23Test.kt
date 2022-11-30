package zone.frog.advent.fifteen

import org.apache.commons.lang3.mutable.MutableInt
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import java.io.File

class Day23Test {
    @Test
    fun example() {
        println(
            runProgram(File("../input/day23-test.txt").readText())
        )
    }

    @Test
    fun scenario1() {
        assertEquals(
            170u,
            runProgram(File("../input/day23.txt").readText())
                .registerB
        )
    }

    @Test
    fun scenario2() {
        assertEquals(
            247u,
            runProgram(
                File("../input/day23.txt").readText(),
                ComputerState(registerA = 1u)
            ).registerB
        )
    }
}