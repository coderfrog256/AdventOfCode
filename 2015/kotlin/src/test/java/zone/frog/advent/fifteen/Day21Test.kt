package zone.frog.advent.fifteen

import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test

class Day21Test {
    @Test
    internal fun testBeats() {
        val hero = Character(8, -1, 5, 5)
        val boss = Character(12, -1, 7, 2)
        assertTrue(hero.beats(boss))
    }

    @Test
    internal fun questionOne() {
        val cheapestWinner = buildCharacters()
            .filter { it.beats(boss) }
            .sortedBy { it.cost }
            .first()
        println(cheapestWinner.cost)
    }

    @Test
    internal fun questionTwo() {
        val expensiveLoser = buildCharacters()
            .filter { !it.beats(boss) }
            .sortedByDescending { it.cost }
            .first()
        println(expensiveLoser.cost)
    }
}