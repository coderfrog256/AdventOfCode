package zone.frog.advent.fifteen

import org.apache.commons.lang3.mutable.MutableInt
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test

class Day22Test {
    @Test
    fun exampleOne() {
        assertEquals(
            GameResult.WIN, runGame(
                player = MageCharacter("Player", hp = 10, mp = 250),
                boss = MageCharacter("Boss", hp = 13, damage = 8),
                toCast = listOf(
                    Poison(),
                    MagicMissile()
                ).iterator()
            )
        )
    }

    @Test
    fun exampleTwo() {
        assertEquals(
            GameResult.WIN, runGame(
                player = MageCharacter("Player", hp = 10, mp = 250),
                boss = MageCharacter("Boss", hp = 14, damage = 8),
                toCast = listOf(
                    Recharge(),
                    Shield(),
                    Drain(),
                    Poison(),
                    MagicMissile()
                ).iterator()
            )
        )
    }

    @Test
    fun drainCheck() {
        val player = MageCharacter("Player", hp = 50, mp = 500)
        val mageBoss = MageCharacter("Boss", hp = 58, damage = 9)

        val drain = Drain()
        while (!drain.apply(player, mageBoss));
        assertEquals(52, player.hp)
        assertEquals(56, mageBoss.hp)
    }

    @Test
    fun shieldCheck() {
        val player = MageCharacter("Player", hp = 50, mp = 500)
        val mageBoss = MageCharacter("Boss", hp = 58, damage = 9)

        val shield = Shield()

        assertEquals(0, player.armor)
        assertFalse(shield.apply(player, mageBoss))
        assertEquals(7, player.armor)
        assertFalse(shield.apply(player, mageBoss))
        assertEquals(7, player.armor)
        assertFalse(shield.apply(player, mageBoss))
        assertEquals(7, player.armor)
        assertFalse(shield.apply(player, mageBoss))
        assertEquals(7, player.armor)
        assertFalse(shield.apply(player, mageBoss))
        assertEquals(7, player.armor)
        assertFalse(shield.apply(player, mageBoss))
        assertEquals(7, player.armor)
        assertTrue(shield.apply(player, mageBoss))
        assertEquals(0, player.armor)
    }

    @Test
    fun poisonCheck() {
        val player = MageCharacter("Player", hp = 50, mp = 500)
        val mageBoss = MageCharacter("Boss", hp = 58, damage = 9)

        val poison = Poison()
        while (!poison.apply(player, mageBoss));
        assertEquals(58 - (6 * 3), mageBoss.hp)
    }

    @Test
    fun rechargeCheck() {
        val player = MageCharacter("Player", hp = 50, mp = 500)
        val mageBoss = MageCharacter("Boss", hp = 58, damage = 9)

        val recharge = Recharge()
        while (!recharge.apply(player, mageBoss));
        assertEquals((500 - 229) + (101 * 5), player.mp)
    }

    @Test
    fun scenarioOne() {
        logger = {}
        val player = MageCharacter("Player", hp = 50, mp = 500)
        val mageBoss = MageCharacter("Boss", hp = 58, damage = 9)
        val lowestMP = MutableInt(Int.MAX_VALUE)
        findWinningScenario(player, mageBoss, lowestMP, emptyList())
        assertNotEquals(Int.MAX_VALUE, lowestMP.value)

        assertEquals(1269, lowestMP.value)
    }

    @Test
    fun scenarioTwo() {
        logger = {}
        val player = MageCharacter("Player", hp = 50, mp = 500)
        val mageBoss = MageCharacter("Boss", hp = 58, damage = 9)
        val lowestMP = MutableInt(Int.MAX_VALUE)
        findWinningScenario(player, mageBoss, lowestMP, emptyList(), hardMode=true)
        assertNotEquals(Int.MAX_VALUE, lowestMP.value)

        assertEquals(1309, lowestMP.value)
    }
}