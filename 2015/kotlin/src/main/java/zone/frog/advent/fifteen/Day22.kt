package zone.frog.advent.fifteen

import org.apache.commons.lang3.mutable.MutableInt
import kotlin.math.max

// Lazy disable-able logger. Assign this to an empty lambda to bypass string formatting. We don't need no log4j!
var logger: (() -> String) -> Unit = { println(it()) }

data class MageCharacter(val name: String, var hp: Int, var mp: Int = -1, var armor: Int = 0, val damage: Int = -1) {
    var effects: List<Spell> = mutableListOf()

    override fun toString() = "$name has $hp HP, $mp MP, $armor armor"

    fun applyDamage(enemy: MageCharacter) {
        val delta = max(1, enemy.damage - armor)
        logger { "${enemy.name} attacks for $delta damage." }
        this.hp -= delta
    }

    fun hasDuplicateEffects() = effects.groupBy { it.javaClass }.any { it.value.size > 1 }
}

abstract class Spell(val mpCost: Int) {
    abstract fun apply(player: MageCharacter, enemy: MageCharacter): Boolean
    open fun reset() = this
    override fun toString() = javaClass.simpleName!!
}

class MagicMissile : Spell(53) {
    override fun apply(player: MageCharacter, enemy: MageCharacter): Boolean {
        player.mp -= 53
        enemy.hp -= 4
        logger { "Magic Missile deals 4 damage." }
        return true
    }
}

class Drain : Spell(73) {
    override fun apply(player: MageCharacter, enemy: MageCharacter): Boolean {
        player.mp -= 73
        player.hp += 2
        enemy.hp -= 2
        logger { "Drain transfers 2 damage." }
        return true
    }
}

class Shield : Spell(113) {
    private var ticks = 7

    override fun apply(player: MageCharacter, enemy: MageCharacter): Boolean {
        if (ticks == 7) {
            player.mp -= 113
            player.armor += 7
        } else if (ticks == 1) {
            player.armor -= 7
        }
        return (--ticks == 0).also { logger { "Shield's timer is now $ticks" } }
    }

    override fun reset(): Spell {
        this.ticks = 7
        return this
    }
}

class Poison : Spell(173) {
    private var ticks = 7

    override fun apply(player: MageCharacter, enemy: MageCharacter): Boolean {
        if (ticks == 7) {
            player.mp -= 173
        } else if (ticks > 0) {
            enemy.hp -= 3
            logger { "Poison deals 3 damage." }
        }
        return (--ticks == 0).also { logger { "Poison's timer is now $ticks" } }
    }

    override fun reset(): Spell {
        this.ticks = 7
        return this
    }
}

class Recharge : Spell(229) {
    private var ticks = 6

    override fun apply(player: MageCharacter, enemy: MageCharacter): Boolean {
        if (ticks == 6) {
            player.mp -= 229
            ticks--
            return false
        } else if (ticks-- > 0) {
            player.mp += 101
            logger { "Recharge provides 101 mp." }
        }
        logger { "Recharge's timer is now $ticks" }
        return ticks == 0
    }

    override fun reset(): Spell {
        this.ticks = 6
        return this
    }
}

enum class GameResult {
    NO_SPELLS,
    LOSS,
    WIN
}

fun runGame(
    player: MageCharacter,
    boss: MageCharacter,
    toCast: Iterator<Spell>,
    hardMode: Boolean = false
): GameResult {
    val gamePlayer = player.copy()
    val gameBoss = boss.copy()
    while (true) {
        logger { "\n-- Player Turn --" }
        logger { "- $gamePlayer" }
        logger { "- $gameBoss" }
        if (hardMode) {
            gamePlayer.hp -= 1
            if (gamePlayer.hp <= 0) {
                logger { "You died of old age." }
                return GameResult.LOSS
            }
        }
        val spell = toCast.nextOrNull()
        if (spell == null) {
            logger { "Ran out of spells, try adding more spells." }
            return GameResult.NO_SPELLS
        }
        gameBoss.effects += spell.reset()
        gameBoss.effects = gameBoss.effects.filter { !it.apply(gamePlayer, gameBoss) }
        logger { "Player casts $spell" }
        if (gameBoss.hasDuplicateEffects()) {
            logger { "You cast the same spell while it was already active. You broke the rules!" }
            return GameResult.LOSS
        }
        if (gamePlayer.mp <= 0) {
            logger { "Ran out of MP, you lost!" }
            return GameResult.LOSS
        }
        if (gameBoss.hp <= 0) {
            logger { "This kills the boss, and the player wins" }
            return GameResult.WIN
        }

        logger { "\n-- Boss Turn --" }
        logger { "- $gamePlayer" }
        logger { "- $gameBoss" }
        gameBoss.effects = gameBoss.effects.filter { !it.apply(gamePlayer, gameBoss) }
        if (gameBoss.hp <= 0) {
            logger { "This kills the boss, and the player wins." }
            return GameResult.WIN
        }

        gamePlayer.applyDamage(gameBoss)
        if (gamePlayer.hp <= 0) {
            logger { "This kills the player, and the boss wins. bossHP=${gameBoss.hp}" }
            return GameResult.LOSS
        }
    }
}

fun buildSpellList() = listOf(
    Poison(),
    Shield(),
    Recharge(),
    MagicMissile(),
    Drain(),
)

fun findWinningScenario(
    player: MageCharacter,
    boss: MageCharacter,
    lowestMP: MutableInt,
    castStack: List<Spell>,
    hardMode: Boolean = false
) {
    val mpSpent = castStack.sumOf { it.mpCost }
    for (spell in buildSpellList()) {
        val newCost = mpSpent + spell.mpCost
        if (newCost >= lowestMP.value) {
            continue
        }
        val newStack = castStack + spell
        when (runGame(player, boss, newStack.iterator(), hardMode)) {
            GameResult.NO_SPELLS -> findWinningScenario(player, boss, lowestMP, newStack, hardMode)
            GameResult.LOSS -> continue
            GameResult.WIN -> lowestMP.setValue(newCost)
        }
    }
}

fun <T> Iterator<T>.nextOrNull(): T? = if (this.hasNext()) this.next() else null