package zone.frog.advent.fifteen

data class Item(val cost: Int, val damage: Int, val armor: Int)

val weapons = listOf(
    Item(8, 4, 0),
    Item(10, 5, 0),
    Item(25, 6, 0),
    Item(40, 7, 0),
    Item(74, 8, 0),
)

val armors = listOf(
    Item(0, 0, 0),
    Item(13, 0, 1),
    Item(31, 0, 2),
    Item(53, 0, 3),
    Item(75, 0, 4),
    Item(102, 0, 5),
)

val rings = listOf(
    Item(0, 0, 0),
    Item(0, 0, 0),
    Item(25, 1, 0),
    Item(50, 2, 0),
    Item(100, 3, 0),
    Item(20, 0, 1),
    Item(40, 0, 2),
    Item(80, 0, 3),
)

data class Character(val hp: Int, val cost: Int, val damage: Int, val armor: Int) {
    fun beats(other: Character): Boolean {
        var heroHpRemaining = hp
        var bossHpRemaining = other.hp

        while (true) {
            bossHpRemaining -= damage - other.armor
            if (bossHpRemaining <= 0) return true
            heroHpRemaining -= other.damage - armor
            if (heroHpRemaining <= 0) return false
        }
    }
}

val boss = Character(104, -1, 8, 1)

fun buildCharacters() = sequence {
    for (weapon in weapons) {
        for (armor in armors) {
            for ((leftIndex, leftRing) in rings.withIndex()) {
                for ((rightIndex, rightRing) in rings.withIndex()) {
                    if (leftIndex == rightIndex) {
                        continue
                    }
                    yield(
                        Character(
                            100,
                            weapon.cost + armor.cost + leftRing.cost + rightRing.cost,
                            weapon.damage + armor.damage + leftRing.damage + rightRing.damage,
                            weapon.armor + armor.armor + leftRing.armor + rightRing.armor,
                        )
                    )
                }
            }
        }
    }
}