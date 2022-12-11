package zone.frog.advent.twentytwo

import java.io.File
import java.math.BigDecimal
import java.math.RoundingMode

object Day11 {
    data class Monkey(
        val worryDivisor: BigDecimal?,
        val items: MutableList<BigDecimal>,
        val operation: (BigDecimal) -> BigDecimal,
        val testDivisor: BigDecimal,
        val trueMonkeyId: Int,
        val falseMonkeyId: Int,
        var inspectCount: Long = 0
    ) {
        fun act(monkeys: Map<Int, Monkey>, commonDenominator: BigDecimal) {
            inspectCount += items.size
            for (item in items) {
                val inspectedValue = when (worryDivisor) {
                    null -> operation(item).remainder(commonDenominator)
                    else -> operation(item).divide(worryDivisor, RoundingMode.DOWN)
                }
                val targetId = when {
                    inspectedValue.remainder(testDivisor).compareTo(BigDecimal.ZERO) == 0 -> trueMonkeyId
                    else -> falseMonkeyId
                }
                monkeys[targetId]!!.items += inspectedValue
            }
            items.clear()
        }
    }

    private fun parseMonkey(worryDivisor: Long?, monkeyLines: List<String>): Pair<Int, Monkey> {
        val (monkeyId) = Regex("Monkey (\\d+):").matchEntire(monkeyLines[0].trim())?.destructured
            ?: throw IllegalArgumentException("Bad Monkey Id")
        val (items) = Regex("Starting items: (.*)").matchEntire(monkeyLines[1].trim())?.destructured
            ?: throw IllegalArgumentException("Bad Monkey Items")
        val (operator, value) = Regex("Operation: new = old (\\S+) (\\S+)").matchEntire(monkeyLines[2].trim())?.destructured
            ?: throw IllegalArgumentException("Bad Monkey Operation")
        val (divisibleBy) = Regex("Test: divisible by (\\d+)").matchEntire(monkeyLines[3].trim())?.destructured
            ?: throw IllegalArgumentException("Bad Monkey Test")
        val (trueMonkeyId) = Regex("If true: throw to monkey (\\d+)").matchEntire(monkeyLines[4].trim())?.destructured
            ?: throw IllegalArgumentException("Bad True Monkey")
        val (falseMonkeyId) = Regex("If false: throw to monkey (\\d+)").matchEntire(monkeyLines[5].trim())?.destructured
            ?: throw IllegalArgumentException("Bad false Monkey")

        val operation = if (operator == "*") { oldValue: BigDecimal ->
            val resolvedValue = if (value == "old") oldValue else value.toLong().toBigDecimal()
            oldValue * resolvedValue
        } else { oldValue: BigDecimal ->
            val resolvedValue = if (value == "old") oldValue else value.toLong().toBigDecimal()
            oldValue + resolvedValue
        }

        val bdDivisor = divisibleBy.toLong().toBigDecimal()
        return monkeyId.toInt() to Monkey(
            worryDivisor?.toBigDecimal(),
            items.split(",").map { it.trim().toBigDecimal() }.toMutableList(),
            operation,
            bdDivisor,
            trueMonkeyId.toInt(),
            falseMonkeyId.toInt()
        )
    }

    private fun buildMonkeyMap(worryDivisor: Long?, lines: List<String>): Map<Int, Monkey> {
        return lines
            .filter { it.isNotBlank() }
            .chunked(6)
            .associate { parseMonkey(worryDivisor, it) }
            .toSortedMap()
    }

    private fun playMonkeyBall(monkeys: Map<Int, Monkey>, turns: Int): List<Long> {
        val commonDenominator = monkeys.values
            .map { it.testDivisor }
            .fold(1L.toBigDecimal()) { acc, i -> acc * i }

        repeat(turns) {
            monkeys.values.forEach {
                it.act(monkeys, commonDenominator)
            }
        }

        return monkeys.values.map { it.inspectCount }
    }

    fun scenarioOne(textFile: String) =
        File(textFile)
            .readLines()
            .let { buildMonkeyMap(3, it) }
            .let { playMonkeyBall(it, 20) }
            .sortedDescending()
            .take(2)
            .fold(1L) { acc, i -> acc * i }

    fun scenarioTwo(textFile: String) =
        File(textFile)
            .readLines()
            .let { buildMonkeyMap(null, it) }
            .let { playMonkeyBall(it, 10000) }
            .sortedDescending()
            .take(2)
            .fold(1L) { acc, i -> acc * i }
}