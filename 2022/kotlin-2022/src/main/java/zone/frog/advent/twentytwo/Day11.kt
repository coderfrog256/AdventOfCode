package zone.frog.advent.twentytwo

import java.io.File

object Day11 {
    data class Monkey(
        val worryDivisor: Long?,
        val items: MutableList<Long>,
        val operation: (Long) -> Long,
        val testDivisor: Long,
        val trueMonkeyId: Int,
        val falseMonkeyId: Int,
        var inspectCount: Long = 0
    ) {
        fun act(monkeys: Map<Int, Monkey>, leastCommonMultiple: Long) {
            inspectCount += items.size
            for (item in items) {
                val inspectedValue = when (worryDivisor) {
                    null -> operation(item) % leastCommonMultiple
                    else -> operation(item) / worryDivisor
                }
                val targetId = when {
                    inspectedValue % testDivisor == 0L -> trueMonkeyId
                    else -> falseMonkeyId
                }
                monkeys[targetId]!!.items += inspectedValue
            }
            items.clear()
        }
    }

    private fun parseMonkey(monkeyLines: List<String>, worryDivisor: Long?): Pair<Int, Monkey> {
        val (monkeyId) = Regex("Monkey (\\d+):").matchEntire(monkeyLines[0])?.destructured
            ?: throw IllegalArgumentException("Bad Monkey Id: ${monkeyLines[0]}")
        val (items) = Regex("\\s+Starting items: (.*)").matchEntire(monkeyLines[1])?.destructured
            ?: throw IllegalArgumentException("Bad Monkey Items: ${monkeyLines[1]}")
        val (operator, value) = Regex("\\s+Operation: new = old (\\S+) (\\S+)").matchEntire(monkeyLines[2])?.destructured
            ?: throw IllegalArgumentException("Bad Monkey Operation: ${monkeyLines[2]}")
        val (divisibleBy) = Regex("\\s+Test: divisible by (\\d+)").matchEntire(monkeyLines[3])?.destructured
            ?: throw IllegalArgumentException("Bad Monkey Test: ${monkeyLines[3]}")
        val (trueMonkeyId) = Regex("\\s+If true: throw to monkey (\\d+)").matchEntire(monkeyLines[4])?.destructured
            ?: throw IllegalArgumentException("Bad True Monkey Id: ${monkeyLines[4]}")
        val (falseMonkeyId) = Regex("\\s+If false: throw to monkey (\\d+)").matchEntire(monkeyLines[5])?.destructured
            ?: throw IllegalArgumentException("Bad False Monkey Id: ${monkeyLines[5]}")

        val operation = if (operator == "*") { oldValue: Long ->
            val resolvedValue = if (value == "old") oldValue else value.toLong()
            oldValue * resolvedValue
        } else { oldValue: Long ->
            val resolvedValue = if (value == "old") oldValue else value.toLong()
            oldValue + resolvedValue
        }

        return monkeyId.toInt() to Monkey(
            worryDivisor,
            items.split(",").map { it.trim().toLong() }.toMutableList(),
            operation,
            divisibleBy.toLong(),
            trueMonkeyId.toInt(),
            falseMonkeyId.toInt()
        )
    }

    private fun buildMonkeyMap(worryDivisor: Long?, lines: List<String>): Map<Int, Monkey> {
        return lines
            .filter { it.isNotBlank() }
            .chunked(6) //Chunk into monkey.
            .associate { parseMonkey(it, worryDivisor) }
    }

    private fun playMonkeyBall(monkeys: Map<Int, Monkey>, turns: Int): List<Long> {
        // Even with BigDecimal, the worries get too big!
        // If we divide them by the LCM, it stays reasonable, while keeping the monkey-routing the same.
        // This is safe as all the monkeys have primes as their divisors.
        val leastCommonMultiple = monkeys.values
            .map { it.testDivisor }
            .fold(1L) { acc, i -> acc * i }

        repeat(turns) {
            monkeys.values.forEach {
                it.act(monkeys, leastCommonMultiple)
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