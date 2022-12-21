package zone.frog.advent.twentytwo

import java.io.File
import java.lang.IllegalArgumentException

object Day21 {
    //TODO: name as property. Drop pair as name is right here.
    interface Monkey {
        fun getName(): String
        fun getValue(monkeyMap: Map<String, Monkey>): Long
        fun caresAboutHumans(monkeyMap: Map<String, Monkey>): Boolean
        fun solveForValue(desiredValue: Long, monkeyMap: MutableMap<String, Monkey>): Long
    }

    data class MathMonkey(private val name: String, val lhs: String, val operator: String, val rhs: String) : Monkey {
        override fun getName(): String {
            return name
        }

        override fun getValue(monkeyMap: Map<String, Monkey>): Long {
            return when (operator) {
                "+" -> monkeyMap[lhs]!!.getValue(monkeyMap) + monkeyMap[rhs]!!.getValue(monkeyMap)
                "-" -> monkeyMap[lhs]!!.getValue(monkeyMap) - monkeyMap[rhs]!!.getValue(monkeyMap)
                "*" -> monkeyMap[lhs]!!.getValue(monkeyMap) * monkeyMap[rhs]!!.getValue(monkeyMap)
                "/" -> monkeyMap[lhs]!!.getValue(monkeyMap) / monkeyMap[rhs]!!.getValue(monkeyMap)
                else -> throw IllegalArgumentException(operator)
            }
        }

        override fun caresAboutHumans(monkeyMap: Map<String, Monkey>): Boolean {
            return monkeyMap[lhs]!!.caresAboutHumans(monkeyMap) || monkeyMap[rhs]!!.caresAboutHumans(monkeyMap)
        }

        //TODO: Unify this
        override fun solveForValue(desiredValue: Long, monkeyMap: MutableMap<String, Monkey>): Long {
            return if (monkeyMap[lhs]!!.caresAboutHumans(monkeyMap)) {
                val operand = monkeyMap[rhs]!!.getValue(monkeyMap)
                monkeyMap[lhs]!!.solveForValue(
                    when (operator) {
                        "+" -> desiredValue - operand
                        "-" -> desiredValue + operand
                        "*" -> desiredValue / operand
                        "/" -> desiredValue * operand
                        else -> throw IllegalArgumentException(operator)
                    }, monkeyMap
                )
            } else {
                val operand = monkeyMap[lhs]!!.getValue(monkeyMap)
                monkeyMap[rhs]!!.solveForValue(
                    when (operator) {
                        "+" -> desiredValue - operand
                        "-" -> operand - desiredValue
                        "*" -> desiredValue / operand
                        "/" -> operand / desiredValue
                        else -> throw IllegalArgumentException(operator)
                    }, monkeyMap
                )
            }
        }
    }

    data class ValueMonkey(private val name: String, val value: Long) : Monkey {
        override fun getName(): String {
            return name
        }

        override fun getValue(monkeyMap: Map<String, Monkey>): Long {
            return value
        }

        override fun caresAboutHumans(monkeyMap: Map<String, Monkey>): Boolean {
            return name == "humn"
        }

        override fun solveForValue(desiredValue: Long, monkeyMap: MutableMap<String, Monkey>): Long {
            if(name == "humn") return desiredValue
            return value
        }
    }

    fun getRootValue(lines: List<String>): Long {
        val monkeyMap = mutableMapOf<String, Monkey>()
        val monkeys = lines
            .map { it.split(": ") }
            .map { it[0] to it[1] }

        monkeys
            .map { it.first to it.second.toLongOrNull() }
            .filter { it.second != null }
            .map { it.first to ValueMonkey(it.first, it.second!!) }
            .forEach { monkeyMap += it }

        monkeys
            .filter { it.second.contains(" ") }
            .map {
                val math = it.second.split(" ")
                it.first to MathMonkey(it.first, math[0], math[1], math[2])
            }
            .forEach { monkeyMap += it }

        return monkeyMap["root"]!!.getValue(monkeyMap)
    }

    fun getRootValuePartTwo(lines: List<String>): Long {
        val monkeyMap = mutableMapOf<String, Monkey>()
        val monkeys = lines
            .map { it.split(": ") }
            .map { it[0] to it[1] }

        monkeys
            .map { it.first to it.second.toLongOrNull() }
            .filter { it.second != null }
            .map { it.first to ValueMonkey(it.first, it.second!!) }
            .forEach { monkeyMap += it }

        monkeys
            .filter { it.second.contains(" ") }
            .map {
                val math = it.second.split(" ")
                it.first to MathMonkey(it.first, math[0], math[1], math[2])
            }
            .forEach { monkeyMap += it }

        val rootLhs = (monkeyMap["root"] as MathMonkey).lhs
        val rootRhs = (monkeyMap["root"] as MathMonkey).rhs
        val desiredValue =
            if (monkeyMap[rootLhs]!!.caresAboutHumans(monkeyMap)) monkeyMap[rootRhs]!!.getValue(monkeyMap)
            else monkeyMap[rootLhs]!!.getValue(monkeyMap)

        val humanValue =
            if (monkeyMap[rootLhs]!!.caresAboutHumans(monkeyMap)) monkeyMap[rootLhs]!!.solveForValue(
                desiredValue,
                monkeyMap
            )
            else monkeyMap[rootRhs]!!.solveForValue(desiredValue, monkeyMap)

        return humanValue
    }

    fun scenarioOne(textFile: String) =
        File(textFile).readLines()
            .let { getRootValue(it) }

    fun scenarioTwo(textFile: String) =
        File(textFile).readLines()
            .let { getRootValuePartTwo(it) }
}