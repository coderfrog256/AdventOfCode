package zone.frog.advent.twentytwo

import java.io.File
import java.lang.IllegalArgumentException


object Day21 {
    private const val HUMAN_NAME = "humn"
    private const val ROOT_NAME = "root"

    interface Monkey {
        val name: String
        fun getValue(monkeyMap: Map<String, Monkey>): Long
        fun caresAboutHumans(monkeyMap: Map<String, Monkey>): Boolean
        fun solveForValue(desiredValue: Long, monkeyMap: Map<String, Monkey>): Long
    }

    data class ValueMonkey(override val name: String, val value: Long) : Monkey {
        override fun getValue(monkeyMap: Map<String, Monkey>) = value
        override fun caresAboutHumans(monkeyMap: Map<String, Monkey>) = name == HUMAN_NAME
        override fun solveForValue(desiredValue: Long, monkeyMap: Map<String, Monkey>) = desiredValue
    }

    data class CalculatorMonkey(
        override val name: String,
        val lhs: String,
        val operator: String,
        val rhs: String
    ) : Monkey {
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

        override fun solveForValue(desiredValue: Long, monkeyMap: Map<String, Monkey>): Long {
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

    private fun parseMonkeys(lines: List<String>) =
        lines.map { line ->
            val (name, job) = line.split(": ")
            job.toLongOrNull()
                ?.let { ValueMonkey(name, it) }
                ?: job.split(" ").let { CalculatorMonkey(name, it[0], it[1], it[2]) }
        }.associateBy { it.name }

    private fun getRootValue(lines: List<String>): Long {
        val monkeyMap = parseMonkeys(lines)
        return monkeyMap[ROOT_NAME]!!.getValue(monkeyMap)
    }

    private fun getHumanValue(lines: List<String>): Long {
        val monkeyMap = parseMonkeys(lines)

        val rootLhs = monkeyMap[(monkeyMap[ROOT_NAME] as CalculatorMonkey).lhs]!!
        val rootRhs = monkeyMap[(monkeyMap[ROOT_NAME] as CalculatorMonkey).rhs]!!

        val dynamicOperand = if (rootLhs.caresAboutHumans(monkeyMap)) rootLhs else rootRhs
        val staticOperand = if (dynamicOperand == rootLhs) rootRhs else rootLhs

        val desiredValue = staticOperand.getValue(monkeyMap)
        return dynamicOperand.solveForValue(desiredValue, monkeyMap)
    }

    fun scenarioOne(textFile: String) =
        File(textFile).readLines()
            .let { getRootValue(it) }

    fun scenarioTwo(textFile: String) =
        File(textFile).readLines()
            .let { getHumanValue(it) }
}