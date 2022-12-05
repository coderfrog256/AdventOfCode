package zone.frog.advent.twentytwo

import java.io.File
import java.util.regex.Pattern

object Day5 {
    private val operationRegex = Pattern.compile("move (\\d+) from (\\d+) to (\\d+)")!!

    private class Crates(private val stacks: List<ArrayDeque<Char>>) {
        fun move(from: Int, to: Int) {
            val toMove = stacks[from].removeLast()
            stacks[to].addLast(toMove)
        }

        fun moveMulti(items: Int, from: Int, to: Int) {
            val toMove = stacks[from].takeLast(items)
            repeat(items) { stacks[from].removeLast() }
            toMove.forEach { stacks[to].addLast(it) }
        }

        fun topLetters() = stacks
            .mapNotNull { it.lastOrNull() }
            .fold("") { s, c -> s + c }
    }

    private fun buildCrates(input: String) =
        input.split("\n")
            .filter { it.trim().startsWith("[") }
            .flatMap { row ->
                row.windowed(4, 4, partialWindows = true)
                    .mapIndexed { index, crate ->
                        if (crate.isBlank()) {
                            index to null
                        } else {
                            index to crate[1]
                        }
                    }
            }
            .filter { it.second != null }
            .fold(mutableListOf<ArrayDeque<Char>>()) { acc, pair ->
                while (acc.size <= pair.first) acc.add(ArrayDeque())
                acc[pair.first].addFirst(pair.second!!)
                acc
            }
            .let { Crates(it) }

    fun scenarioOne(textFileTop: String, textFileBottom: String): String {
        val crates = buildCrates(File(textFileTop).readText())
        File(textFileBottom).readLines()
            .map { operationRegex.matcher(it) }
            .filter { it.matches() }
            .forEach { command ->
                repeat(command.group(1).toInt()) {
                    crates.move(
                        from = command.group(2).toInt() - 1,
                        to = command.group(3).toInt() - 1
                    )
                }
            }

        return crates.topLetters()
    }

    fun scenarioTwo(textFileTop: String, textFileBottom: String): String {
        val crates = buildCrates(File(textFileTop).readText())
        File(textFileBottom).readLines()
            .map { operationRegex.matcher(it) }
            .filter { it.matches() }
            .forEach { command ->
                crates.moveMulti(
                    items = command.group(1).toInt(),
                    from = command.group(2).toInt() - 1,
                    to = command.group(3).toInt() - 1
                )
            }

        return crates.topLetters()
    }
}