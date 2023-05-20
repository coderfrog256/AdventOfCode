package zone.frog.advent.sixteen

import java.io.File

object Day1 {
    data class State(val position: IntPair, val direction: Direction)
    enum class Direction(private val offset: IntPair) {
        North(0 to -1), East(-1 to 0), South(0 to 1), West(1 to 0);

        fun steps(command: String, state: State) = sequence {
            val newDirection = values().circularGet(state.direction.ordinal + 1 * if (command[0] == 'L') -1 else 1)
            (1..command.substring(1).toInt()).fold(state) { state, _ ->
                State(state.position + newDirection.offset, newDirection)
                    .also { yield(it) }
            }
        }
    }

    fun scenarioOne(textFile: String) =
        File(textFile).readText().trim()
            .split(", ")
            .fold(State(intOrigin, Direction.North)) { state, command ->
                state.direction.steps(command, state).last()
            }
            .let { intOrigin.distance(it.position) }

    fun scenarioTwo(textFile: String): Int =
        File(textFile).readText().trim()
            .split(", ")
            .fold(State(intOrigin, Direction.North) to mutableSetOf<IntPair>()) { (state, history), command ->
                var newState = state
                state.direction.steps(command, state).forEach { state ->
                    if (state.position in history) return@scenarioTwo intOrigin.distance(state.position)
                    history.add(state.position)
                    newState = state
                }
                newState to history
            }
            .let { throw IllegalStateException("No location visited twice") }
}