package zone.frog.advent.twentytwo

import java.io.File
import java.lang.IllegalArgumentException
import java.util.concurrent.ConcurrentHashMap


object Day16 {
    data class MemoKey(val timer: Int, val runningSum: Int, val currentValve: Valve)
    data class MemoKeyPart2(val timer: Int, val runningSum: Int, val humanValve: Valve, val elephantValve: Valve)
    data class Move(val toValve: Valve, val open: Boolean)

    const val TIME_LIMIT = 30
    const val TIME_LIMIT_PART_2 = 26

    data class Valve(val name: String, val flowRate: Int, val tunnelNames: List<String>) {
        val individualTurnValue = (0..TIME_LIMIT).associateBy({ it }, { getTotalFlowed(it + 1) })
        val individualTurnValuePart2 = (0..TIME_LIMIT_PART_2).associateBy({ it }, { getTotalFlowedPart2(it + 1) })

        private fun getTotalFlowed(time: Int) = ((TIME_LIMIT + 1) - time) * flowRate
        private fun getTotalFlowedPart2(time: Int) = ((TIME_LIMIT_PART_2 + 1) - time) * flowRate

        fun isUseful(path: Set<Valve>, enabled: Set<Valve>, valves: Map<String, Valve>): Boolean {
            val unvisited = tunnelNames.map { valves[it]!! }.filter { !path.contains(it) }
            return (flowRate > 0 && !enabled.contains(this))
                    || unvisited.any { it.flowRate > 0 && !enabled.contains(it) }
                    || unvisited.any { it.isUseful(path + it, enabled, valves) }
        }

        companion object {
            fun parse(line: String): Valve {
                val (name, flowRate, paths) = Regex("Valve (\\w+) has flow rate=(\\d+); tunnels? leads? to valves? (.*)")
                    .matchEntire(line)?.destructured ?: throw IllegalArgumentException(line)
                return Valve(name, flowRate.toInt(), paths.split(", "))
            }
        }
    }

    fun parseValves(lines: List<String>): Map<String, Valve> {
        return lines.map { Valve.parse(it) }.associateBy { it.name }
    }


    private fun turnAllOn(valves: Map<String, Valve>): Int {
        val memoizedState = HashMap<MemoKey, Int>()
        fun visitValve(timer: Int, valve: Valve, runningSum: Int, from: Valve?, enabled: Set<Valve>): Int {
            val memoized = memoizedState[MemoKey(timer, runningSum, valve)]
            if (memoized != null) {
                return memoized
            }

            val toEnable = valves.filter { it.value.flowRate > 0 }.count()

            if (timer >= TIME_LIMIT) {
                //We've the time limit, see how much has flowed.
                memoizedState[MemoKey(timer, runningSum, valve)] = runningSum
                return runningSum
            }

            // All enabled, nothing to do but wait.
            if (toEnable == enabled.size) {
                memoizedState[MemoKey(timer, runningSum, valve)] = runningSum
                return runningSum
            }

            var max = 0
            // If we've still got time left, and it's worth turning on our valve, do so.
            if (timer + 1 <= TIME_LIMIT && !enabled.contains(valve) && valve.flowRate > 0) {
                val newRunningSum = runningSum + valve.individualTurnValue[timer + 1]!!
                if (toEnable == enabled.size + 1) {
                    memoizedState[MemoKey(timer, runningSum, valve)] = runningSum
                    return newRunningSum
                }

                val childMaxes = valve.tunnelNames.maxOfOrNull {
                    visitValve(timer + 2, valves[it]!!, newRunningSum, null, enabled + valve)
                } ?: 0
                max = if (childMaxes > max) childMaxes else max
            }

            // Visit each option, without turning on our valve
            val childMaxes = valve.tunnelNames
                .mapNotNull { valves[it]?.takeIf { it != from && it.isUseful(setOf(valve), enabled, valves) } }
                .maxOfOrNull {
                    visitValve(timer + 1, it, runningSum, valve, enabled) ?: 0
                } ?: 0
            max = if (childMaxes > max) childMaxes else max

            memoizedState[MemoKey(timer, runningSum, valve)] = max
            return max
        }
        return visitValve(0, valves["AA"]!!, 0, null, emptySet())
    }


    private fun turnAllOnWithAnElephant(valves: Map<String, Valve>): Int {
        val memoizedStates = ConcurrentHashMap<MemoKeyPart2, Int>()
        fun visitValves(
            timer: Int,
            humanValve: Valve,
            elephantValve: Valve,
            runningSum: Int,
            fromHuman: Valve?,
            fromElephant: Valve?,
            humanOpening: Boolean,
            elephantOpening: Boolean,
            enabled: Set<Valve>
        ): Int {
            val memoized = memoizedStates[MemoKeyPart2(timer, runningSum, humanValve, elephantValve)]
                ?: memoizedStates[MemoKeyPart2(timer, runningSum, elephantValve, humanValve)]
            if (memoized != null) {
                return memoized
            }

            val toEnable = valves.filter { it.value.flowRate > 0 }.count()

            if (timer >= TIME_LIMIT_PART_2) {
                //We've the time limit, see how much has flowed.
                memoizedStates[MemoKeyPart2(timer, runningSum, humanValve, elephantValve)] = runningSum
                memoizedStates[MemoKeyPart2(timer, runningSum, elephantValve, humanValve)] = runningSum
                return runningSum
            }

            // All enabled, nothing to do but wait.
            if (toEnable == enabled.size) {
                memoizedStates[MemoKeyPart2(timer, runningSum, humanValve, elephantValve)] = runningSum
                memoizedStates[MemoKeyPart2(timer, runningSum, elephantValve, humanValve)] = runningSum
                return runningSum
            }

            val humanMoves = ArrayList<Move>()
            val elephantMoves = ArrayList<Move>()

            fun buildMoves(actorMoves: MutableList<Move>) {
                val actor = if (actorMoves === humanMoves) humanValve else elephantValve
                val fromActor = if (actorMoves === humanMoves) fromHuman else fromElephant
                val actorOpening = if (actorMoves === humanMoves) humanOpening else elephantOpening
                if (actorOpening) {
                    // The actor has chosen a new direction, but is still opening. Execute the walk next turn.
                    actorMoves.add(Move(actor, false))
                } else {
                    if (timer + 1 <= TIME_LIMIT_PART_2 && !enabled.contains(actor) && actor.flowRate > 0) {
                        if (toEnable == enabled.size + 1) {
                            actorMoves.add(Move(actor, true))
                        } else {
                            actorMoves += actor.tunnelNames.map { Move(valves[it]!!, true) }
                        }
                    }

                    actorMoves += actor.tunnelNames
                        .mapNotNull {
                            valves[it]?.takeIf {
                                it != fromActor && it.isUseful(
                                    setOf(actor),
                                    enabled,
                                    valves
                                )
                            }
                        }
                        .map { Move(it, false) }
                }
            }
            buildMoves(humanMoves)
            buildMoves(elephantMoves)

            if (humanMoves.isEmpty()) humanMoves += Move(humanValve, false)
            if (elephantMoves.isEmpty()) elephantMoves += Move(elephantValve, false)

            var max = 0
            val childMaxes = humanMoves.parallelStream()
                .mapToInt { humanMove ->
                    val humanSum =
                        if (humanMove.open) runningSum + humanValve.individualTurnValuePart2[timer + 1]!! else runningSum
                    val humanEnabled = if (humanMove.open) enabled + humanValve else enabled
                    elephantMoves.parallelStream()
                        .filter { elephantMove -> !(elephantMove.toValve == humanMove.toValve && elephantMove.open == humanMove.open) }
                        .mapToInt { elephantMove ->
                            visitValves(
                                timer + 1,
                                humanMove.toValve,
                                elephantMove.toValve,
                                if (elephantMove.open) humanSum + elephantValve.individualTurnValuePart2[timer + 1]!! else humanSum,
                                if (humanMove.open || humanOpening) null else humanValve,
                                if (elephantMove.open || elephantOpening) null else elephantValve,
                                humanMove.open,
                                elephantMove.open,
                                if (elephantMove.open) humanEnabled + elephantValve else humanEnabled
                            )
                        }.max().orElse(0)
                }.max().orElse(0)
            max = if (childMaxes > max) childMaxes else max

            memoizedStates[MemoKeyPart2(timer, runningSum, humanValve, elephantValve)] = max
            memoizedStates[MemoKeyPart2(timer, runningSum, elephantValve, humanValve)] = max
            return max
        }
        return visitValves(0, valves["AA"]!!, valves["AA"]!!, 0, null, null, false, false, emptySet())
    }


    fun scenarioOne(textFile: String) =
        File(textFile).readLines()
            .let { parseValves(it) }
            .let { turnAllOn(it) }

    fun scenarioTwo(textFile: String) =
        File(textFile).readLines()
            .let { parseValves(it) }
            .let { turnAllOnWithAnElephant(it) }
}
