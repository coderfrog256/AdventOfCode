package zone.frog.advent.twentytwo

import java.io.File
import java.lang.IllegalArgumentException
import java.util.concurrent.ConcurrentHashMap


object Day16 {
    data class MemoKey(val timer: Int, val runningSum: Int, val valveOne: Valve?, val valveTwo: Valve?)
    data class Move(val toValve: Valve?, val open: Boolean)

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

    private fun optimumValvePath(valves: Map<String, Valve>, includeElephant: Boolean): Int {
        val memoizedStates = ConcurrentHashMap<MemoKey, Int>()
        val timeLimit = if(includeElephant) TIME_LIMIT_PART_2 else TIME_LIMIT
        val valveDrainAmount = if(includeElephant) Valve::individualTurnValuePart2 else Valve::individualTurnValue
        val toEnable = valves.filter { it.value.flowRate > 0 }.count()
        fun visitValves(
            timer: Int,
            humanValve: Valve,
            elephantValve: Valve?,
            runningSum: Int,
            fromHuman: Valve?,
            fromElephant: Valve?,
            humanOpening: Boolean,
            elephantOpening: Boolean,
            enabled: Set<Valve>
        ): Int {
            val memoized = memoizedStates[MemoKey(timer, runningSum, humanValve, elephantValve)]
                ?: memoizedStates[MemoKey(timer, runningSum, elephantValve, humanValve)]
            if (memoized != null) {
                return memoized
            }

            if (timer >= timeLimit) {
                //We've the time limit, see how much has flowed.
                memoizedStates[MemoKey(timer, runningSum, humanValve, elephantValve)] = runningSum
                memoizedStates[MemoKey(timer, runningSum, elephantValve, humanValve)] = runningSum
                return runningSum
            }

            // All enabled, nothing to do but wait.
            if (toEnable == enabled.size) {
                memoizedStates[MemoKey(timer, runningSum, humanValve, elephantValve)] = runningSum
                memoizedStates[MemoKey(timer, runningSum, elephantValve, humanValve)] = runningSum
                return runningSum
            }

            val humanMoves = ArrayList<Move>()
            val elephantMoves = ArrayList<Move>()

            fun buildMoves(actorMoves: MutableList<Move>) {
                val actor = if (actorMoves === humanMoves) humanValve else elephantValve ?: return
                val fromActor = if (actorMoves === humanMoves) fromHuman else fromElephant
                val actorOpening = if (actorMoves === humanMoves) humanOpening else elephantOpening
                if (actorOpening) {
                    // The actor has chosen a new direction, but is still opening. Execute the walk next turn.
                    actorMoves.add(Move(actor, false))
                } else {
                    if (timer + 1 <= timeLimit && actor.flowRate > 0 && !enabled.contains(actor)) {
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
                        if (humanMove.open) runningSum + valveDrainAmount(humanValve)[timer + 1]!! else runningSum
                    val humanEnabled = if (humanMove.open) enabled + humanValve else enabled
                    elephantMoves.parallelStream()
                        .filter { elephantMove -> !(elephantMove.toValve == humanMove.toValve && elephantMove.open == humanMove.open) }
                        .mapToInt { elephantMove ->
                            visitValves(
                                timer + 1,
                                humanMove.toValve!!,
                                elephantMove.toValve,
                                if (elephantMove.open && elephantValve != null) humanSum + valveDrainAmount(elephantValve)[timer + 1]!! else humanSum,
                                if (humanMove.open || humanOpening) null else humanValve,
                                if (elephantMove.open || elephantOpening) null else elephantValve,
                                humanMove.open,
                                elephantMove.open,
                                if (elephantMove.open && elephantValve != null) humanEnabled + elephantValve else humanEnabled
                            )
                        }.max().orElse(0)
                }.max().orElse(0)
            max = if (childMaxes > max) childMaxes else max

            memoizedStates[MemoKey(timer, runningSum, humanValve, elephantValve)] = max
            memoizedStates[MemoKey(timer, runningSum, elephantValve, humanValve)] = max
            return max
        }
        return visitValves(
            timer = 0,
            humanValve = valves["AA"]!!,
            elephantValve = if(includeElephant) valves["AA"]!! else null,
            runningSum = 0,
            fromHuman = null,
            fromElephant = null,
            humanOpening = false,
            elephantOpening = false,
            enabled = emptySet()
        )
    }

    fun scenarioOne(textFile: String) =
        File(textFile).readLines()
            .let { parseValves(it) }
            .let { optimumValvePath(it, false) }

    fun scenarioTwo(textFile: String) =
        File(textFile).readLines()
            .let { parseValves(it) }
            .let { optimumValvePath(it, true) }
}
