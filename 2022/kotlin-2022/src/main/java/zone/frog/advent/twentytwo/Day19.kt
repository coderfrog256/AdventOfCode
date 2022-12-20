package zone.frog.advent.twentytwo

import java.io.File
import java.lang.IllegalArgumentException
import kotlin.reflect.KProperty1

object Day19 {
    enum class Resource {
        Ore,
        Clay,
        Obsidian,
        Geode,
    }

    data class RobotPrice(val resource: Resource, val oreCost: Int, val clayCost: Int, val obsidianCost: Int) {
        companion object {
            fun parsePrices(line: String): Pair<Int, Map<Resource, RobotPrice>> {
                val (lineNum, oreOre, clayOre, obsidianOre, obsidianClay, geodeOre, geodeObsidian) =
                    Regex("Blueprint (\\d+): Each ore robot costs (\\d+) ore. Each clay robot costs (\\d+) ore. Each obsidian robot costs (\\d+) ore and (\\d+) clay. Each geode robot costs (\\d+) ore and (\\d+) obsidian.")
                        .matchEntire(line)?.destructured
                        ?: throw IllegalArgumentException(line)
                return lineNum.toInt() to listOf(
                    RobotPrice(Resource.Ore, oreOre.toInt(), 0, 0),
                    RobotPrice(Resource.Clay, clayOre.toInt(), 0, 0),
                    RobotPrice(Resource.Obsidian, obsidianOre.toInt(), obsidianClay.toInt(), 0),
                    RobotPrice(Resource.Geode, geodeOre.toInt(), 0, geodeObsidian.toInt()),
                ).associateBy { it.resource }
            }
        }
    }

    data class Resources(val ore: Int, val clay: Int, val obsidian: Int, val geode: Int) {
        fun canAffordRobot(price: RobotPrice): Boolean {
            return ore >= price.oreCost && clay >= price.clayCost && obsidian >= price.obsidianCost
        }

        fun addRobot(price: RobotPrice): Resources {
            return when (price.resource) {
                Resource.Ore -> copy(ore = ore + 1)
                Resource.Clay -> copy(clay = clay + 1)
                Resource.Obsidian -> copy(obsidian = obsidian + 1)
                Resource.Geode -> copy(geode = geode + 1)
            }
        }

        fun applyRobots(currentRobots: Resources): Resources {
            return copy(
                ore = ore + currentRobots.ore,
                clay = clay + currentRobots.clay,
                obsidian = obsidian + currentRobots.obsidian,
                geode = geode + currentRobots.geode
            )
        }

        fun buyRobot(price: RobotPrice): Resources {
            return copy(
                ore = ore - price.oreCost,
                clay = clay - price.clayCost,
                obsidian = obsidian - price.obsidianCost
            )
        }
    }

    data class PurchaseChoice(val timeLeft: Int, val resources: Resources, val robots: Resources) {
        fun getNextChoices(robotOptions: Map<RobotPrice, Pair<Int, (Resources) -> Int>>): List<PurchaseChoice> {
            val choices = mutableListOf(PurchaseChoice(timeLeft - 1, resources.applyRobots(robots), robots))
            robotOptions.forEach { (robot, requirements) ->
                val (max, getter) = requirements
                if (resources.canAffordRobot(robot) && getter(resources) < max) {
                    val nextResources = resources.buyRobot(robot).applyRobots(robots)
                    val nextRobots = robots.addRobot(robot)
                    choices += PurchaseChoice(timeLeft - 1, nextResources, nextRobots)
                }
            }
            return choices
        }
    }

    private fun getGeodes(
        timeLimit: Int,
        includeIdNumberInScore: Boolean,
        idNumber: Int,
        robotPrices: Map<Resource, RobotPrice>
    ): Int {
        val initialRobots = Resources(1, 0, 0, 0)
        val initialResources = Resources(0, 0, 0, 0)

        val robotOptions = mapOf(
            robotPrices[Resource.Ore]!! to (robotPrices.values.sumOf { it.oreCost } to Resources::ore),
            robotPrices[Resource.Clay]!! to (robotPrices.values.maxOf { it.clayCost } to Resources::clay),
            robotPrices[Resource.Obsidian]!! to (robotPrices.values.maxOf { it.obsidianCost } to Resources::obsidian),
            robotPrices[Resource.Geode]!! to (Int.MAX_VALUE to Resources::geode),
        )

        val workStack = ArrayDeque<PurchaseChoice>()
        workStack += PurchaseChoice(timeLimit, initialResources, initialRobots)

        var max = 0
        var timer = System.currentTimeMillis()
        while (workStack.isNotEmpty()) {
            val work = workStack.removeLast()
            if (work.timeLeft == 0) {
                if (work.resources.geode > max) {
                    max = work.resources.geode
                    println(
                        "$idNumber - New Max $max, remaining ${workStack.size} (${workStack.maxOf { it.timeLeft }}, ${
                            workStack.map { it.timeLeft }.average()
                        })"
                    )
                } else if (System.currentTimeMillis() - timer > 60_000) {
                    println(
                        "$idNumber - max $max remaining ${workStack.size} (${workStack.maxOf { it.timeLeft }}, ${
                            workStack.map { it.timeLeft }.average()
                        })"
                    )
                    timer = System.currentTimeMillis()
                }
            } else {
                workStack += work.getNextChoices(robotOptions)
            }
        }
        return (if (includeIdNumberInScore) (idNumber * max) else max)
            .also { println("Blueprint $idNumber done. Max=$max") }
    }

    fun scenarioOne(textFile: String) =
        File(textFile).readLines().parallelStream()
            .map { RobotPrice.parsePrices(it) }
            .map { getGeodes(24, true, it.first, it.second) }
            .toList()
            .sum()

    fun scenarioTwo(textFile: String, blueprintsLeft: Int) =
        File(textFile).readLines().subList(0, blueprintsLeft).parallelStream()
            .map { RobotPrice.parsePrices(it) }
            .map { getGeodes(32, false, it.first, it.second) }
            .toList()
            .fold(1) { acc, i -> acc * i }
}