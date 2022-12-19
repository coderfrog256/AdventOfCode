package zone.frog.advent.twentytwo

import java.io.File
import java.lang.IllegalArgumentException

object Day19 {
    private const val TIME_LIMIT = 24

    data class Robot(val name: String, val oreCost: Int, val clayCost: Int, val obsidianCost: Int) {
        fun canProduceRobot(resources: Resources): Boolean {
            return resources.ore >= oreCost && resources.clay >= clayCost && resources.obsidian >= obsidianCost
        }

        fun consumeResources(resources: Resources): Resources {
            return resources.copy(
                ore = resources.ore - oreCost,
                clay = resources.clay - clayCost,
                obsidian = resources.obsidian - obsidianCost
            )
        }
    }

    data class Resources(val ore: Int, val clay: Int, val obsidian: Int, val geode: Int) {
        fun addRobot(robo: String): Resources {
            return when (robo) {
                "ore" -> copy(ore = ore + 1)
                "clay" -> copy(clay = clay + 1)
                "obsidian" -> copy(obsidian = obsidian + 1)
                "geode" -> copy(geode = geode + 1)
                else -> throw IllegalArgumentException(robo)
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
    }

    private fun parseRobots(line: String): Pair<Int, Map<String, Robot>> {
        val (lineNum, oreOre, clayOre, obsidianOre, obsidianClay, geodeOre, geodeObsidian) = Regex("Blueprint (\\d+): Each ore robot costs (\\d+) ore. Each clay robot costs (\\d+) ore. Each obsidian robot costs (\\d+) ore and (\\d+) clay. Each geode robot costs (\\d+) ore and (\\d+) obsidian.")
            .matchEntire(line)?.destructured
            ?: throw IllegalArgumentException(line)
        return lineNum.toInt() to listOf(
            Robot("ore", oreOre.toInt(), 0, 0),
            Robot("clay", clayOre.toInt(), 0, 0),
            Robot("obsidian", obsidianOre.toInt(), obsidianClay.toInt(), 0),
            Robot("geode", geodeOre.toInt(), 0, geodeObsidian.toInt()),
        ).associateBy { it.name }
    }

    private fun getQualityNumber(idNumber: Int, robotPrices: Map<String, Robot>): Int {
        val initialRobots = Resources(1, 0, 0, 0)
        val initialResources = Resources(0, 0, 0, 0)

        val geodePrice = robotPrices["geode"]!!
        val obsidianPrice = robotPrices["obsidian"]!!
        val clayPrice = robotPrices["clay"]!!
        val orePrice = robotPrices["ore"]!!

        // I have no clue why the +1 is needed, but whatever.
        val maxOreNeeded = robotPrices.maxOf { it.value.oreCost+1 }
        val maxClayNeeded = robotPrices.maxOf { it.value.clayCost+1 }
        val maxObsidianNeeded = robotPrices.maxOf { it.value.obsidianCost+1 }

        data class PurchaseChoices(val timeLeft: Int, val resources: Resources, val robots: Resources){
            fun getNextChoices(): List<PurchaseChoices> {
                val choices = mutableListOf(PurchaseChoices(timeLeft-1, resources.applyRobots(robots), robots))
                if(orePrice.canProduceRobot(resources) && resources.ore < maxOreNeeded) {
                    val nextResources = orePrice.consumeResources(resources).applyRobots(robots)
                    val nextRobots = robots.addRobot(orePrice.name)
                    choices += PurchaseChoices(timeLeft-1, nextResources, nextRobots)
                }
                if(clayPrice.canProduceRobot(resources) && resources.clay < maxClayNeeded) {
                    val nextResources = clayPrice.consumeResources(resources).applyRobots(robots)
                    val nextRobots = robots.addRobot(clayPrice.name)
                    choices += PurchaseChoices(timeLeft-1, nextResources, nextRobots)
                }
                if(obsidianPrice.canProduceRobot(resources) && resources.obsidian < maxObsidianNeeded) {
                    val nextResources = obsidianPrice.consumeResources(resources).applyRobots(robots)
                    val nextRobots = robots.addRobot(obsidianPrice.name)
                    choices += PurchaseChoices(timeLeft-1, nextResources, nextRobots)
                }
                if(geodePrice.canProduceRobot(resources)) {
                    val nextResources = geodePrice.consumeResources(resources).applyRobots(robots)
                    val nextRobots = robots.addRobot(geodePrice.name)
                    choices += PurchaseChoices(timeLeft-1, nextResources, nextRobots)
                }
                return choices
            }
        }

        val workStack = ArrayDeque<PurchaseChoices>()
        workStack += PurchaseChoices(TIME_LIMIT, initialResources, initialRobots)

        var max = 0
        while(workStack.isNotEmpty()) {
            val work = workStack.removeLast()
            if(work.timeLeft == 0) {
                max = maxOf(max, work.resources.geode)
            } else {
                workStack += work.getNextChoices()
            }
        }
        return (idNumber * max)
            .also { println("Blueprint $idNumber done. Max=$max") }
    }

    fun scenarioOne(textFile: String) =
        File(textFile).readLines()
            .map { parseRobots(it) }
            .toList()
            .sumOf { getQualityNumber(it.first, it.second) }

    fun scenarioTwo(textFile: String) =
        File(textFile)
}