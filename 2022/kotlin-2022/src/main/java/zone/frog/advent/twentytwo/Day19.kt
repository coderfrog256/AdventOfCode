package zone.frog.advent.twentytwo

import java.io.File
import java.lang.IllegalArgumentException

object Day19 {
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

    private fun getQualityNumber(
        timeLimit: Int,
        includeIdNumberInScore: Boolean,
        idNumber: Int,
        robotPrices: Map<String, Robot>
    ): Int {
        val initialRobots = Resources(1, 0, 0, 0)
        val initialResources = Resources(0, 0, 0, 0)

        val geodePrice = robotPrices["geode"]!!
        val obsidianPrice = robotPrices["obsidian"]!!
        val clayPrice = robotPrices["clay"]!!
        val orePrice = robotPrices["ore"]!!

        // I have no clue why the +1 is needed, but whatever.
        val maxOreNeeded = robotPrices.values.sumOf { it.oreCost }
        val maxClayNeeded = robotPrices.values.maxOf { it.clayCost+1 }
        val maxObsidianNeeded = robotPrices.values.maxOf { it.obsidianCost+1 }

        data class PurchaseChoices(val timeLeft: Int, val resources: Resources, val robots: Resources) {
            fun getNextChoices(): List<PurchaseChoices> {
                val choices = mutableListOf(PurchaseChoices(timeLeft - 1, resources.applyRobots(robots), robots))
                if (orePrice.canProduceRobot(resources) && resources.ore < maxOreNeeded) {
                    val nextResources = orePrice.consumeResources(resources).applyRobots(robots)
                    val nextRobots = robots.addRobot(orePrice.name)
                    choices += PurchaseChoices(timeLeft - 1, nextResources, nextRobots)
                }
                if (clayPrice.canProduceRobot(resources) && resources.clay < maxClayNeeded) {
                    val nextResources = clayPrice.consumeResources(resources).applyRobots(robots)
                    val nextRobots = robots.addRobot(clayPrice.name)
                    choices += PurchaseChoices(timeLeft - 1, nextResources, nextRobots)
                }
                if (obsidianPrice.canProduceRobot(resources) && resources.obsidian < maxObsidianNeeded) {
                    val nextResources = obsidianPrice.consumeResources(resources).applyRobots(robots)
                    val nextRobots = robots.addRobot(obsidianPrice.name)
                    choices += PurchaseChoices(timeLeft - 1, nextResources, nextRobots)
                }
                if (geodePrice.canProduceRobot(resources)) {
                    val nextResources = geodePrice.consumeResources(resources).applyRobots(robots)
                    val nextRobots = robots.addRobot(geodePrice.name)
                    choices += PurchaseChoices(timeLeft - 1, nextResources, nextRobots)
                }
                return choices
            }
        }

        val workStack = ArrayDeque<PurchaseChoices>()
        workStack += PurchaseChoices(timeLimit, initialResources, initialRobots)

        var max = 0
        var timer = System.currentTimeMillis()
        while (workStack.isNotEmpty()) {
            val work = workStack.removeLast()
            if (work.timeLeft == 0) {
                if (work.resources.geode > max) {
                    max = work.resources.geode
                    println("$idNumber - New Max $max, remaining ${workStack.size} (${workStack.maxOf { it.timeLeft }}, ${workStack.map { it.timeLeft }.average()})")
                } else if(System.currentTimeMillis() - timer > 60_000) {
                    println("$idNumber - max $max remaining ${workStack.size} (${workStack.maxOf { it.timeLeft }}, ${workStack.map { it.timeLeft }.average()})")
                    timer = System.currentTimeMillis()
                }
            } else {
                workStack += work.getNextChoices()
            }
        }
        return (if (includeIdNumberInScore) (idNumber * max) else max)
            .also { println("Blueprint $idNumber done. Max=$max") }
    }

    fun scenarioOne(textFile: String) =
        File(textFile).readLines().parallelStream()
            .map { parseRobots(it) }
            .map { getQualityNumber(24, true, it.first, it.second) }
            .toList()
            .sum()

    fun scenarioTwo(textFile: String, blueprintsLeft: Int) =
        File(textFile).readLines().subList(0, blueprintsLeft).parallelStream()
            .map { parseRobots(it) }
            .map { getQualityNumber(32, false, it.first, it.second) }
            .toList()
            .fold(1) { acc, i -> acc * i }
}