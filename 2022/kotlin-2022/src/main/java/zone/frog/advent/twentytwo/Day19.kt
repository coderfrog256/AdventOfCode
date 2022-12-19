package zone.frog.advent.twentytwo

import java.io.File
import java.lang.IllegalArgumentException

object Day19 {
    private const val TIME_LIMIT = 24

    data class Robot(val name: String, val oreCost: Int, val clayCost: Int, val obsidianCost: Int) {
        fun mine(resources: MutableMap<String, Int>) {
            resources.computeIfPresent(name) { _, old -> old + 1 }
        }

        fun canProduceRobot(resources: Resources): Boolean {
            val oreAvailable = resources.ore
            val clayAvailable = resources.clay
            val obsidianAvailable = resources.obsidian

            if (oreAvailable >= oreCost && clayAvailable >= clayCost && obsidianAvailable >= obsidianCost) {
                return true
            }
            return false
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

        var currentResources = initialResources
        var currentRobots = initialRobots

        fun turnsToBuild(price: Robot, currentResources: Resources, currentRobots: Resources): Int {
            val secondaryResourceToCheck = when (price) {
                geodePrice -> geodePrice.obsidianCost to Resources::obsidian
                obsidianPrice -> obsidianPrice.clayCost to Resources::clay
                clayPrice -> clayPrice.oreCost to Resources::ore
                orePrice -> orePrice.oreCost to Resources::ore
                else -> throw IllegalArgumentException(price.name)
            }
            val oreNeeded = price.oreCost
            val oreAvailable = currentResources.ore
            val oreRate = currentRobots.ore

            val turnsToRequiredOre = (oreNeeded - oreAvailable) / oreRate

            val secondaryResourceNeeded = secondaryResourceToCheck.first
            val secondaryResourceAvailable = secondaryResourceToCheck.second(currentResources)
            val secondaryResourceRate = secondaryResourceToCheck.second(currentRobots)

            if (secondaryResourceRate < 1) return Int.MAX_VALUE
            val turnsToRequiredSecondary = (secondaryResourceNeeded - secondaryResourceAvailable) / secondaryResourceRate
            return maxOf(turnsToRequiredSecondary, turnsToRequiredOre)
        }

        fun bestInvestment(currentResources: Resources, currentRobots: Resources): Robot? {
            if(geodePrice.canProduceRobot(currentResources)) {
                return geodePrice
            }
            if(obsidianPrice.canProduceRobot(currentResources)) {
                val robotsWithObsidian = currentResources.addRobot(obsidianPrice.name)
                val resourcesWithObsidian = obsidianPrice.consumeResources(currentResources.applyRobots(currentResources))
                if(turnsToBuild(geodePrice, resourcesWithObsidian, robotsWithObsidian) < turnsToBuild(geodePrice, currentResources.applyRobots(currentRobots), currentRobots)) {
                    return obsidianPrice
                }
            }
            if(clayPrice.canProduceRobot(currentResources)) {
                val robotsWithClay = currentResources.addRobot(clayPrice.name)
                val resourcesWithClay = clayPrice.consumeResources(currentResources.applyRobots(currentResources))
                if(turnsToBuild(obsidianPrice, resourcesWithClay, robotsWithClay) < turnsToBuild(obsidianPrice, currentResources.applyRobots(currentRobots), currentRobots)) {
                    return clayPrice
                }
            }
            if(orePrice.canProduceRobot(currentResources)) {
                val robotsWithOre = currentResources.addRobot(orePrice.name)
                val resourcesWithOre = orePrice.consumeResources(currentResources.applyRobots(currentResources))
                if(turnsToBuild(clayPrice, resourcesWithOre, robotsWithOre) < turnsToBuild(clayPrice, currentResources.applyRobots(currentRobots), currentRobots)) {
                    return orePrice
                }
            }
            return null
        }

        var timeLeft = TIME_LIMIT
        var newRobot: Robot? = null
        while (timeLeft-- > 0) {
            currentResources = currentResources.applyRobots(currentRobots)
            currentRobots = newRobot?.name?.let { currentRobots.addRobot(it) } ?: currentRobots
            newRobot = bestInvestment(currentResources, currentRobots)
            if(newRobot != null) {
                currentResources = newRobot.consumeResources(currentResources)
            }
        }
        val max = currentResources.geode
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