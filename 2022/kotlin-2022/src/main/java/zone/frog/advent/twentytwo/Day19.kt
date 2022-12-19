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

        fun excessOre(price: Robot, currentResources: Resources, currentRobots: Resources): Int? {
            val secondaryResourceToCheck = when (price) {
                geodePrice -> geodePrice.obsidianCost to Resources::obsidian
                obsidianPrice -> obsidianPrice.clayCost to Resources::clay
                clayPrice -> clayPrice.oreCost to Resources::ore
                else -> throw IllegalArgumentException(price.name)
            }

            val secondaryResourceNeeded = secondaryResourceToCheck.first
            val secondaryResourceAvailable = secondaryResourceToCheck.second(currentResources)
            val secondaryResourceRate = secondaryResourceToCheck.second(currentRobots)

            if (secondaryResourceRate < 1) return null
            val turnsNeededForSecondary = (secondaryResourceNeeded - secondaryResourceAvailable) / secondaryResourceRate
            val oreAvailableAtBuildTime =
                (price.oreCost - currentResources.ore) + (currentRobots.ore * turnsNeededForSecondary)

            return oreAvailableAtBuildTime - price.oreCost
        }

        val FUCK = ::excessOre

        var timeLeft = TIME_LIMIT
        var newRobot: Robot? = null
        while (timeLeft-- > 0) {
            currentResources = currentResources.applyRobots(currentRobots)
            currentRobots = newRobot?.name?.let { currentRobots.addRobot(it) } ?: currentRobots
            newRobot = null
            val oreAvailable = currentResources.ore

            if (geodePrice.canProduceRobot(currentResources)) {
                newRobot = geodePrice
                currentResources = geodePrice.consumeResources(currentResources)
                continue
            }
            val excessGeodeOreIfObsidianAdded = excessOre(geodePrice, currentResources, currentRobots)
            if (obsidianPrice.canProduceRobot(currentResources)
                && (excessGeodeOreIfObsidianAdded == null || oreAvailable > excessGeodeOreIfObsidianAdded)
            ) {
                newRobot = obsidianPrice
                currentResources = obsidianPrice.consumeResources(currentResources)
                continue
            }
            val excessGeodeOreIfClayAdded = excessOre(geodePrice, currentResources, currentRobots)
            val excessObsidianOreIfClayAdded = excessOre(obsidianPrice, currentResources, currentRobots)
            if (clayPrice.canProduceRobot(currentResources)
                && (excessGeodeOreIfClayAdded == null || oreAvailable > excessGeodeOreIfClayAdded)
                && (excessObsidianOreIfClayAdded == null || oreAvailable > excessObsidianOreIfClayAdded)
            ) {
                newRobot = clayPrice
                currentResources = clayPrice.consumeResources(currentResources)
                continue
            }
            val excessGeodeOreIfOreAdded = excessOre(geodePrice, currentResources, currentRobots)
            val excessObsidianOreIfOreAdded = excessOre(obsidianPrice, currentResources, currentRobots)
            val excessClayOreIfOreAdded = excessOre(clayPrice, currentResources, currentRobots)
            if (obsidianPrice.canProduceRobot(currentResources)
                && (excessGeodeOreIfOreAdded == null || oreAvailable > excessGeodeOreIfOreAdded)
                && (excessObsidianOreIfOreAdded == null || oreAvailable > excessObsidianOreIfOreAdded)
                && (excessClayOreIfOreAdded == null || oreAvailable > excessClayOreIfOreAdded)
            ) {
                newRobot = orePrice
                currentResources = orePrice.consumeResources(currentResources)
                continue
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