package zone.frog.advent.twentytwo

import java.io.File
import java.lang.IllegalArgumentException
import java.util.concurrent.CompletableFuture

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

        val oreNeeded = clayPrice.oreCost
//        val oreNeeded = robotPrices.maxOf { it.value.oreCost }
        val clayNeeded = obsidianPrice.clayCost
//        val clayNeeded = robotPrices.maxOf { it.value.clayCost }
        val obsidianNeeded = geodePrice.obsidianCost
//        val obsidianNeeded = robotPrices.maxOf { it.value.obsidianCost }

        var currentMax = -1
        fun evaluateOptions(
            timeRemaining: Int,
            robots: Resources,
            resources: Resources,
            nextRobot: String?
        ): CompletableFuture<Int> {
            if (timeRemaining == 0) {
                if(currentMax < resources.geode) {
                    currentMax = resources.geode
                    println("$idNumber - New max found: $currentMax")
                }
                return CompletableFuture.completedFuture(resources.geode)
            }
            val newRobotCount =
                nextRobot?.let { robo -> robots.addRobot(robo) } ?: robots

            val newResources = resources.copy(
                ore = resources.ore + robots.ore,
                clay = resources.clay + robots.clay,
                obsidian = resources.obsidian + robots.obsidian,
                geode = resources.geode + robots.geode
            )
            val futures = mutableListOf<CompletableFuture<CompletableFuture<Int>>>()
            if (geodePrice.canProduceRobot(newResources)) {
                futures.add(CompletableFuture.supplyAsync {
                    evaluateOptions(
                        timeRemaining - 1,
                        newRobotCount,
                        geodePrice.consumeResources(newResources),
                        geodePrice.name
                    )
                })
            }
            if (obsidianPrice.canProduceRobot(newResources) && robots.obsidian < obsidianNeeded) {
                futures.add(CompletableFuture.supplyAsync {
                    evaluateOptions(
                        timeRemaining - 1,
                        newRobotCount,
                        obsidianPrice.consumeResources(newResources),
                        obsidianPrice.name
                    )
                })
            }
            if (clayPrice.canProduceRobot(newResources) && robots.clay < clayNeeded) {
                futures.add(CompletableFuture.supplyAsync {
                    evaluateOptions(
                        timeRemaining - 1,
                        newRobotCount,
                        clayPrice.consumeResources(newResources),
                        clayPrice.name
                    )
                })
            }
            if (orePrice.canProduceRobot(newResources) && robots.ore < oreNeeded) {
                futures.add(CompletableFuture.supplyAsync {
                    evaluateOptions(
                        timeRemaining - 1,
                        newRobotCount,
                        orePrice.consumeResources(newResources),
                        orePrice.name
                    )
                })
            }
            futures.add(CompletableFuture.supplyAsync {
                evaluateOptions(
                    timeRemaining - 1,
                    newRobotCount,
                    newResources,
                    null
                )
            })
            return CompletableFuture.allOf(*futures.toTypedArray())
                .thenApply { futures.map { it.join() } }
                .thenApply { it.maxOfOrNull { it.join() } }
        }

        val max = evaluateOptions(TIME_LIMIT, initialRobots, initialResources, null).join()
        return (idNumber * max)
            .also { println("Blueprint $idNumber done. Max=$max") }
    }

    fun scenarioOne(textFile: String) =
        File(textFile).readLines()//.parallelStream()
            .map { parseRobots(it) }
            .toList()
            .sumOf { getQualityNumber(it.first, it.second) }

    fun scenarioTwo(textFile: String) =
        File(textFile)
}