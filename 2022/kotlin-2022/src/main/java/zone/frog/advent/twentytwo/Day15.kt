package zone.frog.advent.twentytwo

import java.io.File
import java.lang.IllegalArgumentException
import kotlin.math.abs
import kotlin.math.max
import kotlin.math.min

object Day15 {
    data class Sensor(val thisPosition: LongPair, val beaconPosition: LongPair) {
        val beaconDistance = manhattanDistance(thisPosition, beaconPosition)

        fun inRange(point: LongPair): Boolean {
            return manhattanDistance(thisPosition, point) <= beaconDistance
        }

        fun getRangesForColumn(x: Long): LongRange? {
            val xDistance = abs(thisPosition.first - x)
            if (xDistance <= beaconDistance) {
                val yDistance = beaconDistance - xDistance
                return LongRange(thisPosition.second - yDistance, thisPosition.second + yDistance)
            }
            return null
        }

        companion object {
            fun parse(line: String): Sensor {
                val (x, y, beaconX, beaconY) = Regex("Sensor at x=(-?\\d+), y=(-?\\d+): closest beacon is at x=(-?\\d+), y=(-?\\d+)")
                    .matchEntire(line)?.destructured ?: throw IllegalArgumentException(line)
                return Sensor(x.toLong() to y.toLong(), beaconX.toLong() to beaconY.toLong())
            }
        }
    }

    fun manhattanDistance(from: LongPair, to: LongPair): Long {
        return abs(from.first - to.first) + abs(from.second - to.second)
    }

    private fun beaconsNotInRange(y: Long, sensors: List<Sensor>): Int {
        val start = sensors.minOfOrNull {
            min(
                -it.beaconDistance,
                min(0, min(it.thisPosition.first, it.beaconPosition.first))
            )
        } ?: throw IllegalArgumentException("No beacons provided")
        val end = sensors.maxOfOrNull {
            max(
                it.thisPosition.first + it.beaconDistance,
                max(it.thisPosition.first, it.beaconPosition.first)
            )
        }  ?: throw IllegalArgumentException("No beacons provided")

        val beaconPositions = sensors.map { it.beaconPosition }.toSet()

        return (start..end).asSequence()
            .map { it to y }
            .filter { position -> !beaconPositions.contains(position) }
            .filter { position -> sensors.any { it.inRange(position) } }
            .count()
    }

    // Totally different approach from part 1. It doesn't scale.
    // Instead of checking each inclusion, just merge the ranges and look for holes.
    private fun findHoleInSensorRange(upperBound: Long, sensors: List<Sensor>): Long {
        for(x in 0..upperBound) {
            val ranges = sensors.mapNotNull { it.getRangesForColumn(x) }.sortedBy { it.first }
            val merged = mutableListOf<LongRange>()
            var current = ranges[0]
            for(range in ranges) {
                if(range.first > current.last) {
                    merged.add(current)
                    current = range
                } else if(range.last > current.last) {
                    current = LongRange(current.first, range.last)
                }
            }
            if(!merged.contains(current)) {
                merged.add(current)
            }

            for(i in merged.indices) {
                val range = merged[i]
                if(i == 0 && range.first > 0 // Hole on left?
                    || (i+1 < merged.size && range.last+1 < merged[i+1].first) // Hole between ranges?
                    || (i+1 == merged.size && range.last+1 < upperBound)) { //Hole on right?
                    return (x * 4000000L) + range.last+1
                }
            }
        }
        throw IllegalArgumentException("Could not find a match.")
    }

    fun scenarioOne(textFile: String, yPosition: Int) =
        File(textFile).readLines()
            .map { Sensor.parse(it) }
            .let { beaconsNotInRange(yPosition.toLong(), it) }

    fun scenarioTwo(textFile: String, upperBound: Long) =
        File(textFile).readLines()
            .map { Sensor.parse(it) }
            .let { findHoleInSensorRange(upperBound, it) }
}
