package zone.frog.advent.twentytwo

import java.io.File

typealias CubeSpace = List<List<MutableList<Day18.Cube?>>>

object Day18 {
    data class Cube(val x: Int, val y: Int, val z: Int) {
        fun getPossibleNeighbors() = sequence {
            yield(Cube(x - 1, y, z))
            yield(Cube(x + 1, y, z))
            yield(Cube(x, y - 1, z))
            yield(Cube(x, y + 1, z))
            yield(Cube(x, y, z - 1))
            yield(Cube(x, y, z + 1))
        }
    }

    private fun parseCube(line: String) =
        line.split(",")
            .let { Cube(it[0].toInt(), it[1].toInt(), it[2].toInt()) }

    private fun getAdjacent(cube: Cube, cubeSpace: CubeSpace): List<Cube> {
        val (x, y, z) = cube
        val adjacent = mutableListOf<Cube>()
        cubeSpace[x][y].getOrNull(z - 1)?.let { adjacent.add(it) }
        cubeSpace[x][y].getOrNull(z + 1)?.let { adjacent.add(it) }
        cubeSpace[x].getOrNull(y - 1)?.get(z)?.let { adjacent.add(it) }
        cubeSpace[x].getOrNull(y + 1)?.get(z)?.let { adjacent.add(it) }
        cubeSpace.getOrNull(x - 1)?.get(y)?.get(z)?.let { adjacent.add(it) }
        cubeSpace.getOrNull(x + 1)?.get(y)?.get(z)?.let { adjacent.add(it) }
        return adjacent
    }

    private fun evalArea(cubes: List<Cube>): Int {
        val cubeSpace: CubeSpace = (0..cubes.maxOf { it.x })
            .map {
                (0..cubes.maxOf { it.y })
                    .map {
                        (0..cubes.maxOf { it.z })
                            .map { null }.toMutableList()
                    }
            }
        cubes.forEach { cubeSpace[it.x][it.y][it.z] = it }

        //Look at the cubes six ways, sum up the non-obscured edges.
        var sides = 0
        for (x in cubeSpace.indices) {
            for (y in cubeSpace[x].indices) {
                for (z in cubeSpace[x][y].indices) {
                    val cube = cubeSpace[x][y][z] ?: continue
                    sides += 6 - getAdjacent(cube, cubeSpace).size
                }
            }
        }

        return sides
    }

    private fun evalExternalArea(cubes: List<Cube>): Int {
        // Get our bounds. NOTE: Must be offset by one as the air is outside the furthest cubes.
        val minX = cubes.minOf { it.x } - 1
        val maxX = cubes.maxOf { it.x } + 1
        val minY = cubes.minOf { it.y } - 1
        val maxY = cubes.maxOf { it.y } + 1
        val minZ = cubes.minOf { it.z } - 1
        val maxZ = cubes.maxOf { it.z } + 1

        // Build a set of all air cubes (those not in input). Start with the first cube (guaranteed to be air as it's -1 for all dimensions)
        val airCubes = mutableSetOf<Cube>()
        val workQueue = ArrayDeque<Cube>()
        workQueue += Cube(minX, minY, minZ)

        while (!workQueue.isEmpty()) {
            val work = workQueue.removeFirst()
            airCubes += work

            work.getPossibleNeighbors()
                .filter { it.x in minX..maxX && it.y in minY..maxY && it.z in minZ..maxZ }
                .filter { !cubes.contains(it) && !airCubes.contains(it) && !workQueue.contains(it) }
                .forEach { workQueue.add(it) }
        }

        // How many of those air cubes have a neighbor that is from our input.
        return airCubes
            .flatMap { it.getPossibleNeighbors() }
            .count { cubes.contains(it) }
    }

    fun scenarioOne(textFile: String) =
        File(textFile).readLines()
            .map { parseCube(it) }
            .let { evalArea(it) }

    //1959 is too low
    fun scenarioTwo(textFile: String) =
        File(textFile).readLines()
            .map { parseCube(it) }
            .let { evalExternalArea(it) }
}
