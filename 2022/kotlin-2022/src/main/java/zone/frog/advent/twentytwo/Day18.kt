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

    private fun getActualNeighbors(cube: Cube, cubeSpace: CubeSpace) =
        cube.getPossibleNeighbors()
            .mapNotNull { cubeSpace.getOrNull(it.x)?.getOrNull(it.y)?.getOrNull(it.z) }

    private fun evalArea(cubes: List<Cube>): Int {
        val cubeSpace: CubeSpace = (0..cubes.maxOf { it.x })
            .map {
                (0..cubes.maxOf { it.y })
                    .map {
                        (0..cubes.maxOf { it.z })
                            .map { null }
                            .toMutableList()
                    }
            }
        cubes.forEach { cubeSpace[it.x][it.y][it.z] = it }

        //Start with six visible sides per cube. How many are obscured by a neighbor?
        var sides = 0
        for (x in cubeSpace.indices) {
            for (y in cubeSpace[x].indices) {
                for (z in cubeSpace[x][y].indices) {
                    val cube = cubeSpace[x][y][z] ?: continue
                    sides += 6 - getActualNeighbors(cube, cubeSpace).count()
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

        // Build a set of all air cubes (those not in input).
        // Start with the first cube (guaranteed to be air as it's min-1 for all dimensions) and expand everything.
        val airCubes = mutableSetOf<Cube>()
        val cubesToExpand = ArrayDeque<Cube>()
        cubesToExpand += Cube(minX, minY, minZ)

        while (!cubesToExpand.isEmpty()) {
            val airCube = cubesToExpand.removeFirst()
            airCubes += airCube

            // Get neighbors that are in range. If it's an unseen air-cube, add it to the queue to expand.
            airCube.getPossibleNeighbors()
                .filter { it.x in minX..maxX && it.y in minY..maxY && it.z in minZ..maxZ }
                .filter { !cubes.contains(it) && !airCubes.contains(it) && !cubesToExpand.contains(it) }
                .forEach { cubesToExpand.add(it) }
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

    fun scenarioTwo(textFile: String) =
        File(textFile).readLines()
            .map { parseCube(it) }
            .let { evalExternalArea(it) }
}
