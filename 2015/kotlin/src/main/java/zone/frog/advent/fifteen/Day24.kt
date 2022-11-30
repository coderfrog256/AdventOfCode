package zone.frog.advent.fifteen

fun buildGroups(
    groupCapacity: Long,
    weights: List<Long>,
    startingIndex: Int = 0,
    runningList: MutableList<Long> = ArrayList(),
    used: Set<Int> = emptySet()
): Sequence<List<Long>> {
    return sequence {
        val runningSum = runningList.sum()
        for (index in startingIndex.until(weights.size)) {
            val weight = weights[index]
            if (used.contains(index) || runningSum + weight > groupCapacity) {
                continue
            }
            if (runningSum + weight == groupCapacity) {
                yield(runningList + weight)
                continue
            }

            runningList.add(weight)
            yieldAll(buildGroups(groupCapacity, weights, index + 1, runningList, used))
            runningList.removeLast()
        }
    }
}

fun buildThreeGroupsScenarioOne(weights: List<Long>): Sequence<List<Long>> {
    return sequence {
        val groupCapacity = weights.sum() / 3
        for (group1 in buildGroups(groupCapacity, weights)) {
            var found = false
            val group1Indexes = group1.map { weights.indexOf(it) }.toSet()
            for (group2 in buildGroups(groupCapacity, weights, used = group1Indexes)) {
                val group12Indexes = group1Indexes + group2.map { weights.indexOf(it) }.toSet()
                for (group3 in buildGroups(groupCapacity, weights, used = group12Indexes)) {
                    yield(group1)
                    yield(group2)
                    yield(group3)
                    found = true
                    break
                }
                if(found) {
                    break
                }
            }
        }
    }
}

fun getGroupOneQEScenarioOne(weights: List<Long>): Long {
    return buildThreeGroupsScenarioOne(weights)
        .map { it.size to it.fold(1L) { acc, i -> acc * i } }
        .sortedWith(compareBy({ it.first }, {it.second}))
        .map { it.second }
        .first()
}

fun buildThreeGroupsScenarioTwo(weights: List<Long>): Sequence<List<Long>> {
    return sequence {
        val groupCapacity = weights.sum() / 4
        for (group1 in buildGroups(groupCapacity, weights)) {
            var found = false
            val group1Indexes = group1.map { weights.indexOf(it) }.toSet()
            for (group2 in buildGroups(groupCapacity, weights, used = group1Indexes)) {
                val group12Indexes = group1Indexes + group2.map { weights.indexOf(it) }.toSet()
                for (group3 in buildGroups(groupCapacity, weights, used = group12Indexes)) {
                    val group123Indexes = group12Indexes + group3.map { weights.indexOf(it) }.toSet()
                    for (group4 in buildGroups(groupCapacity, weights, used = group123Indexes)) {
                        yield(group1)
                        yield(group2)
                        yield(group3)
                        yield(group4)
                        found = true
                        break
                    }
                    if(found) {
                        break
                    }
                }
                if(found) {
                    break
                }
            }
        }
    }
}

fun getGroupOneQEScenarioTwo(weights: List<Long>): Long {
    return buildThreeGroupsScenarioTwo(weights)
        .map { it.size to it.fold(1L) { acc, i -> acc * i } }
        .sortedWith(compareBy({ it.first }, {it.second}))
        .map { it.second }
        .first()
}