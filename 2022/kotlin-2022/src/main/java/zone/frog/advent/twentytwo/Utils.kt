package zone.frog.advent.twentytwo

typealias IntPair = Pair<Int, Int>
typealias LongPair = Pair<Long, Long>
typealias MutableGrid<T> = MutableList<MutableList<T>>

// Returns a range that iterates over all elements, even if the RHS is after the LHS (this).
infix fun Int.bidirectionalRange(rhs: Int) =
    if(this < rhs) (this..rhs)
    else (rhs..this)

fun <T> List<List<T>>.inRange(position: IntPair): Boolean {
    return position.first >= 0 && position.first < this.size
            && position.second >= 0 && position.second < this[position.first].size
}

operator fun <T> List<List<T>>.get(position: IntPair): T {
    return this[position.first][position.second]
}

operator fun <T> MutableGrid<T>.set(position: IntPair, value: T) {
    this[position.first][position.second] = value
}

operator fun IntPair.plus(rhs: IntPair) = this.first+rhs.first to this.second+rhs.second


fun printState(xRange: IntRange, yRange: IntRange, position: IntPair, blizzard: List<Day24.Blizzard>) {
    val out = (0..yRange.last + 1).map { y ->
        (0..xRange.last + 1).map { x ->
            if (x == 0 || y == 0 || x == xRange.last + 1 || y == yRange.last + 1) '#' else '.'
        }.toMutableList()
    }.toMutableList()
    out[0][1] = '.'
    out.last()[out.last().size - 2] = '.'
    blizzard.forEach {
        if (out[it.position.second][it.position.first] == '.') out[it.position.second][it.position.first] =
            it.move.symbol
        else out[it.position.second][it.position.first] =
            if (out[it.position.second][it.position.first].isDigit()) (out[it.position.second][it.position.first].digitToInt() + 1).digitToChar() else '2'
    }
    if (out[position.second][position.first] != '.') out[position.second][position.first] = 'X'
    else out[position.second][position.first] = 'E'

    println(yRange.map { '=' }.joinToString(""))
    println(out.joinToString("\n") { it.joinToString("") })
    println()
}
