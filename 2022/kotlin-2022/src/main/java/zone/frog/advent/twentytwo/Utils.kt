package zone.frog.advent.twentytwo

typealias IntPair = Pair<Int, Int>
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