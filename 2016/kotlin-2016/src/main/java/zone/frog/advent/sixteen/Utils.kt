package zone.frog.advent.sixteen

import kotlin.math.abs

typealias IntPair = Pair<Int, Int>
typealias LongPair = Pair<Long, Long>
typealias MutableGrid<T> = MutableList<MutableList<T>>
val intOrigin = 0 to 0

// Function that looks up elements from a list. If the index is out of bounds, it loops around.
// Negative indexes are supported
fun <T> List<T>.circularGet(index: Int): T {
    val i = index % this.size
    return if(i < 0) this[this.size + i]
    else this[i]
}

fun <T> Array<T>.circularGet(index: Int): T {
    val i = index % this.size
    return if(i < 0) this[this.size + i]
    else this[i]
}

fun IntPair.distance(rhs: IntPair) = abs(first - rhs.first) + abs(second - rhs.second)

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

