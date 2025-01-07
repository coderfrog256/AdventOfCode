package zone.frog.advent.twentyfour

import javax.swing.JFrame
import javax.swing.JProgressBar
import javax.swing.WindowConstants
import java.awt.BorderLayout
import javax.swing.JOptionPane

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

fun Any.toClipboard() {
    val process = ProcessBuilder("xclip", "-selection", "clipboard").start()
    process.outputStream.bufferedWriter().use { it.write(toString()) }
    JOptionPane.showMessageDialog(null, toString());
    process.waitFor()
}

// ProgressBar class, wraps a Collection and is iterable.
// Returns an iterator that iterates over the elements of the collection.
// As the iterator iterates over the elements, it updates the progress bar.
// Also includes a text field that displays the ETA, based on the rate of progress.
class ProgressBar<T>(val iterable: Iterable<T>) : Iterable<T> {
    private val progressBar = JProgressBar(0, if (iterable is Collection<*>) iterable.size else 100)
    private val frame = JFrame("Progress")
    private val etaField = javax.swing.JTextField()
    private var startTime = System.currentTimeMillis()
    private var lastTime = startTime

    init {
        frame.defaultCloseOperation = WindowConstants.DISPOSE_ON_CLOSE
        frame.add(progressBar, BorderLayout.NORTH)
        frame.add(etaField, BorderLayout.SOUTH)
        frame.pack()
        frame.isVisible = true
    }

    override fun iterator(): Iterator<T> {
        return object : Iterator<T> {
            private val iterator = iterable.iterator()
            private var index = 0

            override fun hasNext(): Boolean {
                return iterator.hasNext()
            }

            override fun next(): T {
                val currentTime = System.currentTimeMillis()
                val elapsedTime = currentTime - startTime
                val rate = index.toDouble() / elapsedTime
                if (iterable is Collection<*>) {
                    progressBar.value = ++index
                    val remaining = iterable.size - index
                    val eta = remaining / rate
                    etaField.text = "ETA: ${eta / 1000} seconds"
                } else {
                    ++index
                    // Set the etaField to the current index and the rate in elements per second
                    // Print the rate as regular text, not scientific notation
                    etaField.text = "Index: $index, Rate: ${String.format("%.2f", rate * 1000)} elements per second"
                }
                lastTime = currentTime

                val result = iterator.next()
                if (!iterator.hasNext()) {
                    frame.isVisible = false
                    frame.dispose()
                }
                return result
            }
        }
    }
}