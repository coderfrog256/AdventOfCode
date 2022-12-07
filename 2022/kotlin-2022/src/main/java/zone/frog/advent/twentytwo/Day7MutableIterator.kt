package zone.frog.advent.twentytwo

import java.io.File
import java.util.stream.Collectors

// This is verbatim the first solution I came up with. Pretty nasty!
object Day7MutableIterator {
    data class FileEntry(val size: Int)

    data class Directory(val parent: Directory?, val directories: MutableMap<String, Directory> = HashMap(), val files: MutableMap<String, FileEntry> = HashMap()) {
        fun fileSize(): Int = files.values.sumOf { it.size } + directories.values.sumOf { it.fileSize() }
        fun allDirs(): Sequence<Directory> = sequence {
            yield(this@Directory)
            directories.values.forEach { yieldAll(it.allDirs()) }
        }

        override fun toString(): String {
            return "size=${fileSize()}" + ", subdirs=(" + directories.entries.stream().map { it.toString() }.collect(Collectors.joining(",")) + ")"
        }
    }

    private fun parseDirectory(directory: Directory, next: Iterator<String>): Directory {
        var line = next.next()
        while (next.hasNext()) {
            val parts = line.split(" ")
            if (parts[1] == "cd") {
                if(parts[2] == "..") {
                    return directory
                }
                val subdir = directory.directories.computeIfAbsent(parts[2]) { Directory(directory) }
                parseDirectory(subdir, next)
                line = next.takeIf { it.hasNext() }?.next() ?: return directory
            } else if (parts[1] == "ls") {
                while (next.hasNext()) {
                    line = next.next()
                    if (line.startsWith("$")) {
                        break
                    } else if(line.startsWith("dir")) {
                        directory.directories.computeIfAbsent(line.substringAfter("dir ")) {Directory(directory)}
                    } else {
                        val fileParts = line.split(" ")
                        directory.files.computeIfAbsent(fileParts[1]) {FileEntry(fileParts[0].toInt())}
                    }
                }
            }
        }
        return directory
    }

    fun scenarioOne(textFile: String) =
        parseDirectory(Directory(null), File(textFile).readLines().iterator().also { it.next() })
            .allDirs()
            .map { it.fileSize() }
            .filter { it <= 100000 }
            .sum()


    fun scenarioTwo(textFile: String): Int {
        val root = parseDirectory(Directory(null), File(textFile).readLines().iterator().also { it.next() })
        val totalUsed = root.fileSize()
        val unused = 70000000 - totalUsed
        val toFree = 30000000 - unused
        return root
            .allDirs()
            .map { it.fileSize() }
            .filter { it >= toFree }
            .sorted()
            .first()
    }
}