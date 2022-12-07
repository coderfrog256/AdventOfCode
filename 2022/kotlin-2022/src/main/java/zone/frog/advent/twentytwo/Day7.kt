package zone.frog.advent.twentytwo

import java.io.File

object Day7 {
    private fun directorySizes(dirTree: Map<String, Int>) = sequence {
        for (key in dirTree.keys) {
            yield(directorySize(dirTree, key))
        }
    }

    private fun directorySize(dirTree: Map<String, Int>, key: String) =
        dirTree.filter { key != it.key && it.key.startsWith(key) }.values.sum()

    private fun buildDirectoryTree(textFile: String) =
        File(textFile).readLines()
            .fold("" to mapOf("/" to 0)) { acc, line ->
                val (dir, tree) = acc
                if (line == "$ cd ..") {
                    dir.substringBeforeLast("/") to tree
                } else if (line == "$ cd /") {
                    //Special case the first line.
                    acc
                } else if (line.startsWith("$ cd ")) {
                    val subdir = "$dir/${line.substringAfterLast(" ")}"
                    subdir to (tree + (subdir to 0))
                } else if (line.startsWith("dir") || line == "$ ls") {
                    acc
                } else {
                    dir to tree + ("$dir/${line.substringAfterLast(" ")}" to line.substringBeforeLast(" ").toInt())
                }
            }
            .second

    fun scenarioOne(textFile: String) =
        directorySizes(buildDirectoryTree(textFile))
            .filter { it <= 100000 }
            .sum()

    fun scenarioTwo(textFile: String): Int {
        val directoryTree = buildDirectoryTree(textFile)
        val totalUsed = directorySize(directoryTree, "")
        val unused = 70000000 - totalUsed
        val toFree = 30000000 - unused
        return directorySizes(directoryTree)
            .filter { it >= toFree }
            .sorted()
            .first()
    }
}