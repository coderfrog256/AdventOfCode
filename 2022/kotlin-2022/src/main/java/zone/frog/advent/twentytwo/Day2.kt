package zone.frog.advent.twentytwo

import java.io.File

object Day2 {
    enum class Result(val value: Int) {
        LOSS(0),
        TIE(3),
        WIN(6),
    }

    enum class Gesture(val letter: String, val pointValue: Int, val beats: String) {
        ROCK("A", 1, "SCISSORS"),
        PAPER("B", 2, "ROCK"),
        SCISSORS("C", 3, "PAPER");

        fun versus(other: Gesture) = when {
            other == this -> Result.TIE
            beats == other.name -> Result.WIN
            else -> Result.LOSS
        }

        companion object {
            fun forOpponent(letter: String) = Gesture.values().first { it.letter == letter }
        }
    }

    enum class SecondColumn(val gesture: Gesture, val result: Result) {
        X(Gesture.ROCK, Result.LOSS),
        Y(Gesture.PAPER, Result.TIE),
        Z(Gesture.SCISSORS, Result.WIN)
    }

    private fun getPoints(opponent: String, you: String): Int {
        val opponentGesture = Gesture.forOpponent(opponent)
        val yourGesture = SecondColumn.valueOf(you).gesture
        return yourGesture.versus(opponentGesture).value + yourGesture.pointValue
    }

    private fun rigMatch(opponent: String, you: String): Int {
        val opponentGesture = Gesture.forOpponent(opponent)
        return when(SecondColumn.valueOf(you).result) {
            Result.LOSS -> getPoints(opponent, SecondColumn.values().first { it.gesture.name == opponentGesture.beats }.name)
            Result.TIE -> getPoints(opponent, SecondColumn.values().first { it.gesture == opponentGesture }.name)
            Result.WIN -> getPoints(opponent, SecondColumn.values().first { Gesture.valueOf(it.gesture.beats).letter == opponent }.name)
        }
    }

    fun scenarioOne(textFile: String) =
        File(textFile).readLines()
            .map { it.split(" ") }
            .sumOf { getPoints(it[0], it[1]) }

    fun scenarioTwo(textFile: String) =
        File(textFile).readLines()
            .map { it.split(" ") }
            .sumOf { rigMatch(it[0], it[1]) }
}