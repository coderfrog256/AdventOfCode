package zone.frog.advent.fifteen

fun countParens(parens: String) = parens.map { if (it == '(') 1 else -1 }.sum()

fun countParensPart2(parens: String): Int? = parens
    .map { if (it == '(') 1 else -1 }
    .reduceIndexed { index, acc, i -> if (acc + i == -1) return@countParensPart2 index else acc + i }
    .let { null } // Edge case: If we never hit -1, return null.
