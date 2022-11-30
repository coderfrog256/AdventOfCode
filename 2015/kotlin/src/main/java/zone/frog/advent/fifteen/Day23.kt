package zone.frog.advent.fifteen

data class ComputerState(
    val instructionPointer: Int = 0,
    val registerA: UInt = 0u,
    val registerB: UInt = 0u
) {
    fun useRegister(register: String, func: (UInt) -> UInt): ComputerState {
        return if (register == "a")
            copy(registerA = func(registerA), instructionPointer = instructionPointer + 1)
        else
            copy(registerB = func(registerB), instructionPointer = instructionPointer + 1)
    }

    fun jump(register: String? = null, func: (UInt, Int) -> Int): ComputerState {
        return if (register == "a")
            copy(instructionPointer = func(registerA, instructionPointer))
        else
            copy(instructionPointer = func(registerB, instructionPointer))
    }
}

fun applyOperation(line: String, state: ComputerState): ComputerState {
    val parts = line.replace(",", "").split(" ")
    return when (parts[0]) {
        "hlf" -> state.useRegister(parts[1]) { it / 2u }
        "tpl" -> state.useRegister(parts[1]) { it * 3u }
        "inc" -> state.useRegister(parts[1]) { it + 1u }
        "jmp" -> state.jump { _, ip -> ip + parts[1].toInt() }
        "jie" -> state.jump(parts[1]) { reg, ip -> if (reg % 2u == 0u) ip + parts[2].toInt() else ip + 1 }
        "jio" -> state.jump(parts[1]) { reg, ip -> if (reg == 1u) ip + parts[2].toInt() else ip + 1 }
        else -> throw IllegalArgumentException(line)
    }
}

fun runProgram(program: String, initialState: ComputerState = ComputerState()): ComputerState {
    val lines = program.split("\n").filter { it.isNotEmpty() }
    var state = initialState
    while (state.instructionPointer < lines.size) {
        state = applyOperation(lines[state.instructionPointer], state)
    }
    return state
}