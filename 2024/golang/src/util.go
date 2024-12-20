package main

import (
	"fmt"
	"time"
)

func TimeTrack(start time.Time) {
    elapsed := time.Since(start)
    fmt.Printf("Execution time: %s\n", elapsed)
}

// I'm feeling like this language's stdlib is kind of shit.
// Why is abs only for floats?
func Abs(x int) int {
	if x < 0 {
		return -x
	}
	return x
}

// Yeah, this standard library is trash.
func Signum(x int) int {
	if x < 0 {
		return -1
	} else if x > 0 {
		return 1
	}
	return 0
}
