package main

import (
	"fmt"
	"os"
	"strings"
	"time"
)

type IntPair struct {
	left  int
	right int
}

func partOne() {
	defer TimeTrack(time.Now())
	content, err := os.ReadFile(os.Args[1])
	if err != nil {
		fmt.Println(err)
		return
	}
	// Split content on newlines
	lines := strings.Split(string(content), "\n")
	for _, fuck := range lines {
		fuck = fuck
		// I give up. This "unused variable is compiler error" is bullshit.
	}
}

func main() {
	partOne()
	//partTwo()
}
