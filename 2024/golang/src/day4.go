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
	grid := make(map[IntPair]rune)
	maxX, maxY := len(lines[0]), len(lines)
	for y, line := range lines {
		for x, char := range line {
			grid[IntPair{x, y}] = char
		}
	}

	occurances := 0
	// Define patterns and directions
	patterns := []string{"XMAS", "SAMX"}
	directions := []IntPair{
		{1, 0}, {0, 1}, {1, 1}, {-1, 1},
	}

	for y := 0; y < maxY; y++ {
		for x := 0; x < maxX; x++ {
			for _, pattern := range patterns {
				for _, dir := range directions {
					matched := true
					for i := 0; i < len(pattern); i++ {
						nx, ny := x+dir.left*i, y+dir.right*i
						if grid[IntPair{nx, ny}] != rune(pattern[i]) {
							matched = false
							break
						}
					}
					if matched {
						occurances++
					}
				}
			}
		}
	}
	fmt.Println("occurances:", occurances)
}

func main() {
	partOne()
	// partTwo()
}
