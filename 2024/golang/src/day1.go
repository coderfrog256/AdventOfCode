package main

import (
	"bufio"
	"fmt"
	"os"
	"sort"
	"time"
)

func partOne() {
	defer TimeTrack(time.Now())
	leftNumbers := make([]int, 0)
	rightNumbers := make([]int, 0)
	file, err := os.Open(os.Args[1])
	if err != nil {
		fmt.Println(err)
		return
	}
	defer file.Close()
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		var leftNumber, rightNumber int
		_, err := fmt.Sscanf(scanner.Text(), "%d %d", &leftNumber, &rightNumber)
		if err != nil {
			fmt.Println(err)
			return
		}
		leftNumbers = append(leftNumbers, leftNumber)
		rightNumbers = append(rightNumbers, rightNumber)
	}
	// Sort the left and right number lists
	sort.Ints(leftNumbers)
	sort.Ints(rightNumbers)

	sum := 0
	for i := 0; i < len(leftNumbers); i++ {
		sum += Abs(leftNumbers[i] - rightNumbers[i])
	}
	fmt.Printf("%.0f\n", sum)
}

func partTwo() {
	defer TimeTrack(time.Now())
	leftNumbers := make([]int, 0)
	rightNumbers := make(map[int]int)
	file, err := os.Open(os.Args[1])
	if err != nil {
		fmt.Println(err)
		return
	}
	defer file.Close()
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		var leftNumber, rightNumber int
		_, err := fmt.Sscanf(scanner.Text(), "%d %d", &leftNumber, &rightNumber)
		if err != nil {
			fmt.Println(err)
			return
		}
		leftNumbers = append(leftNumbers, leftNumber)
		rightNumbers[rightNumber] = rightNumbers[rightNumber] + 1
	}
	sum := 0
	for i := 0; i < len(leftNumbers); i++ {
		sum += leftNumbers[i] * rightNumbers[leftNumbers[i]]
	}
	fmt.Println(sum)
}

func main() {
	partOne()
	partTwo()
}
