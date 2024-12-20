package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
	"time"
)

func isSafe(numbers []int) bool {
	signum := 0
	for i := 0; i < len(numbers)-1; i++ {
		diff := numbers[i] - numbers[i+1]
		if (signum != 0 && Signum(diff) != signum) || Abs(diff) > 3 {
			return false
		}
		signum = Signum(diff)
	}
	return true
}

func partOne() {
	defer TimeTrack(time.Now())
	file, err := os.Open(os.Args[1])
	if err != nil {
		fmt.Println(err)
		return
	}
	defer file.Close()
	scanner := bufio.NewScanner(file)
	valid := 0
	for scanner.Scan() {
		numbers := make([]int, 0)
		line := scanner.Text()
		for _, num := range strings.Fields(line) {
			n, err := strconv.Atoi(num)
			if err != nil {
				fmt.Println(err)
				return
			}
			numbers = append(numbers, n)
		}
		if isSafe(numbers) {
			valid++
		}
	}
	fmt.Println(valid)
}

func partTwo() {
	defer TimeTrack(time.Now())
	file, err := os.Open(os.Args[1])
	if err != nil {
		fmt.Println(err)
		return
	}
	defer file.Close()
	scanner := bufio.NewScanner(file)
	valid := 0
	for scanner.Scan() {
		numbers := make([]int, 0)
		line := scanner.Text()
		for _, num := range strings.Fields(line) {
			n, err := strconv.Atoi(num)
			if err != nil {
				fmt.Println(err)
				return
			}
			numbers = append(numbers, n)
		}
		for i := 0; i < len(numbers); i++ {
			withRemoved := make([]int, len(numbers))
			copy(withRemoved, numbers)
			if i < len(numbers)-1 {
				withRemoved = append(withRemoved[:i], withRemoved[i+1:]...)
			} else {
				withRemoved = withRemoved[:i]
			}
			if isSafe(withRemoved) {
				valid++
				break
			}
		}
	}
	fmt.Println(valid)
}

func main() {
	partOne()
	partTwo()
}
