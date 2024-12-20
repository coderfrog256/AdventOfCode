package main

import (
	"fmt"
	"os"
	"regexp"
	"sort"
	"strconv"
	"time"
)

type IndexInfo struct {
	index  int
	isDont bool
}

func partOne() {
	defer TimeTrack(time.Now())
	content, err := os.ReadFile(os.Args[1])
	if err != nil {
		fmt.Println(err)
		return
	}
	re := regexp.MustCompile(`mul\((\d+),(\d+)\)`)
	matches := re.FindAllStringSubmatch(string(content), -1)
	sum := 0
	for _, match := range matches {
		leftString, rightString := match[1], match[2]
		leftNumber, err := strconv.Atoi(leftString)
		if err != nil {
			fmt.Println(err)
			return
		}
		rightNumber, err := strconv.Atoi(rightString)
		if err != nil {
			fmt.Println(err)
			return
		}
		sum += (leftNumber * rightNumber)
	}
	fmt.Println(sum)
}

func partTwo() {
	defer TimeTrack(time.Now())
	content, err := os.ReadFile(os.Args[1])
	if err != nil {
		fmt.Println(err)
		return
	}
	// Find all indices of "don't" in the content
	re := regexp.MustCompile(`don't\(\)`)
	donts := re.FindAllStringIndex(string(content), -1)
	// Find all indices of "do" in the content
	re = regexp.MustCompile(`do\(\)`)
	dos := re.FindAllStringIndex(string(content), -1)

	to_remove := make([]IndexInfo, 0)
	for _, dont := range donts {
		// Add the index of the "don't" to the to_remove slice, with a boolean value of true
		to_remove = append(to_remove, IndexInfo{dont[0], true})
	}
	for _, do := range dos {
		// Add the index of the "do" to the to_remove slice, with a boolean value of false
		to_remove = append(to_remove, IndexInfo{do[0], false})
	}
	// Sort the to_remove slice by index, using a custom comparator function
	sort.Slice(to_remove, func(i, j int) bool {
		return to_remove[i].index < to_remove[j].index
	})
	pos := 0
	removed := 0
	removing := false
	for _, index := range to_remove {
		if !removing && index.isDont {
			removing = true
			pos = index.index
		} else if removing && !index.isDont {
			removing = false
			content = append(content[:pos-removed], content[index.index-removed+len("do()"):]...)
			removed += index.index - pos + len("do()")
		}
	}

	re = regexp.MustCompile(`mul\((\d+),(\d+)\)`)
	matches := re.FindAllStringSubmatch(string(content), -1)
	sum := 0
	for _, match := range matches {
		leftString, rightString := match[1], match[2]
		leftNumber, err := strconv.Atoi(leftString)
		if err != nil {
			fmt.Println(err)
			return
		}
		rightNumber, err := strconv.Atoi(rightString)
		if err != nil {
			fmt.Println(err)
			return
		}
		sum += (leftNumber * rightNumber)
	}
	fmt.Println(sum)
}

func main() {
	partOne()
	partTwo()
}
