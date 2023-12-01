package main

import (
	"strings"
	"unicode"
	"fmt"
	"os"
)


func solve_part1(content string) int {
	lines := strings.Fields(content)
	sum := 0
	for _,line := range lines{
		sum += getNumber(line)
	}
	fmt.Println(sum)
	return sum
}





func solve_part2(content string) int {
	lines := strings.Fields(content)
	names :=  []string {"one",
         "two",
         "three",
         "four",
         "five",
         "six",
         "seven",
          "eight",
          "nine"}
	replaced := []string  {"o1ne",
         "t2wo",
         "t3hree",
         "f4our",
         "f5ive",
         "s6ix",
         "s7even",
          "e8ight",
          "n9ine"}

	sum := 0
	for _, line := range lines{
		for i:= 0 ; i<9 ;i++{
			line = strings.Replace(line, names[i], replaced[i], -1)
		}
		sum += getNumber(line)
	}
	return sum
}



func getNumber(line string) int{
	filtered := []int{}
	for _,c := range line{
		if unicode.IsDigit(c){
		filtered = append(filtered,int(c - '0'))
			
		}
	}
	return filtered[0] * 10 + filtered[len(filtered) -1]
}

func main() {
	data, err := os.ReadFile("input.txt")
	if err != nil {
		fmt.Printf("Could not read file")
	}

	var content = string(data[:])
	fmt.Println(solve_part2(content))
}
