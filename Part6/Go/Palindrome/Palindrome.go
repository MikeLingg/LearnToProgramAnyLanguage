package main

import "fmt"

func main() {
	// Use nested conditions to test a palindrome up to a set length
	testString := "radar"
	isAPalindrome := false
	leftCharIndex := 0
	rightCharIndex := len(testString) - 1

	if testString[leftCharIndex] == testString[rightCharIndex] {
		leftCharIndex = leftCharIndex + 1
		rightCharIndex = rightCharIndex - 1
		if testString[leftCharIndex] == testString[rightCharIndex] {
			leftCharIndex = leftCharIndex + 1
			rightCharIndex = rightCharIndex - 1
			if testString[leftCharIndex] == testString[rightCharIndex] {
				isAPalindrome = true
			}
		}
	}

	fmt.Printf("Input string is a palindrome: %t\n", isAPalindrome)

	// Guarded conditions version with length protections
	testString = "radar"
	notAPalindrome := false
	leftCharIndex = 0
	rightCharIndex = len(testString) - 1

	if leftCharIndex < len(testString) {
		if testString[leftCharIndex] != testString[rightCharIndex] {
			notAPalindrome = true
		}
	}

	if leftCharIndex < len(testString) {
		if notAPalindrome != true {
			leftCharIndex = leftCharIndex + 1
			rightCharIndex = rightCharIndex - 1
			if testString[leftCharIndex] != testString[rightCharIndex] {
				notAPalindrome = true
			}
		}
	}

	if leftCharIndex < len(testString) {
		if notAPalindrome != true {
			leftCharIndex = leftCharIndex + 1
			rightCharIndex = rightCharIndex - 1
			if testString[leftCharIndex] != testString[rightCharIndex] {
				notAPalindrome = true
			}
		}
	}

	fmt.Printf("Input string is a palindrome: %t\n", !notAPalindrome)
}
