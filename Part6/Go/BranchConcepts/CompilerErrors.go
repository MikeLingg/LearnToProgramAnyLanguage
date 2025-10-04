package main

import "fmt"

func main() {
	score := 85
	value := 5

	// ERROR 1: Missing braces around if body
	if score >= 90
		fmt.Printf("You got an A\n")

	// ERROR 2: Parentheses required around condition (Go doesn't require them but opening brace on wrong line)
	if score >= 90
	{
		fmt.Printf("You got an A\n")
	}

	// ERROR 3: Using 'then' keyword
	if score >= 90 then {
		fmt.Printf("You got an A\n")
	}

	// ERROR 4: Using 'elif' instead of 'else if'
	if score >= 90 {
		fmt.Printf("A\n")
	} elif score >= 80 {
		fmt.Printf("B\n")
	}

	// ERROR 5: Using 'elsif' instead of 'else if'
	if score >= 90 {
		fmt.Printf("A\n")
	} elsif score >= 80 {
		fmt.Printf("B\n")
	}

	// ERROR 6: Using 'elseif' (one word) instead of 'else if'
	if score >= 90 {
		fmt.Printf("A\n")
	} elseif score >= 80 {
		fmt.Printf("B\n")
	}

	// ERROR 7: else on new line (Go requires else on same line as closing brace)
	if score >= 90 {
		fmt.Printf("A\n")
	}
	else {
		fmt.Printf("Not A\n")
	}

	// ERROR 8: else if on new line
	if score >= 90 {
		fmt.Printf("A\n")
	}
	else if score >= 80 {
		fmt.Printf("B\n")
	}

	// ERROR 9: Missing braces in switch
	switch value
	case 1:
		fmt.Printf("One\n")
	default:
		fmt.Printf("Other\n")
	}

	// ERROR 10: Using colon after switch expression
	switch value: {
	case 1:
		fmt.Printf("One\n")
	default:
		fmt.Printf("Other\n")
	}

	// ERROR 11: Missing colon after case
	switch value {
	case 1
		fmt.Printf("One\n")
	default:
		fmt.Printf("Other\n")
	}

	// ERROR 12: Using break (not needed in Go, but not an error - remove this one)

	// ERROR 13: Duplicate case values
	switch value {
	case 1:
		fmt.Printf("First one\n")
	case 1:
		fmt.Printf("Second one\n")
	default:
		fmt.Printf("Other\n")
	}

	// ERROR 14: case outside of switch
	case 1:
		fmt.Printf("One\n")

	// ERROR 15: default outside of switch
	default:
		fmt.Printf("Default\n")

	// ERROR 16: else if without preceding if
	} else if score >= 80 {
		fmt.Printf("B\n")
	}

	// ERROR 17: else without preceding if
	} else {
		fmt.Printf("Failed\n")
	}

	// ERROR 18: Semicolon before opening brace in if
	if score >= 90; {
		fmt.Printf("A\n")
	}

	// ERROR 19: Assignment in condition without proper syntax
	var isValid bool
	if isValid = true {
		fmt.Printf("Valid\n")
	}

	// ERROR 20: Using parentheses around condition with brace on next line
	if (score >= 90)
	{
		fmt.Printf("A\n")
	}
}
