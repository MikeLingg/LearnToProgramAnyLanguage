package main

import "fmt"

func main() {
	// ERROR: Missing opening brace on same line as for statement
	for i := 0; i < 10; i++
	{
		fmt.Println(i)
	}

	// ERROR: Using parentheses around for loop condition
	for (i := 0; i < 10; i++) {
		fmt.Println(i)
	}

	// ERROR: Using 'while' keyword instead of 'for'
	i := 0
	while i < 10 {
		fmt.Println(i)
		i++
	}

	// ERROR: Using 'do' keyword
	do {
		fmt.Println(i)
		i++
	} while (i < 10)

	// ERROR: Semicolon after for statement
	for i := 0; i < 10; i++; {
		fmt.Println(i)
	}

	// ERROR: Missing semicolons in three-part for loop
	for i := 0, i < 10, i++ {
		fmt.Println(i)
	}

	// ERROR: Using assignment (=) instead of comparison (==) in condition
	counter := 0
	for counter = 10 {
		fmt.Println(counter)
		counter++
	}

	// ERROR: Using single = instead of := for short declaration
	for i = 0; i < 10; i++ {
		fmt.Println(i)
	}

	// ERROR: Using 'var' in for loop initialization
	for var i = 0; i < 10; i++ {
		fmt.Println(i)
	}

	// ERROR: Missing increment in for loop (when three parts are expected)
	for i := 0; i < 10 {
		fmt.Println(i)
	}

	// ERROR: Using ++ in wrong position (prefix and suffix together)
	for i := 0; i < 10; ++i++ {
		fmt.Println(i)
	}

	// ERROR: Using comma instead of semicolon in for parts
	for i := 0; i < 10, i++ {
		fmt.Println(i)
	}

	// ERROR: Missing condition entirely with semicolons
	for i := 0; ; i++ {
		fmt.Println(i)
	}

	// ERROR: Using 'break' with label that doesn't exist
	for i := 0; i < 10; i++ {
		if i == 5 {
			break outer
		}
	}

	// ERROR: Using 'continue' with label that doesn't exist
	for i := 0; i < 10; i++ {
		if i % 2 == 0 {
			continue outer
		}
	}

	// ERROR: Missing colon after label
	outer for i := 0; i < 10; i++ {
		fmt.Println(i)
	}

	// ERROR: Using 'end' or 'end for'
	for i := 0; i < 10; i++ {
		fmt.Println(i)
	}
	end for

	// ERROR: Using 'foreach' keyword
	arr := []int{1, 2, 3, 4, 5}
	foreach item in arr {
		fmt.Println(item)
	}

	// ERROR: Using 'in' without 'range'
	for item in arr {
		fmt.Println(item)
	}

	// ERROR: Using 'of' instead of range
	for item of arr {
		fmt.Println(item)
	}

	// ERROR: Missing 'range' keyword
	for i, v := arr {
		fmt.Println(i, v)
	}

	// ERROR: Using assignment = instead of := in range
	for i, v = range arr {
		fmt.Println(i, v)
	}

	// ERROR: Using wrong assignment operator in range
	for i, v == range arr {
		fmt.Println(i, v)
	}

	// ERROR: Missing := or = in range loop
	for i, v range arr {
		fmt.Println(i, v)
	}

	// ERROR: Using parentheses around range
	for i, v := range (arr) {
		fmt.Println(i, v)
	}

	// ERROR: Using square brackets instead of curly braces
	for i := 0; i < 10; i++ [
		fmt.Println(i)
	]

	// ERROR: Missing opening brace entirely
	for i := 0; i < 10; i++
		fmt.Println(i)

	// ERROR: Missing closing brace
	for i := 0; i < 10; i++ {
		fmt.Println(i)

	// ERROR: Using 'then' keyword
	for i := 0; i < 10; i++ then {
		fmt.Println(i)
	}

	// ERROR: Using double equals in initialization
	for i == 0; i < 10; i++ {
		fmt.Println(i)
	}

	// ERROR: Using increment in initialization
	for i++; i < 10; i++ {
		fmt.Println(i)
	}

	// ERROR: Using decrement when expecting increment
	for i := 0; i < 10; i-- {
		fmt.Println(i)
	}

	// ERROR: Missing variable name in range
	for := range arr {
		fmt.Println("item")
	}

	// ERROR: Using three variables in range (only 2 allowed)
	for i, v, x := range arr {
		fmt.Println(i, v, x)
	}

	// ERROR: Using 'loop' keyword (from other languages)
	loop i := 0; i < 10; i++ {
		fmt.Println(i)
	}

	// ERROR: Using colon instead of semicolon
	for i := 0: i < 10: i++ {
		fmt.Println(i)
	}

	// ERROR: Missing semicolon between parts
	for i := 0 i < 10; i++ {
		fmt.Println(i)
	}

	// ERROR: Extra semicolon at end
	for i := 0; i < 10; i++;; {
		fmt.Println(i)
	}

	// ERROR: Using 'to' instead of comparison
	for i := 0 to 10 {
		fmt.Println(i)
	}

	// ERROR: Using '..' for range
	for i := 0..10 {
		fmt.Println(i)
	}

	// ERROR: Trying to use multiple types in initialization
	for i := 0, j := 0.0; i < 10; i++ {
		fmt.Println(i)
	}

	// ERROR: Missing comma between multiple variable declarations
	for i, j := 0 0; i < 10; i++ {
		fmt.Println(i, j)
	}

	// ERROR: Using += in increment position incorrectly
	for i := 0; i < 10; i+ =1 {
		fmt.Println(i)
	}

	// ERROR: Using double increment
	for i := 0; i < 10; i++++ {
		fmt.Println(i)
	}

	// ERROR: Missing range value variables with blank identifier wrong syntax
	for range arr {
		fmt.Println("iteration")
	}

	// ERROR: Using wrong blank identifier syntax
	for _, v = range arr {
		fmt.Println(v)
	}

	// ERROR: Using 'until' instead of condition
	for until i >= 10 {
		fmt.Println(i)
		i++
	}

	// ERROR: Using 'repeat' keyword
	repeat {
		fmt.Println(i)
		i++
	} until i >= 10

	// ERROR: Missing condition with just semicolons
	for ; ; {
		break
	}

	// ERROR: Brace on wrong line (Go requires same line)
	for i := 0; i < 10; i++
	{
		fmt.Println(i)
	}

	// ERROR: Using else with for loop
	for i := 0; i < 10; i++ {
		fmt.Println(i)
	} else {
		fmt.Println("done")
	}

	// ERROR: Multiple increments without comma
	for i, j := 0, 0; i < 10; i++ j++ {
		fmt.Println(i, j)
	}

	// ERROR: Using 'step' keyword
	for i := 0; i < 10; step 2 {
		fmt.Println(i)
	}

	// ERROR: Trying to modify range variable
	for i := range arr {
		i = i + 1
		fmt.Println(i)
	}
}
