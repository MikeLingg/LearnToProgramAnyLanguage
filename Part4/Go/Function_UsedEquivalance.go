package main

import (
	"fmt"
	"strconv"
)

func main() {
	var userInput string

	fmt.Println( "Type 1 and press enter." )
	fmt.Scanln( &userInput )

	// Using == (comparison) instead of := (assignment)
	enteredInteger, _ == strconv.Atoi( userInput )

	fmt.Printf( "The user entered the integer %d\n", enteredInteger )
}
