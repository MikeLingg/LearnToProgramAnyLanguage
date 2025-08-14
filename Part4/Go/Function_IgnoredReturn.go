package main

import (
	"fmt"
	"strconv"
)

func main() {
	var userInput string
	var enteredInteger int

	fmt.Println( "Type 1 and press enter." )
	fmt.Scanln( &userInput )

	// Function called but return value ignored
	strconv.Atoi( userInput )

	fmt.Printf( "The user entered the integer %d\n", enteredInteger )
}
