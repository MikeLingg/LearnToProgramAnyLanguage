package main

import (
	"fmt"
	"strconv"
)

func main() {
	var userInput string

	fmt.Println( "Type 1 and press enter." )
	fmt.Scanln( &userInput )

	// Function name has wrong capitalization
	enteredInteger, _ := strconv.atoi( userInput )

	fmt.Printf( "The user entered the integer %d\n", enteredInteger )
}
