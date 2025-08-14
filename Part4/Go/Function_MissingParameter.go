package main

import (
	"fmt"
	"strconv"
)

func main() {
	var userInput string

	fmt.Println( "Type 1 and press enter." )
	fmt.Scanln( &userInput )

	// Function requires 1 parameter, but we're passing 0
	enteredInteger, _ := strconv.Atoi()

	fmt.Printf( "The user entered the integer %d\n", enteredInteger )
}
