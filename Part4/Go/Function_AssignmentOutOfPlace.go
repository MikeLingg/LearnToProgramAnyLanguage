package main

import (
	"fmt"
	"strconv"
)

func main() {
	var userInput string

	fmt.Println( "Type 1 and press enter." )
	fmt.Scanln( &userInput )

	// Assignment operator placed incorrectly in declaration
	enteredInteger strconv.Atoi, _ := ( userInput )

	fmt.Printf( "The user entered the integer %d\n", enteredInteger )
}
