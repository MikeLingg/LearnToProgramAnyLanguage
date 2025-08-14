package main

import (
	"fmt"
	"strconv"
)

func main() {
	extraInput := 5
	var userInput string

	fmt.Println( "Type 1 and press enter." )
	fmt.Scanln( &userInput )

	// Function only takes 1 parameter, but we're passing 2
	enteredInteger, _ := strconv.Atoi( userInput, extraInput )

	fmt.Printf( "The user entered the integer %d\n", enteredInteger )
}
