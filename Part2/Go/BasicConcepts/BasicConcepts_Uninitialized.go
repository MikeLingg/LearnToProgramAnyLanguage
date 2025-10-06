package main

import "fmt"

func main() {
	// I think in some programs this will cause a crash or failure to compile, so it will be in a separate program.
	// Go initializes variables to zero values, so this will print the zero value for rune (0)
	var myCharacter rune
	fmt.Printf("myCharacter: %c as int: %d\n", myCharacter, myCharacter)
}
