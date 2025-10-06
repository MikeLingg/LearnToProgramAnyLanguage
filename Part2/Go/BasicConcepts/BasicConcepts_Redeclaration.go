package main

import "fmt"

func main() {
	// Do not redeclare variable names in most languages:
	duplicateCharacter := 'a'
	// This will cause compile error - variable already declared
	duplicateCharacter := 'b'

	fmt.Printf("duplicateCharacter: %c\n", duplicateCharacter)
}
