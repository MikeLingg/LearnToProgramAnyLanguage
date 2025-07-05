package main

import "fmt"

func main() {
	// So the ASCII table shows the tab symbol as TAB, but this doesn't work in programming.
	// I think in some programs this will crash, so it will be in a separate program.
	// Go doesn't allow multi-character rune literals - this will cause compile error
	charInvalid := 'TAB'
	fmt.Printf ( "Invalid char: %c\n", charInvalid )
}
