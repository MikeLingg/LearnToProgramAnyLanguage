package main

import "fmt"

func main() {
	// Do not mix your types in many languages
	// Go is strongly typed and will not allow implicit type conversions
	// These will cause compile errors
	var myInt int = 123.45
	var myFloat float32 = 'a'
	var myChar byte = 543.21

	fmt.Printf("myInt: %v\n", myInt)
	fmt.Printf("myFloat: %v\n", myFloat)
	fmt.Printf("myChar: %v\n", myChar)
}
