package main

import "fmt"

func main() {
	
	// Error 1: Using parentheses instead of square brackets for array declaration
	var temperatures ( 24 )int
	
	// Error 2: Missing array size in declaration
	var testScores [ ]int
	
	// Error 3: Wrong syntax for array initialization
	bookNumbers := [ 5 ]int[ 1, 2, 3, 4, 5 ]
	
	// Error 4: Using curly braces instead of square brackets for array literal
	values := { 5 }int{ 1, 2, 3, 4, 5 }
	
	// Error 5: Missing type in array declaration
	var scores [ 10 ]
	
	// Error 6: Wrong syntax for array access
	temp := temperatures( 0 )
	
	// Error 7: Missing square brackets for array access
	score := testScores 2
	
	// Error 8: Missing comma in array literal
	badArray := [ 3 ]int{ 1 2 3 }
	
	// Error 9: Wrong syntax for multidimensional array initialization
	grid := [ 2, 2 ]int{ { 1, 2 }, { 3, 4 } }
	
	// Error 10: Missing closing brace in array literal
	incomplete := [ 3 ]int{ 1, 2, 3
	
	// Error 11: Wrong syntax for array length
	length := len( temperatures )()
	
	// Error 12: Using make for arrays instead of slices
	dynamicArray := make( [ 10 ]int )
	
	fmt.Println( temperatures )
}
