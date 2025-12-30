package main

import (
	"fmt"
	"unsafe"
)

func main () {
	// Note the structured example is assuming zero based indexing.
	// One based index languages will differ.
	// Also note how some references are array location, with ranges from first to last,
	// while indexes being zero based are 0 to size - 1.

	temperatures := [ 24 ]int{ 55, 58, 60, 65, 70, 73, 76, 79, 81, 83, 84, 85, 85, 84, 83, 81, 78, 75, 72, 69, 65, 62, 59, 57 }
	testScores := [ 6 ]int{ 95, 75, 86, 86, 78, 94 }
	bookNumber := [ 5 ]int{ 12495, 35786, 15863, 84962, 42697 }

	// 10th entry (0-based index 9)
	fmt.Printf ( "Temperature at tenth hour is %d\n", temperatures[ 9 ] )
	// 4th entry (0-based index 3)
	fmt.Printf ( "Fourth student grade is %d\n", testScores[ 3 ] )

	// 2nd entry (0-based index 1)
	bookTwoIndex := 1
	fmt.Printf ( "Second book index is %d\n", bookNumber[ bookTwoIndex ] )

	// First and last 0 based indexes, 0 and array size - 1.
	hourCount := 24
	firstTemperatureIndex := 0
	lastTemperatureIndex := hourCount - 1
	fmt.Printf ( "First temperature is %d\n", temperatures[ firstTemperatureIndex ] )
	fmt.Printf ( "Last temperature is %d\n", temperatures[ lastTemperatureIndex ] )

	// set temperature first entry to 65
	temperatures[ 0 ] = 65
	fmt.Printf ( "First temperature is now %d\n", temperatures[ 0 ] )

	// set testScores fourth entry to 99
	testScores[ 3 ] = 99
	fmt.Printf ( "Fourth test score is now %d\n", testScores[ 3 ] )

	// set bookNumber at index third entry to 75681
	bookIndex := 2
	bookNumber[ bookIndex ] = 75681
	fmt.Printf ( "Third book number is now %d\n", bookNumber[ bookIndex ] )

	// Large arrays - Go initializes to zero values
	largeArraySize := 10000
	var largeArray [ 10000 ]bool
	var largeArray1 [ 1000 ]int
	var largeArray2 [ 5000 ]float64

	fmt.Printf ( "First large array first and last initial values: %t %t\n", largeArray[ 0 ], largeArray[ largeArraySize-1 ] )
	fmt.Printf ( "Second large array first and last initial values: %d %d\n", largeArray1[ 0 ], largeArray1[ 999 ] )
	fmt.Printf ( "Third large array first and last initial values: %.1f %.2f\n", largeArray2[ 0 ], largeArray2[ 4999 ] )

	// set largeArray first entry to true
	largeArray[ 0 ] = true
	// set largeArray last entry to false
	largeArray[ largeArraySize-1 ] = false
	fmt.Printf ( "First large array first and last values: %t %t\n", largeArray[ 0 ], largeArray[ largeArraySize-1 ] )

	// set largeArray1 first entry to 25
	largeArray1[ 0 ] = 25
	// set largeArray1 last entry to 55
	largeArray1[ 999 ] = 55
	fmt.Printf ( "Second large array first and last values: %d %d\n", largeArray1[ 0 ], largeArray1[ 999 ] )

	// set largeArray2 first entry to 27.5
	largeArray2[ 0 ] = 27.5
	// set largeArray2 last entry to 58.25
	largeArray2[ 4999 ] = 58.25
	fmt.Printf ( "Third large array first and last values: %.1f %.2f\n", largeArray2[ 0 ], largeArray2[ 4999 ] )

	// Character array (Go strings are immutable, but we can use byte arrays)
	var myString [ 100 ]byte
	myString[ 0 ] = 'H'
	myString[ 1 ] = 'e'
	myString[ 2 ] = 'l'
	myString[ 3 ] = 'l'
	myString[ 4 ] = 'o'
	myString[ 5 ] = ' '
	myString[ 6 ] = 'W'
	myString[ 7 ] = 'o'
	myString[ 8 ] = 'r'
	myString[ 9 ] = 'l'
	myString[ 10 ] = 'd'
	myString[ 11 ] = '.'
	myString[ 12 ] = 0 // null terminator
	// Convert byte array to string, stopping at null terminator
	nullIndex := 0
	for i, b := range myString {
		if b == 0 {
			nullIndex = i
			break
		}
	}
	fmt.Printf ( "%s\n", string( myString[ :nullIndex ] ) )

	myString1 := [ 100 ]byte{ 'H', 'e', 'l', 'l', 'o', ' ', 'W', 'o', 'r', 'l', 'd', '.', 0 }
	nullIndex1 := 0
	for i, b := range myString1 {
		if b == 0 {
			nullIndex1 = i
			break
		}
	}
	fmt.Printf ( "%s\n", string( myString1[ :nullIndex1 ] ) )

	// Note the \0 is not needed in Go strings
	myString2 := "Hello World."
	fmt.Printf ( "%s\n", myString2 )

	// 2D Array (Go uses true multidimensional arrays)
	var twoDArray [ 4 ][ 4 ]byte
	twoDArray[ 0 ][ 0 ] = '0'
	twoDArray[ 0 ][ 1 ] = '1'
	twoDArray[ 0 ][ 2 ] = '2'
	twoDArray[ 0 ][ 3 ] = '3'
	twoDArray[ 1 ][ 0 ] = '4'
	twoDArray[ 1 ][ 1 ] = '5'
	twoDArray[ 1 ][ 2 ] = '6'
	twoDArray[ 1 ][ 3 ] = '7'
	twoDArray[ 2 ][ 0 ] = '8'
	twoDArray[ 2 ][ 1 ] = '9'
	twoDArray[ 2 ][ 2 ] = 'A'
	twoDArray[ 2 ][ 3 ] = 'B'
	twoDArray[ 3 ][ 0 ] = 'C'
	twoDArray[ 3 ][ 1 ] = 'D'
	twoDArray[ 3 ][ 2 ] = 'E'
	twoDArray[ 3 ][ 3 ] = 'F'

	// Note: the actual implementation of this code will use some advanced
	// techniques that will not be described, only the results of the code observed.
	fmt.Printf ( "twoDArray memory location as flat data: " )
	flatPtr := ( *[ 16 ]byte )( unsafe.Pointer( &twoDArray[ 0 ][ 0 ] ) )
	for i := 0; i < 16; i++ {
		fmt.Printf ( "%c", flatPtr[ i ] )
	}
	fmt.Printf ( "\n" )
	
	// Note these are not defined as constant, but the capital naming
	// indicates the values should not change.
	const RED = 0
	const GREEN = 1
	const BLUE = 2
	const YELLOW = 3
	const CYAN = 4
	const MAGENTA = 5
	const WHITE = 6

	const RGB_RED = 0
	const RGB_GREEN = 1
	const RGB_BLUE = 2

	// Columns: Red Intensity, Green Intensity, Blue Intensity
	colorTable := [ 7 ][ 3 ]int{
		{ 255, 0, 0 },     // Red
		{ 0, 255, 0 },     // Green
		{ 0, 0, 255 },     // Blue
		{ 255, 255, 0 },   // Yellow = Red + Green
		{ 0, 255, 255 },   // Cyan   = Green + Blue
		{ 255, 0, 255 },   // Magenta = Red + Blue
		{ 255, 255, 255 }, // White = Red + Green + Blue
	}

	fmt.Printf ( "CYAN color values: %d %d %d\n", colorTable[ CYAN ][ RGB_RED ],
		colorTable[ CYAN ][ RGB_GREEN ], colorTable[ CYAN ][ RGB_BLUE ] )
}
