package main

import (
	"bufio"
	"fmt"
	"math"
	"os"
	"strconv"
)

func main () {
	// Note: For this program to function as expected, the user will have to correctly enter the requested values.

	// Boolean strings of true/false cannot be converted to a bool variable without conditions,
	// so we will discuss how that works in the branches video coming soon.
	// Some languages will not even allow for reading values of 0 or 1 from the terminal as
	// booleans so we will identify which languages this fails with, and revisit how to make 
	// this work in the branches video.

	scanner := bufio.NewScanner ( os.Stdin )

	// Note: Go's ParseBool only works with specific strings like "true", "false", "1", "0"
	// Other values like "11" or "-1" will cause errors
	fmt.Print ( "Type 0 and press enter.\n" )
	scanner.Scan ()
	userInput := scanner.Text ()
	enteredBoolean, err := strconv.ParseBool ( userInput )
	fmt.Printf ( "The user entered the boolean %t (error: %v)\n", enteredBoolean, err )

	fmt.Print ( "Type 1 and press enter.\n" )
	scanner.Scan ()
	userInput = scanner.Text ()
	enteredBoolean, err = strconv.ParseBool ( userInput )
	fmt.Printf ( "The user entered the boolean %t (error: %v)\n", enteredBoolean, err )

	fmt.Print ( "Type True and press enter.\n" )
	scanner.Scan ()
	userInput = scanner.Text ()
	enteredBoolean, err = strconv.ParseBool ( userInput )
	fmt.Printf ( "The user entered the boolean %t (error: %v)\n", enteredBoolean, err )

	fmt.Print ( "Type False and press enter.\n" )
	scanner.Scan ()
	userInput = scanner.Text ()
	enteredBoolean, err = strconv.ParseBool ( userInput )
	fmt.Printf ( "The user entered the boolean %t (error: %v)\n", enteredBoolean, err )

	fmt.Print ( "Type FALSE and press enter.\n" )
	scanner.Scan ()
	userInput = scanner.Text ()
	enteredBoolean, err = strconv.ParseBool ( userInput )
	fmt.Printf ( "The user entered the boolean %t (error: %v)\n", enteredBoolean, err )

	fmt.Print ( "Type 11 and press enter.\n" )
	scanner.Scan ()
	userInput = scanner.Text ()
	enteredBoolean, err = strconv.ParseBool ( userInput )
	fmt.Printf ( "The user entered the boolean %t (error: %v)\n", enteredBoolean, err )

	fmt.Print ( "Type -1 and press enter.\n" )
	scanner.Scan ()
	userInput = scanner.Text ()
	enteredBoolean, err = strconv.ParseBool ( userInput )
	fmt.Printf ( "The user entered the boolean %t (error: %v)\n", enteredBoolean, err )

	fmt.Print ( "Type 55 and press enter.\n" )
	scanner.Scan ()
	userInput = scanner.Text ()
	enteredInteger, err := strconv.Atoi ( userInput )
	fmt.Printf ( "The user entered the integer %d (error: %v)\n", enteredInteger, err )

	fmt.Print ( "Type 55.5 and press enter.\n" )
	scanner.Scan ()
	userInput = scanner.Text ()
	enteredFloat, err := strconv.ParseFloat ( userInput, 64 )
	fmt.Printf ( "The user entered the float %f (error: %v)\n", enteredFloat, err )

	fmt.Print ( "Type Hello World! and press enter.\n" )
	scanner.Scan ()
	userInput = scanner.Text ()
	fmt.Printf ( "The user entered the string %s\n", userInput )

	fmt.Print ( "Type 123abc and press enter.\n" )
	scanner.Scan ()
	userInput = scanner.Text ()
	enteredInteger, err = strconv.Atoi ( userInput )
	fmt.Printf ( "The user entered the integer %d (error: %v)\n", enteredInteger, err )

	fmt.Print ( "Type 123.45 and press enter.\n" )
	scanner.Scan ()
	userInput = scanner.Text ()
	enteredInteger, err = strconv.Atoi ( userInput )
	fmt.Printf ( "The user entered the integer %d (error: %v)\n", enteredInteger, err )

	fmt.Print ( "Type abc123 and press enter.\n" )
	scanner.Scan ()
	userInput = scanner.Text ()
	enteredInteger, err = strconv.Atoi ( userInput )
	fmt.Printf ( "The user entered the integer %d (error: %v)\n", enteredInteger, err )

	fmt.Print ( "Type  567 and press enter.\n" )
	scanner.Scan ()
	userInput = scanner.Text ()
	enteredInteger, err = strconv.Atoi ( userInput )
	fmt.Printf ( "The user entered the integer %d (error: %v)\n", enteredInteger, err )

	fmt.Print ( "Type +567 and press enter.\n" )
	scanner.Scan ()
	userInput = scanner.Text ()
	enteredInteger, err = strconv.Atoi ( userInput )
	fmt.Printf ( "The user entered the integer %d (error: %v)\n", enteredInteger, err )

	fmt.Print ( "Type -567 and press enter.\n" )
	scanner.Scan ()
	userInput = scanner.Text ()
	enteredInteger, err = strconv.Atoi ( userInput )
	fmt.Printf ( "The user entered the integer %d (error: %v)\n", enteredInteger, err )

	// Go has no built-in abs for integers, must write custom function or use math.Abs for floats
	fmt.Printf ( "Abs of -5 is %g\n", math.Abs ( -5 ) )
	fmt.Printf ( "Abs of -5.5 is %g\n", math.Abs ( -5.5 ) )
	fmt.Printf ( "Abs of a is %g\n", math.Abs ( float64 ( 'a' ) ) )

	fmt.Printf ( "Pow of 2^5 is %g\n", math.Pow ( 2, 5 ) )
	fmt.Printf ( "Pow of 2.2^5.2 is %g\n", math.Pow ( 2.2, 5.2 ) )
	fmt.Printf ( "Pow of a^b is %g\n", math.Pow ( float64 ( 'a' ), float64 ( 'b' ) ) )

	// Note trig functions are almost always in radians, not degrees
	fmt.Printf ( "Sin of 90 is %g\n", math.Sin ( 90 ) )
	fmt.Printf ( "Sin of pi/2 is %g\n", math.Sin ( 3.14159265358979323846/2 ) )

	fmt.Printf ( "Cos of 180 is %g\n", math.Cos ( 180 ) )
	fmt.Printf ( "Cos of pi is %g\n", math.Cos ( 3.14159265358979323846 ) )

	// Rounding type functions are very useful for explicit float to int conversions
	fmt.Printf ( "Floor of 5.5 is %g\n", math.Floor ( 5.5 ) )
	fmt.Printf ( "Floor of -5.5 is %g\n", math.Floor ( -5.5 ) )

	fmt.Printf ( "Ceil of 5.5 is %g\n", math.Ceil ( 5.5 ) )
	fmt.Printf ( "Ceil of -5.5 is %g\n", math.Ceil ( -5.5 ) )

	fmt.Printf ( "Round of 5.5 is %g\n", math.Round ( 5.5 ) )
	fmt.Printf ( "Round of -5.5 is %g\n", math.Round ( -5.5 ) )

	fmt.Printf ( "Trunc of 5.5 is %g\n", math.Trunc ( 5.5 ) )
	fmt.Printf ( "Trunc of -5.5 is %g\n", math.Trunc ( -5.5 ) )

	// This will NOT crash in Go (strconv.Atoi returns 0 and error for invalid input)
	fmt.Print ( "Type Hello World! and press enter.\n" )
	scanner.Scan ()
	userInput = scanner.Text ()
	enteredInteger, err = strconv.Atoi ( userInput )
	fmt.Printf ( "The user entered the integer %d (error: %v)\n", enteredInteger, err )

	// This will NOT crash in Go (strconv.Atoi returns 0 and error for invalid input)
	fmt.Print ( "Type abc123 and press enter.\n" )
	scanner.Scan ()
	userInput = scanner.Text ()
	enteredInteger, err = strconv.Atoi ( userInput )
	fmt.Printf ( "The user entered the integer %d (error: %v)\n", enteredInteger, err )
}
