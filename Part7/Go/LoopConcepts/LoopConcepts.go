package main

import (
	"bufio"
	"fmt"
	"math"
	"os"
	"strconv"
	"strings"
)

func main() {
	// Basic loop.
	fmt.Println ( "\nSimple 100 iteration loop." )
	for loopIndex := 0; loopIndex < 100; loopIndex++ {
		fmt.Println ( loopIndex )
	}
	fmt.Println ( "After Loop" )

	// Palindrome checker
	// It is a palindrome until we prove it is not.
	fmt.Println ( "\nPalindrome checker." )
	possiblePalindrome := "step on no pets"
	isPalindrome := true

	halfLength := len ( possiblePalindrome ) / 2

	for palindromeIndex := 0; palindromeIndex < halfLength; palindromeIndex++ {
		if possiblePalindrome[palindromeIndex] != possiblePalindrome[len ( possiblePalindrome ) - 1 - palindromeIndex] {
			isPalindrome = false
			break
		}
	}

	if isPalindrome == true {
		fmt.Printf ( "%s is a palindrome.\n", possiblePalindrome )
	} else {
		fmt.Printf ( "%s is NOT a palindrome.\n", possiblePalindrome )
	}

	// Go can have two loop variables using comma separator
	fmt.Println ( "\nGo style loop looping through two variables." )
	for palindromeFrontIndex, palindromeBackIndex := 0, len ( possiblePalindrome ) - 1; palindromeFrontIndex < halfLength; palindromeFrontIndex, palindromeBackIndex = palindromeFrontIndex + 1, palindromeBackIndex - 1 {
		if possiblePalindrome[palindromeFrontIndex] != possiblePalindrome[palindromeBackIndex] {
			isPalindrome = false
			break
		}
	}

	// Two loops continuing a loop variable
	fmt.Println ( "\nLoops continuing a loop variable." )
	var loopIndex int
	for loopIndex = 5; loopIndex < 15; loopIndex++ {
		fmt.Printf ( "Loop Index First Loop: %d\n", loopIndex )
	}
	for ; loopIndex < 55; loopIndex++ {
		fmt.Printf ( "Loop Index Second Loop: %d\n", loopIndex )
	}

	// Go cannot perform a for loop with floating point loop variables directly
	// Go for loops only work with integer conditions

	// Go can loop through arrays of numbers, but does not have range loops like Python.

	// Looping over a range of array indices.
	// Good, range provided from array length
	fmt.Println ( "Looping over a range of indices in an array" )
	myList := []int{1, 2, 3, 4}
	for loopIndex := 0; loopIndex < len ( myList ); loopIndex++ {
		fmt.Printf ( "Loop Index: %d\n", loopIndex )
	}
	fmt.Println ( "After Loop" )

	// Bad, myList is not a number - This will not compile
	fmt.Println ( "\nAttempting bad range usage - myList is not a number" )
	for loopIndex := 0; loopIndex < myList; loopIndex++ {
		fmt.Printf ( "Loop Index: %d\n", loopIndex )
	}
	fmt.Println ( "After Loop" )

	// Initialize all entries in a large slice to 0
	fmt.Println ( "\nInitialize values in a slice" )
	arraySamples := 10000
	intArray := make ( []int, arraySamples )

	// This is kind of pointless in Go as the above code creates a slice
	// of defined size with all values set to 0. The above statement is much faster 
	// than this loop.
	// However non-zero default values need a loop.
	for arrayIndex := 0; arrayIndex < arraySamples; arrayIndex++ {
		intArray[arrayIndex] = 0
	}

	// Initialize all entries in a large slice to index * 2 + 1
	for arrayIndex := 0; arrayIndex < arraySamples; arrayIndex++ {
		intArray[arrayIndex] = arrayIndex * 2 + 1
	}

	// Example with string validation.
	// Loop through a string and ensure it contains a valid number.
	// Scientific notation is not considered, a single period is valid, no non numeric characters are valid
	fmt.Println ( "\nVerify string contains valid number." )
	myString := "123.45"
	periodCount := 0
	validNumber := true
	for arrayIndex := 0; arrayIndex < len ( myString ); arrayIndex++ {
		char := myString[arrayIndex]
		if char == '.' {
			periodCount = periodCount + 1
			if periodCount > 1 {
				validNumber = false
				break
			}
		} else if char < '0' || char > '9' {
			validNumber = false
			break
		}
	}

	fmt.Printf ( "String is valid number: %t\n", validNumber )

	// Basic condition loop
	fmt.Println ( "\nSimple conditional loop" )
	intCount := 1000000
	for true {
		// Just print the hello once so the terminal isn't overwhelmed.
		if intCount == 1000000 {
			fmt.Println ( "Hello" )
		}

		// Break added so this isn't an infinite loop
		intCount = intCount - 1
		if intCount <= 0 {
			break
		}
	}

	// For loop with only condition (acts like while)
	fmt.Println ( "\nExample for loop acting like while loop" )
	loopIndex = 0
	for loopIndex < 100 {
		fmt.Println ( loopIndex )
		loopIndex = loopIndex + 1
	}
	fmt.Println ( "After loop" )

	// More appropriate use of a condition loop, when we don't know how many loops will be performed.
	fmt.Println ( "\nBetter condition loop with length of loop unknown at start." )
	dataArray := []int{1, 2, 3, 4, 5, 6, 5, 7, 5, 8, 5, 9, 5, 10, 11, 12, 13, 14, 15}
	dataArraySize := len ( dataArray )
	loopIndex = 0
	foundCount := 0
	fiveElementsFound := false
	for ( loopIndex < dataArraySize ) && ( fiveElementsFound == false ) {
		if dataArray[loopIndex] < 10 {
			foundCount = foundCount + 1
			if foundCount >= 5 {
				fiveElementsFound = true
			}
		}
		loopIndex = loopIndex + 1
	}

	// Using a loop to prompt user input until valid
	fmt.Println ( "\nConditional loop based on user input." )
	scanner := bufio.NewScanner ( os.Stdin )
	exitMenu := false
	for exitMenu == false {
		fmt.Println ( "Main Menu:" )
		fmt.Println ( "1. Start Game" )
		fmt.Println ( "2. Load Game" )
		fmt.Println ( "3. Show Help" )
		fmt.Println ( "4. Exit" )
		fmt.Print ( "Enter your choice: " )
		
		scanner.Scan()
		input := scanner.Text()
		choice, err := strconv.Atoi ( strings.TrimSpace ( input ) )
		if err != nil {
			choice = 0
		}

		// Go has switch statements
		switch choice {
		case 1:
			fmt.Println ( "Starting new game..." )
		case 2:
			fmt.Println ( "Loading saved game..." )
		case 3:
			fmt.Println ( "Help: Use the number keys to navigate the menu." )
		case 4:
			fmt.Println ( "Exiting program. Goodbye!" )
			exitMenu = true
		default:
			fmt.Println ( "Invalid choice. Please select a valid option." )
		}
	}

	// Go has no do-while loop, so we can only show with condition-only for loop.
	fmt.Println ( "For loop equivalent example: Hint, entering 42 will exit this loop" )
	number := 0
	var err error
	for number != 42 {
		fmt.Print ( "Guess a number: " )
		scanner.Scan()
		input := scanner.Text()
		number, err = strconv.Atoi ( strings.TrimSpace ( input ) )
		if err != nil {
			number = 0
		}
	}

	// Nested loop for simple 2d array print
	fmt.Println ( "\nNested loop, 2d, example." )
	rowCount := 3
	columnCount := 3
	twoDList := [][]int{{1, 2, 3}, {4, 5, 6}, {7, 8, 9}}
	
	for rowIndex := 0; rowIndex < rowCount; rowIndex++ {
		for columnIndex := 0; columnIndex < columnCount; columnIndex++ {
			fmt.Printf ( "%d ", twoDList[rowIndex][columnIndex] )
		}
		fmt.Println ()
	}

	// Nested loop to print character permutations
	fmt.Println ( "\nNested permutations loop." )
	letters := []rune{'a', 'b', 'c', 'd', 'e'}
	lettersSize := len ( letters )
	
	for firstCharIndex := 0; firstCharIndex < lettersSize; firstCharIndex++ {
		for secondCharIndex := 0; secondCharIndex < lettersSize; secondCharIndex++ {
			fmt.Printf ( "%c%c\n", letters[firstCharIndex], letters[secondCharIndex] )
		}
	}

	// Nested loop to order a list of numbers
	fmt.Println ( "\nNested loop for sorting numbers." )
	myList2 := []int{6, 8, 9, 7, 4, 5, 0, 3, 1, 2}
	myList2Size := len ( myList2 )
	for listIndex := 0; listIndex < myList2Size; listIndex++ {
		for swapIndex := 0; swapIndex < myList2Size - listIndex - 1; swapIndex++ {
			if myList2[swapIndex] > myList2[swapIndex + 1] {
				swapValue := myList2[swapIndex]
				myList2[swapIndex] = myList2[swapIndex + 1]
				myList2[swapIndex + 1] = swapValue
			}
		}
	}

	// Break within nested loop
	fmt.Println ( "\nUsing a break in a nested loop." )
	twoDList2 := [][]int{{1, 2}, {3, 4}}
	rowCount = 2
	columnCount = 2
	
	for rowIndex := 0; rowIndex < rowCount; rowIndex++ {
		for columnIndex := 0; columnIndex < columnCount; columnIndex++ {
			fmt.Println ( twoDList2[rowIndex][columnIndex] )
			break
		}
	}

	// Off by 1 error - The first loop will exceed the array bounds
	// Go will panic at runtime with bounds checking
	fmt.Println ( "\nOff by 1 loop error for array access." )
	myArray := []int{1, 2, 3, 4}
	arraySize := len ( myArray )
	fmt.Println ( "Loop through array off by 1." )
	for loopIndex := 0; loopIndex <= arraySize; loopIndex++ {
		arrayValue := myArray[loopIndex]
		fmt.Printf ( "%d %d\n", loopIndex, arrayValue )
	}

	fmt.Println ( "Loop through array correct." )
	for loopIndex := 0; loopIndex < arraySize; loopIndex++ {
		fmt.Println ( myArray[loopIndex] )
	}

	// Off by 1 when performing a partial list sort - Will exceed list bounds
	fmt.Println ( "\nSort list with off by 1 error." )
	myList3 := []int{4, 3, 2, 1}
	myList3Size := len ( myList3 )
	for loopIndex := 0; loopIndex < myList3Size; loopIndex++ {
		if myList3[loopIndex] > myList3[loopIndex + 1] {
			swapValue := myList3[loopIndex]
			myList3[loopIndex] = myList3[loopIndex + 1]
			myList3[loopIndex + 1] = swapValue
		}
	}

	// Off by 1 when performing a partial list sort - Will NOT exceed list bounds
	fmt.Println ( "\nSort list without off by 1 error." )
	for loopIndex := 0; loopIndex < myList3Size - 1; loopIndex++ {
		if myList3[loopIndex] > myList3[loopIndex + 1] {
			swapValue := myList3[loopIndex]
			myList3[loopIndex] = myList3[loopIndex + 1]
			myList3[loopIndex + 1] = swapValue
		}
	}

	// Go does not allow an empty loop block, will not provide example.

	// Redeclaring loop variable
	fmt.Println ( "\nRedeclared loop variable." )
	for loopIndex := 0; loopIndex < 10; loopIndex++ {
		for loopIndex := 0; loopIndex < 10; loopIndex++ {
			fmt.Printf ( "Loop Index: %d\n", loopIndex )
		}
	}

	// Loop condition untrue on entry
	fmt.Println ( "\nLoop condition untrue on entry." )
	currentTime := 5
	nextFrame := 5
	for currentTime < nextFrame {
		fmt.Println ( "Processing background tasks" )
		currentTime = currentTime + 1
	}

	// Go slice - removing elements while iterating will cause index issues
	fmt.Println ( "\nSlice removing elements while iterating - will panic" )
	mySlice := []int{5, 2, 8, 2, 2, 6, 3, 5}
	originalLen := len ( mySlice )
	for index := 0; index < originalLen; index++ {
		if mySlice[index] == 2 {
			// Remove element by slicing
			mySlice = append ( mySlice[:index], mySlice[index+1:]... )
			fmt.Printf ( "mySlice at index %d: %v\n", index, mySlice )
		}
	}

	// Go slice - reverse for removal (correct approach)
	fmt.Println ( "\nSlice removing items while iterating in reverse" )
	mySlice2 := []int{5, 2, 8, 2, 2, 6, 3, 5}
	for index := len ( mySlice2 ) - 1; index >= 0; index-- {
		if mySlice2[index] == 2 {
			mySlice2 = append ( mySlice2[:index], mySlice2[index+1:]... )
			fmt.Printf ( "mySlice at index %d: %v\n", index, mySlice2 )
		}
	}

	// Go condition loop for slice removals
	fmt.Println ( "\nGo condition loop to remove slice items." )
	mySlice3 := []int{5, 2, 8, 2, 2, 6, 3, 5}
	index := 0
	for index < len ( mySlice3 ) {
		if mySlice3[index] == 2 {
			mySlice3 = append ( mySlice3[:index], mySlice3[index+1:]... )
			fmt.Printf ( "mySlice at index %d: %v\n", index, mySlice3 )
		} else {
			index = index + 1
		}
	}

	// Go integers can overflow but will wrap around
	fmt.Println ( "Loop to very large number." )
	largeNumber := math.MaxInt64 - 1000
	count := 0
	for loopIndex := largeNumber; loopIndex <= math.MaxInt64; loopIndex++ {
		count = count + 1
		if loopIndex < 0 {
			fmt.Println ( "Loop index went negative, exiting!" );
			break
		}
	}
}
