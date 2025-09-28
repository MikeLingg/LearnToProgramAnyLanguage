package main

import (
	"bufio"
	"fmt"
	"math"
	"os"
	"strconv"
	"strings"
)

// Stubbed out functions
func userInput() int {
	return 3
}

func someFunctionResult() bool {
	return true
}

func main() {
	// Basic if statements with hard coded conditions
	fmt.Printf("Before Conditions\n")
	if true {
		fmt.Printf("Branch Executed\n")
	}

	if false {
		fmt.Printf("Branch Not Executed\n")
	}

	fmt.Printf("After Conditions\n")

	// If with hard coded assignment
	// Go does not allow these, so we are skipping the example

	// More proper conditional branch
	temperature := 64

	heaterOn := false
	temperatureCold := (temperature < 70)
	if temperatureCold == true {
		heaterOn = true
	}

	fmt.Printf("Heater is on: %t\n", heaterOn)

	// Alternate code to evaluate a conditional branch
	heaterOn = false
	if temperature < 70 {
		heaterOn = true
	}

	fmt.Printf("Heater is on: %t\n", heaterOn)

	// Short Circuit Example
	fmt.Println ( "Short Circuit Example" )
    gamesPlayed := 0
    totalScore := 150
    
    if gamesPlayed > 0 && totalScore / gamesPlayed > 10 {
        fmt.Println ( "You are averaging more than 10 points per game!" )
    }
	
	// Using a code block that executes if a condition is true.
	budget := 5000.00
	buffer := 500.00
	estimatedCost := 4750.00

	budgetOverrun := false
	var overrunAmount float64

	if (estimatedCost + buffer) > budget {
		overrunAmount = (estimatedCost + buffer) - budget
		fmt.Printf("Overrun Amount: %.2f\n", overrunAmount)
		buffer = 50
		estimatedCost = budget - buffer
		budgetOverrun = true
	}

	fmt.Printf("Budget overrun occurred: %t\n", budgetOverrun)

	// Variable scope in code blocks.
	// Many languages will fail trying to access innerVariable outside of the block.
	// In most languages I can even take the if True line out and just have the code block.
	/*
		outerVariable := 10
		if ( true ) {
			innerVariable := 20
		}

		fmt.Printf( "Variables: %d : %d\n", outerVariable, innerVariable )
	*/

	// Variable scope: Shadowed variables
	variable := 10
	if true {
		variable := 20
		fmt.Printf("Variable inside: %d\n", variable)
	}

	fmt.Printf("Variable outside: %d\n", variable)

	// Error of line being outside of if block, note the lack of begin/end block.
	// Different languages handle this differently.
	// Go compiler prevents this, so less of a concern.
	//if ( false )
	//	fmt.Printf( "Statement One\n" )
	//	fmt.Printf( "Statement Two\n" ) // This will always execute in Go

	if false {
		fmt.Printf("Statement Three\n")
		fmt.Printf("Statement Four\n")
	}

	// Good in Go
	if true {
		fmt.Printf("Good branch!\n")
	}

	// Bad in Go
	/*
		if true
			fmt.Printf( "Bad branch!\n" )
	*/

	// If disconnected from the block, note the semicolon.
	// This behavior is going to vary from language to language
	// The go compiler prevents this.
	//if ( false ); {
	//	fmt.Printf( "Hello\n" ) // This will always execute
	//}

	// Multiple separate if statements with overlapping conditions
	score := 85

	if score >= 90 && score <= 100 {
		fmt.Printf("You got an A\n")
	}
	if score >= 80 && score < 90 {
		fmt.Printf("You got a B\n")
	}
	if score >= 70 && score < 80 {
		fmt.Printf("You got a C\n")
	}
	if score >= 60 && score < 70 {
		fmt.Printf("You got a D\n")
	}
	if score < 60 {
		fmt.Printf("You failed the test\n")
	}

	// Else if example
	if score >= 90 {
		fmt.Printf("You got an A\n")
	} else if score >= 80 {
		fmt.Printf("You got a B\n")
	} else if score >= 70 {
		fmt.Printf("You got a C\n")
	} else if score >= 60 {
		fmt.Printf("You got a D\n")
	} else if score < 60 {
		fmt.Printf("You failed the test\n")
	}

	// Forgetting the else on an else if
	if score >= 90 {
		fmt.Printf("You got an A\n")
	}
	if score >= 80 {
		fmt.Printf("You got a B\n")
	} else if score >= 70 {
		fmt.Printf("You got a C\n")
	}

	// Else if without if, this will not compile in most languages.
	/*
		} else if ( score >= 90 ) {
			fmt.Printf( "You got an A\n" )
	*/

	// Adding an else to our if/else if
	if score >= 90 {
		fmt.Printf("You got an A\n")
	} else if score >= 80 {
		fmt.Printf("You got a B\n")
	} else if score >= 70 {
		fmt.Printf("You got a C\n")
	} else if score >= 60 {
		fmt.Printf("You got a D\n")
	} else {
		fmt.Printf("You failed the test\n")
	}

	// Unreachable else, programming languages may not warn about this
	age := 125

	if (age > 0) || (age < 100) {
		fmt.Printf("Valid age\n")
	} else {
		fmt.Printf("Invalid age\n")
	}

	// Else not following If or Else If, very uncompilable.
	/*
		} else {
			fmt.Printf( "Hello from else!" )
		} else if ( true ) {
			fmt.Printf( "Hello from else if!" )
		if ( true ) {
			fmt.Printf( "Hello from if!" )
	*/

	// Example of a complex condition that could be made a nested if
	isLoggedIn := true
	role := "admin"
	method := "POST"
	isBanned := false
	resourceIsAvailable := true

	if (isLoggedIn == true) && (role == "admin") && (method == "POST") && (isBanned == false) && (resourceIsAvailable == true) {
		fmt.Printf("Access granted\n")
	}

	// Breaking the complex condition into a nested if
	if isLoggedIn == true {
		if role == "admin" {
			if method == "POST" {
				if isBanned == false {
					if resourceIsAvailable == true {
						fmt.Printf("Access granted\n")
					} else {
						fmt.Printf("Resource Unavailable\n")
					}
				} else {
					fmt.Printf("User is Banned\n")
				}
			} else {
				fmt.Printf("Wrong Method\n")
			}
		} else {
			fmt.Printf("Wrong User Level\n")
		}
	} else {
		fmt.Printf("Please Log In\n")
	}

	// Dangling Else - How this is handled will differ in different languages
	// The go compiler will not allow this confusing structure
	userExists := true
	passwordValid := true

	//if ( userExists == true )
	//	if ( passwordValid == true )
	//		fmt.Printf( "Access granted\n" )
	//else
	//	fmt.Printf( "Retry user name and password\n" )

	// No dangling else with blocks explicitly defined
	if userExists == true {
		if passwordValid == true {
			fmt.Printf("Access granted\n")
		} else {
			fmt.Printf("Retry password\n")
		}
	}

	// Basic switch statement
	switchVariable := 2

	switch switchVariable {
	case 1:
		fmt.Printf("Variable is 1\n")
	case 2:
		fmt.Printf("Variable is 2\n")
	default:
		fmt.Printf("Variable is unexpected value!\n")
	}

	// Switch on user input
	fmt.Printf("Main Menu:\n")
	fmt.Printf("1. Start Game\n")
	fmt.Printf("2. Load Game\n")
	fmt.Printf("3. Show Help\n")
	fmt.Printf("4. Exit\n")
	fmt.Printf("Enter your choice: ")

	choice := userInput()

	switch choice {
	case 1:
		fmt.Printf("Starting new game...\n")
	case 2:
		fmt.Printf("Loading saved game...\n")
	case 3:
		fmt.Printf("Help: Use the number keys to navigate the menu.\n")
	case 4:
		fmt.Printf("Exiting program. Goodbye!\n")
	default:
		fmt.Printf("Invalid choice. Please select a valid option.\n")
	}

	// Divide by zero defensive condition
	time := 0
	distance := 100
	speed := 0
	if time != 0 {
		speed = distance / time
	}
	fmt.Printf("Speed: %d\n", speed)

	// Handling both valid and invalid user inputs converted to booleans
	fmt.Printf("Enter a number:\n")
	scanner := bufio.NewScanner(os.Stdin)
	scanner.Scan()
	inputString := scanner.Text()

	readInt, err := strconv.Atoi(inputString)
	if err == nil {
		fmt.Printf("User entered a valid number\n")
	} else {
		readInt = -1
		fmt.Printf("Invalid number entered.\n")
	}

	// Method 1 of parsing an input string to a boolean
	fmt.Printf("Enter a boolean:\n")
	scanner.Scan()
	inputString = scanner.Text()

	readValidBool := false
	var readBool bool

	readInt, err = strconv.Atoi(inputString)
	if err == nil {
		readValidBool = true
		if readInt == 0 {
			readBool = false
		} else {
			readBool = true
		}
	} else {
		if strings.ToLower(inputString) == "false" {
			readBool = false
			readValidBool = true
		} else if strings.ToLower(inputString) == "true" {
			readBool = true
			readValidBool = true
		} else {
			fmt.Printf("Invalid boolean entered\n")
		}
	}

	fmt.Printf("Read boolean: %d\n", readBool)
	if readValidBool == true {
		fmt.Printf("Entered boolean is valid\n")
	}

	// Alternate method of parsing a string to boolean using guard clauses instead of nested conditions
	fmt.Printf("Enter a boolean:\n")
	scanner.Scan()
	inputString = strings.TrimSpace(scanner.Text())

	readValidBool = false

	readInt, err = strconv.Atoi(inputString)
	if err == nil {
		readValidBool = true
		if readInt == 0 {
			readBool = false
		} else {
			readBool = true
		}
	}

	if readValidBool == false {
		if strings.ToLower(inputString) == "false" {
			readBool = false
			readValidBool = true
		} else if strings.ToLower(inputString) == "true" {
			readBool = true
			readValidBool = true
		}
	}

	if readValidBool == true {
		fmt.Printf("Valid boolean entered\n")
	} else {
		fmt.Printf("Invalid boolean entered\n")
	}

	// Compare two strings, only up to the length of the shortest string
	falseString := "false"

	inputMatchesFalse := strings.HasPrefix(inputString, falseString)

	// Make sure both strings have the appropriate length
	falseString = "false"
	subStringLength := len(falseString)

	if subStringLength > len(inputString) {
		subStringLength = len(inputString)
	}

	inputMatchesFalse = (inputString[:subStringLength] == falseString)

	fmt.Printf("False Entered %t\n", inputMatchesFalse)

	// Float comparison with both positive and negative differences
	firstFloat := 0.1
	secondFloat := 0.2
	sum := firstFloat + secondFloat
	thirdFloat := 0.3

	tolerance := 0.000001

	difference := sum - thirdFloat

	if (difference > -tolerance) && (difference < tolerance) {
		fmt.Printf("First float plus second float is equal to third float.\n")
	} else {
		fmt.Printf("First float plus second float is NOT equal to third float.\n")
	}

	// Float comparison with condition ensuring positive difference
	firstFloat = 0.1
	secondFloat = 0.2
	sum = firstFloat + secondFloat
	thirdFloat = 0.3

	tolerance = 0.000001

	difference = sum - thirdFloat
	if difference < 0 {
		difference = -difference
	}

	if difference < tolerance {
		fmt.Printf("First float plus second float is equal to third float.\n")
	} else {
		fmt.Printf("First float plus second float is NOT equal to third float.\n")
	}

	// Float comparison using math.Abs to ensure positive difference
	firstFloat = 0.1
	secondFloat = 0.2
	sum = firstFloat + secondFloat
	thirdFloat = 0.3

	tolerance = 0.000001

	difference = math.Abs(sum - thirdFloat)

	if difference < tolerance {
		fmt.Printf("First float plus second float is equal to third float.\n")
	} else {
		fmt.Printf("First float plus second float is NOT equal to third float.\n")
	}
}
