# Stubbed out functions
def userInput():
    return 3

def someFunctionResult():
    return True

def main():
    # Basic if statements with hard coded conditions
    print("Before Conditions")
    if True:
        print("Branch Executed")

    if False:
        print("Branch Not Executed")

    print("After Conditions")

    # If with hard coded assignment
    myVariable = None
    if myVariable := True:
        print("Branch Executed")

    # If with variable assignment (using function result)
    if myVariable := someFunctionResult():
        print("Branch Executed")

    # More proper conditional branch
    temperature = 64

    heaterOn = False
    temperatureCold = (temperature < 70)
    if temperatureCold == True:
        heaterOn = True

    print(f"Heater is on: {heaterOn}")

    # Alternate code to evaluate a conditional branch
    heaterOn = False
    if temperature < 70:
        heaterOn = True

    print(f"Heater is on: {heaterOn}")

    # Using a code block that executes if a condition is true.
    budget = 5000.00
    buffer = 500.00
    estimatedCost = 4750.00

    budgetOverrun = False

    if (estimatedCost + buffer) > budget:
        overrunAmount = (estimatedCost + buffer) - budget
        print(f"Overrun Amount: {overrunAmount:.2f}")
        buffer = 50
        estimatedCost = budget - buffer
        budgetOverrun = True

    print(f"Budget overrun occurred: {budgetOverrun}")

    # Variable scope in code blocks.
    # Many languages will fail trying to access innerVariable outside of the block.
    # In most languages I can even take the if True line out and just have the code block.
    # Python is fine with this.
    outerVariable = 10
    if True:
        innerVariable = 20

    print(f"Variables: {outerVariable} : {innerVariable}")

    # Variable scope: Shadowed variables
    # In python this isn't shadowing, outer or inner is the same variable
    variable = 10
    if True:
        variable = 20
        print(f"Variable inside: {variable}")

    print(f"Variable outside: {variable}")

    # Error of line being outside of if block, note the lack of begin/end block.
    # Different languages handle this differently.
    # In python all indented code is part of the block, not a problem.
    if False:
        print("Statement One")
        print("Statement Two")

    if False:
        print("Statement Three")
        print("Statement Four")

    # Python doesn't care about outer parenthesis in branches
    # Good in Python
    if ( True ):
        print("Good branch!")

    # Also good in Python
    if True:
        print("Also good branch!")

    # What python does care about is the : in branches
    #if True
    #    print( "Bad branch!" )

    # If disconnected from the block, note the semicolon.
    # This behavior does not really appear in python
    #if False: pass
    #    print("Hello")

    # Multiple separate if statements
    score = 85

    if score >= 90 and score <= 100:
        print("You got an A")
    if score >= 80 and score < 90:
        print("You got a B")
    if score >= 70 and score < 80:
        print("You got a C")
    if score >= 60 and score < 70:
        print("You got a D")
    if score < 60:
        print("You failed the test")

    # Else if example (elif in Python)
    if score >= 90:
        print("You got an A")
    elif score >= 80:
        print("You got a B")
    elif score >= 70:
        print("You got a C")
    elif score >= 60:
        print("You got a D")
    elif score < 60:
        print("You failed the test")

    # Forgetting the else on an elif
    if score >= 90:
        print("You got an A")
    if score >= 80:
        print("You got a B")
    elif score >= 70:
        print("You got a C")

    # Elif without if, this will not compile in most languages.
    # Note we have to put an else here to terminate the previous else if
    #else: pass
    #elif score >= 90:
    #    print("You got an A")

    # Adding an else to our if/elif
    if score >= 90:
        print("You got an A")
    elif score >= 80:
        print("You got a B")
    elif score >= 70:
        print("You got a C")
    elif score >= 60:
        print("You got a D")
    else:
        print("You failed the test")

    # Unreachable else, programming languages may not warn about this
    age = 125

    if (age > 0) or (age < 100):
        print("Valid age")
    else:
        print("Invalid age")

    # Else not following If or Elif, very uncompilable.
    #else:
    #    print("Hello from else!")
    #elif True:
    #    print("Hello from elif!")
    #if True:
    #    print("Hello from if!")

    # Example of a complex condition that could be made a nested if
    isLoggedIn = True
    role = "admin"
    method = "POST"
    isBanned = False
    resourceIsAvailable = True

    if (isLoggedIn == True) and (role == "admin") and (method == "POST") and (isBanned == False) and (resourceIsAvailable == True):
        print("Access granted")

    # Breaking the complex condition into a nested if
    if isLoggedIn == True:
        if role == "admin":
            if method == "POST":
                if isBanned == False:
                    if resourceIsAvailable == True:
                        print("Access granted")
                    else:
                        print("Resource Unavailable")
                else:
                    print("User is Banned")
            else:
                print("Wrong Method")
        else:
            print("Wrong User Level")
    else:
        print("Please Log In")

    # Dangling Else - How this is handled will differ in different languages
    # Python's indentation makes this less ambiguous
    userExists = True
    passwordValid = True

    if userExists == True:
        if passwordValid == True:
            print("Access granted")
        else:
            print("Retry user name and password")

    # Python can also connect the else to the other if
    if userExists == True:
        if passwordValid == True:
            print("Access granted")
    else:
        print("Retry user name")

    # Basic switch statement (using match-case in Python 3.10+)
    # For older Python versions, this would be if/elif chains
    switchVariable = 2

    match switchVariable:
        case 1:
            print("Variable is 1")
        case 2:
            print("Variable is 2")
        case _:
            print("Variable is unexpected value!")

    # Switch on user input, more practical example
    print("Main Menu:")
    print("1. Start Game")
    print("2. Load Game")
    print("3. Show Help")
    print("4. Exit")
    print("Enter your choice: ", end="")

    choice = userInput()

    match choice:
        case 1:
            print("Starting new game...")
        case 2:
            print("Loading saved game...")
        case 3:
            print("Help: Use the number keys to navigate the menu.")
        case 4:
            print("Exiting program. Goodbye!")
        case _:
            print("Invalid choice. Please select a valid option.")

    # Divide by zero defensive condition
    time = 0
    distance = 100
    speed = 0
    if time != 0:
        speed = distance / time

    # Handling both valid and invalid user inputs converted to booleans
    # This example in Python is less relevant as Python can handle converting
    # the strings true/false, but not 0/1
    print("Enter a number:")
    inputString = input()
    
    try:
        readInt = int(inputString)
        print("User entered a valid number")
    except ValueError:
        readInt = -1
        print("Invalid number entered.")

    # Method 1 of parsing an input string to a boolean
    print("Enter a boolean:")
    inputString = input()
    
    readValidBool = False

    if ( inputString.isdigit() or ( inputString.startswith( '-' ) and inputString[1:].isdigit() ) ):
        readInt = int( inputString )
        readValidBool = True
        if ( readInt == 0 ):
            readBool = False
        else:
            readBool = True
    else:
        if ( inputString.lower() == "false" ):
            readBool = False
            readValidBool = True
        elif ( inputString.lower() == "true" ):
            readBool = True
            readValidBool = True
        else:
            print( "Invalid boolean entered" )

    if ( readValidBool == True ):
        print( "Entered boolean is valid" )

    # Alternate method of parsing a string to boolean using guard clauses instead of nested conditions
    print( "Enter a boolean:" )
    inputString = input().strip()
    
    readValidBool = False

    if ( inputString.isdigit() or ( inputString.startswith( '-' ) and inputString[1:].isdigit() ) ):
        readInt = int( inputString )
        readValidBool = True
        if ( readInt == 0 ):
            readBool = False
        else:
            readBool = True

    if ( readValidBool == False ):
        if ( inputString.lower() == "false" ):
            readBool = False
            readValidBool = True
        elif ( inputString.lower() == "true" ):
            readBool = True
            readValidBool = True

    if ( readValidBool == True ):
        print( "Valid boolean entered" )
    else:
        print( "Invalid boolean entered" )

    # Compare two strings, only up to the length of the shortest string
    falseString = "false"
    
    inputMatchesFalse = inputString.startswith(falseString)

    # Make sure both strings have the appropriate length
    falseString = "false"
    subStringLength = len(falseString)

    if subStringLength > len(inputString):
        subStringLength = len(inputString)

    inputMatchesFalse = (inputString[:subStringLength] == falseString)

    print(f"False Entered {inputMatchesFalse}")

    # Float comparison with both positive and negative differences
    firstFloat = 0.1
    secondFloat = 0.2
    sum = firstFloat + secondFloat
    thirdFloat = 0.3

    tolerance = 0.000001

    difference = sum - thirdFloat

    if (difference > -tolerance) and (difference < tolerance):
        print("First float plus second float is equal to third float.")
    else:
        print("First float plus second float is NOT equal to third float.")

    # Float comparison with condition ensuring positive difference
    firstFloat = 0.1
    secondFloat = 0.2
    sum = firstFloat + secondFloat
    thirdFloat = 0.3

    tolerance = 0.000001

    difference = sum - thirdFloat
    if difference < 0:
        difference = -difference

    if difference < tolerance:
        print("First float plus second float is equal to third float.")
    else:
        print("First float plus second float is NOT equal to third float.")

    # Float comparison using abs to ensure positive difference
    firstFloat = 0.1
    secondFloat = 0.2
    sum = firstFloat + secondFloat
    thirdFloat = 0.3

    tolerance = 0.000001

    difference = abs(sum - thirdFloat)

    if difference < tolerance:
        print("First float plus second float is equal to third float.")
    else:
        print("First float plus second float is NOT equal to third float.")

if __name__ == "__main__":
    main()
