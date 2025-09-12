def main():
    # Basic loop.
    print ( "\nSimple 100 iteration loop." )
    for loopIndex in range ( 100 ):
        print ( loopIndex )
    print ( "After Loop" )

    # Palindrome checker
    # It is a palindrome until we prove it is not.
    print ( "\nPalindrome checker." )
    possiblePalindrome = "step on no pets"
    isPalindrome = True

    halfLength = len ( possiblePalindrome ) // 2

    for palindromeIndex in range ( halfLength ):
        if possiblePalindrome[palindromeIndex] != possiblePalindrome[len ( possiblePalindrome ) - 1 - palindromeIndex]:
            isPalindrome = False
            break

    if isPalindrome == True:
        print ( f"{possiblePalindrome} is a palindrome." )
    else:
        print ( f"{possiblePalindrome} is NOT a palindrome." )

    # Python cannot have two loop variables.  Must compute other variables 
    # from the primary loop variable, or do something like a while loop.

    # Two loops continuing a loop variable
    # Python can do this, but a little differently
    print ( "\nTwo loops continuing a loop variable" )
    for loopIndex in range ( 15 ):
        print ( "First Loop:" + str ( loopIndex ) )
    for loopIndex in range ( loopIndex + 1, 55 ):
        print ( "Second Loop:" + str ( loopIndex ) )

    # Basic python cannot perform a for loop with a floating point loop variable
    # The np library can do this, but I am sticking to basic libraries for now.

    # Basic range loop
    print ( "\nLooping over a range of values" )
    for loopIndex in range ( 100 ):
        print ( f"Loop Index: {loopIndex}" )
    print ( "After Loop" )

    # Range variants
    print ( "\nDifferent methods of looping over a range." )
    print ( "\nStart at different index\n" )
    for loopIndex in range ( 1, 11 ):
        print ( f"Loop Index: {loopIndex}" )
    print ( "After Loop" )

    print ( "\nDifferent loop increment\n" )
    for loopIndex in range ( 5, 12, 2 ):
        print ( f"Loop Index: {loopIndex}" )
    print ( "After Loop" )

    print ( "\nReverse increment\n" )
    for loopIndex in range ( 9, 1, -1 ):
        print ( f"Loop Index: {loopIndex}" )
    print ( "After Loop" )

    # Looping over a range of array indices.
    # Good, range provided a number
    print ( "Looping over a range of indices in an array" )
    myList = [1, 2, 3, 4]
    for loopIndex in range ( len ( myList ) ):
        print ( f"Loop Index: {loopIndex}" )
    print ( "After Loop" )

    # Bad, myList is not a number - This will cause a TypeError
    print ( "\nAttempting bad range usage - myList is not a number" )
    for loopIndex in range ( myList ):
        print ( f"Loop Index: {loopIndex}" )
    print ( "After Loop" )

    # Initialize all entries in a large list to 0
    print ( "\nInitialize values in an array" )
    arraySamples = 10000
    intArray = [0] * arraySamples

    # This is kind of pointless in Python as the above code creates a Python array
    # of defined size with all values set to 0. The above statement is much faster 
    # Than this loop.
    for arrayIndex in range ( arraySamples ):
        intArray[arrayIndex] = 0

    # Initialize all entries in a large list to index * 2 + 1
    for arrayIndex in range ( arraySamples ):
        intArray[arrayIndex] = arrayIndex * 2 + 1

    # Example to be used in languages with strings.
    # Loop through a string and ensure it contains a valid number.
    # Scientific notation is not considered, a single period is valid, no non numeric characters are valid (in this example that will include spaces)
    print ( "\nVerify string contains valid number." )
    myString = "123.45"
    periodCount = 0
    validNumber = True
    for char in myString:
        if char == '.':
            periodCount = periodCount + 1
            if periodCount > 1:
                validNumber = False
                break
        elif char < '0' or char > '9':
            validNumber = False
            break

    print ( f"String is valid number: {validNumber}" )

    # Basic condition loop
    print ( "\nSimple conditional loop" )
    intCount = 1000000
    while True:
        # Just print the hello once so the terminal isn't overwhelmed.
        if intCount == 1000000:
            print ( "Hello" )

        # Break added so this isn't an infinite loop
        intCount = intCount - 1
        if intCount <= 0:
            break

    # While loop executing like our basic counting loop example
    print ( "\nExample while loop performing basic counting loop" )
    loopIndex = 0
    while loopIndex < 100:
        print ( loopIndex )
        loopIndex = loopIndex + 1
    print ( "After loop" )

    # More appropriate use of a while loop, when we don't know how many loops will be performed.
    print ( "\nBetter while loop with length of loop unknown at start." )
    dataArray = [1, 2, 3, 4, 5, 6, 5, 7, 5, 8, 5, 9, 5, 10, 11, 12, 13, 14, 15]
    dataArraySize = len ( dataArray )
    loopIndex = 0
    foundCount = 0
    fiveElementsFound = False
    while ( loopIndex < dataArraySize ) and ( fiveElementsFound == False ):
        if dataArray[loopIndex] < 10:
            foundCount = foundCount + 1
            if foundCount >= 5:
                fiveElementsFound = True
        loopIndex = loopIndex + 1

    # Using a loop to prompt user input until valid
    print ( "\nConditional loop based on user input." )
    exitMenu = False
    while exitMenu == False:
        print ( "Main Menu:" )
        print ( "1. Start Game" )
        print ( "2. Load Game" )
        print ( "3. Show Help" )
        print ( "4. Exit" )
        try:
            choice = int( input( "Enter your choice: " ) )
        except ValueError:
            choice = 0

        # Note the switch/match was only introduced in python 3.10
        # Older versions need if/else if/else.
        match choice:
            case 1:
                print("Starting new game...")
            case 2:
                print("Loading saved game...")
            case 3:
                print("Help: Use the number keys to navigate the menu.")
            case 4:
                print("Exiting program. Goodbye!")
                exitMenu = True
            case _:  # Default case (like 'default' in C++)
                print("Invalid choice. Please select a valid option.")

    # Python has no do loop, so we can only show with a while loop.
    print ( "While equivalent example: Hint, entering 42 will exit this loop" )
    number = 0
    while number != 42:
        try:
            number = int( input( "Guess a number: " ) )
        except ValueError:
            number = 0

    # Nested loop for simple 2d list print
    print ( "\nNested loop, 2d, example." )
    rowCount = 3
    columnCount = 3
    twoDList = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
    
    for rowIndex in range ( rowCount ):
        for columnIndex in range ( columnCount ):
            print ( twoDList[rowIndex][columnIndex], end=" " )
        print ( )

    # Nested loop to print character permutations
    print ( "\nNested permutations loop." )
    letters = ['a', 'b', 'c', 'd', 'e']
    lettersSize = len ( letters )
    
    for firstCharIndex in range ( lettersSize ):
        for secondCharIndex in range ( lettersSize ):
            print ( f"{letters[firstCharIndex]}{letters[secondCharIndex]}" )

    # Nested loop to order a list of numbers
    print ( "\nNested loop for sorting numbers." )
    myList2 = [6, 8, 9, 7, 4, 5, 0, 3, 1, 2]
    myList2Size = len ( myList2 )
    for listIndex in range ( myList2Size ):
        for swapIndex in range ( myList2Size - listIndex - 1 ):
            if myList2[swapIndex] > myList2[swapIndex + 1]:
                swapValue = myList2[swapIndex]
                myList2[swapIndex] = myList2[swapIndex + 1]
                myList2[swapIndex + 1] = swapValue

    # Break within nested loop
    print ( "\nUsing a break in a nested loop." )
    twoDList2 = [[1, 2], [3, 4]]
    rowCount = 2
    columnCount = 2
    
    for rowIndex in range ( rowCount ):
        for columnIndex in range ( columnCount ):
            print ( twoDList2[rowIndex][columnIndex] )
            break

    # Off by 1 error - The first loop will exceed the array bounds, 
    # some languages will crash, others will access invalid memory.
    print ( "\nOff by 1 loop error for array access." )
    myArray = [1, 2, 3, 4]
    arraySize = len ( myArray )
    print ( "Loop through array off by 1." )
    for loopIndex in range ( arraySize + 1 ):
        arrayValue = myArray[loopIndex]
        print ( f"{loopIndex} {arrayValue}" )

    print ( "Loop through array correct." )
    for loopIndex in range ( arraySize ):
        print ( myArray[loopIndex] )

    # Off by 1 when performing a partial list sort - Will exceed list bounds
    print ( "\nSort list with off by 1 error." )
    myList3 = [4, 3, 2, 1]
    myList3Size = len ( myList3 )
    for loopIndex in range ( myList3Size ):
        if myList3[loopIndex] > myList3[loopIndex + 1]:
            swapValue = myList3[loopIndex]
            myList3[loopIndex] = myList3[loopIndex + 1]
            myList3[loopIndex + 1] = swapValue

    # Off by 1 when performing a partial list sort - Will NOT exceed list bounds
    print ( "\nSort list without off by 1 error." )
    for loopIndex in range ( myList3Size - 1 ):
        if myList3[loopIndex] > myList3[loopIndex + 1]:
            swapValue = myList3[loopIndex]
            myList3[loopIndex] = myList3[loopIndex + 1]
            myList3[loopIndex + 1] = swapValue

    # Python does not allow an empty loop block, will not provide example.

    # Redeclaring loop variable
    print ( "\nRedeclared loop variable." )
    for loopIndex in range ( 10 ):
        for loopIndex in range ( 10 ):
            print ( "Loop Index:" + str ( loopIndex ) )

    # Loop condition untrue on entry
    print ( "\nLoop condition untrue on entry." )
    currentTime = 5
    nextFrame = 5
    while currentTime < nextFrame:
        print ( "Processing background tasks" )
        currentTime = currentTime + 1

    # Wrong reverse range loop - will not loop as arraySize is greater than 0
    print ( "\nReverse range loop failing to loop." )
    arraySize = 100
    for loopIndex in range ( arraySize, 0 ):
        print ( f"Loop index: {loopIndex}" )

    # Wrong reverse range loop for an array - we want to start at array size - 1 and end at index 0
    for loopIndex in range ( arraySize, 0, -1 ):
        print ( f"Loop index: {loopIndex}" )

    # Right reverse Range loop
    print ( "Reverse range loop correct." )
    for loopIndex in range ( arraySize - 1, -1, -1 ):
        print ( f"Loop index: {loopIndex}" )

    # Python like loop - A 2 will be missed and index out of range will occur
    # Remember in actual python a range of arraySize would be used, 
    # which handles the - 1 for you.
    print ( "\nRange loop removing array elements while iterating - will cause an error" )
    myArray3 = [5, 2, 8, 2, 2, 6, 3, 5]
    for loopIndex in range ( len ( myArray3 ) ):
        if loopIndex < len ( myArray3 ) and myArray3[loopIndex] == 2:
            myArray3.pop ( loopIndex )
            print ( f"myArray at index {loopIndex}: {myArray3}" )

    # Python like loop - Reverse for removal
    print ( "\nRange loop removing items while iterating array in reverse" )
    myArray5 = [5, 2, 8, 2, 2, 6, 3, 5]
    for loopIndex in range ( len ( myArray5 ) - 1, -1, -1 ):
        if myArray5[loopIndex] == 2:
            myArray5.pop ( loopIndex )
            print ( f"myArray at index {loopIndex}: {myArray5}" )

    # Python like while loop for array removals
    print ( "\nPython like while loop to remove array items." )
    myArray7 = [5, 2, 8, 2, 2, 6, 3, 5]
    loopIndex = 0
    while loopIndex < len ( myArray7 ):
        if myArray7[loopIndex] == 2:
            myArray7.pop ( loopIndex )
            print ( f"myArray at index {loopIndex}: {myArray7}" )
        else:
            loopIndex = loopIndex + 1

    # Python does not roll over integers, will not show this example.

    # Python has no unsigned, not showing loop while unsigned is positive.

    # Python does not allow uninitialized variables, not showing example.

if __name__ == "__main__":
    main()
