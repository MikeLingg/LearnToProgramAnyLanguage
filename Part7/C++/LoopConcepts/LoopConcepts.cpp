#include <climits>
#include <cstdio>
#include <cstring>

int myStackFunction()
{
    int firstInt = 10;
    int secondInt = 20;
    int thirdInt = 30;
    int fourthInt = 40;
    int fifthInt = 50;

    return firstInt;
}

int main( )
{
    // Basic loop.
    printf ( "\nSimple 100 iteration loop.\n" );
    for ( int loopIndex = 0; loopIndex < 100; loopIndex++ )
    {
        printf ( "%d\n", loopIndex );
    }
    printf ( "After Loop\n" );

    // Palindrome checker
    // It is a palindrome until we prove it is not.
    printf ( "\nPalindrome checker.\n" );
    char possiblePalindrome[] = "step on no pets";
    bool isPalindrome = true;

    int halfLength = strlen ( possiblePalindrome ) / 2;

    for ( int palindromeIndex = 0; palindromeIndex < halfLength; palindromeIndex++ )
    {
        if ( possiblePalindrome[palindromeIndex] !=
             possiblePalindrome[strlen ( possiblePalindrome ) - 1 - palindromeIndex] )
        {
            isPalindrome = false;
            break;
        }
    }

    if ( isPalindrome == true )
    {
        printf ( "%s is a palindrome.\n", possiblePalindrome );
    }
    else
    {
        printf ( "%s is NOT a palindrome.\n", possiblePalindrome );
    }

    // C like loop, this will only be explored in languages that support this format.
    printf ( "\nC style loop\n" );
    for ( int loopIndex = 0; loopIndex < 100; loopIndex++ )
    {
        printf ( "Loop Index:%d\n", loopIndex );
    }
    printf ( "After Loop\n" );

    // C like loop with two loop variables
    printf ( "\nC style loop looping through two variables.\n" );
    int palindromeFrontIndex, palindromeBackIndex;
    for ( palindromeFrontIndex = 0, palindromeBackIndex = strlen ( possiblePalindrome ) - 1;
          palindromeFrontIndex < halfLength; palindromeFrontIndex++,
          palindromeBackIndex-- )
    {
        if ( possiblePalindrome[palindromeFrontIndex] != possiblePalindrome[palindromeBackIndex] )
        {
            isPalindrome = false;
            break;
        }
    }

    // Two loops continuing a loop variable
    printf ( "\nLoops continuing a loop variable.\n" );
    int loopIndex;
    for ( loopIndex = 5; loopIndex < 15; loopIndex++ )
    {
        printf ( "Loop Index First Loop:%d\n", loopIndex );
    }
    for ( ; loopIndex < 55; loopIndex++ )
    {
        printf ( "Loop Index Second Loop:%d\n", loopIndex );
    }

    // C like loop with floating point loop variable.
    printf ( "\nLooping through a float variable.\n" );
    for ( float loopValue = 0.00f; loopValue < 100.00f; loopValue += 0.33f )
    {
        printf ( "Loop Value:%f\n", loopValue );
    }
    printf ( "After Loop\n" );

    // Looping over a range of array indices.
    // C++ cannot do this, but the bad example is still somewhat relevant.
    // Bad, myList is not a number - This will either not compile or will crash
    for ( int loopIndex = 0; loopIndex < myList; loopIndex++ )
    {
        printf ( "Loop Index: %d\n", loopIndex );
    }
    printf ( "After Loop\n" );

    // Initialize all entries in a large integer array to 0
    printf ( "\nInitialize values in an array\n" );
    const int arraySamples = 10000;
    int intArray[arraySamples];

    for ( int arrayIndex = 0; arrayIndex < arraySamples; arrayIndex++ )
    {
        intArray[arrayIndex] = 0;
    }

    // Initialize all entries in a large integer array to index * 2 + 1
    for ( int arrayIndex = 0; arrayIndex < arraySamples; arrayIndex++ )
    {
        intArray[arrayIndex] = arrayIndex * 2 + 1;
    }

    // Example to be used in languages with charArray.
    // Loop through a character array and ensure it contains a valid number.
    // Scientific notation is not considered, a single period is valid, no non numeric characters are valid (in this example that will include spaces)
    printf ( "\nVerify char array contains valid number." );
    char charArray[] = {'1', '2', '3', '.', '4', '5', '\0'};
    int periodCount = 0;
    bool validNumber = true;
    for ( int arrayIndex = 0; charArray[arrayIndex] != '\0'; arrayIndex++ )
    {
        if ( charArray[arrayIndex] == '.' )
        {
            periodCount = periodCount + 1;
            if ( periodCount > 1 )
            {
                validNumber = false;
                break;
            }
        }
        else if ( charArray[arrayIndex] < '0' || charArray[arrayIndex] > '9' )
        {
            validNumber = false;
            break;
        }
    }
    printf ( "String is valid number: %d\n", validNumber );

    // Basic condition loop
    printf ( "\nSimple conditional loop\n" );
    int intCount = 1000000;
    while ( true )
    {
        // Just print the hello once so the terminal isn't overwhelmed.
        if ( intCount == 1000000 )
        {
            printf ( "Hello\n" );
        }

        // Break added so this isn't an infinite loop
        intCount = intCount - 1;
        if ( intCount <= 0 )
        {
            break;
        }
    }

    // While loop executing like our basic counting loop example
    printf ( "\nExample while loop performing basic counting loop\n" );
    loopIndex = 0;
    while ( loopIndex < 100 )
    {
        printf ( "%d\n", loopIndex );
        loopIndex = loopIndex + 1;
    }
    printf ( "After loop\n" );

    // More appropriate use of a while loop, when we don't know how many loops will be performed.
    printf ( "\nBetter while loop with length of loop unknown at start.\n" );
    int dataArray[] = {1, 2, 3, 4, 5, 6, 5, 7, 5, 8, 5, 9, 5, 10, 11, 12, 13, 14, 15};
    int dataArraySize = 19;
    loopIndex = 0;
    int foundCount = 0;
    bool fiveElementsFound = false;
    while ( ( loopIndex < dataArraySize ) && ( fiveElementsFound == false ) )
    {
        if ( dataArray[loopIndex] < 10 )
        {
            foundCount = foundCount + 1;
            if ( foundCount >= 5 )
            {
                fiveElementsFound = true;
            }
        }
        loopIndex = loopIndex + 1;
    }

    // Using a loop to prompt user input until valid
    printf ( "\nConditional loop based on user input.\n" );
    bool exitMenu = false;
    while ( exitMenu == false )
    {
        printf ( "Main Menu:\n" );
        printf ( "1. Start Game\n" );
        printf ( "2. Load Game\n" );
        printf ( "3. Show Help\n" );
        printf ( "4. Exit\n" );
        printf ( "Enter your choice:" );
        
        int choice;
        scanf( "%d", &choice );

        switch ( choice )
        {
        case 1:
            printf ( "Starting new game...\n" );
            break;
        case 2:
            printf ( "Loading saved game...\n" );
            break;
        case 3:
            printf ( "Help: Use the number keys to navigate the menu.\n" );
            break;
        case 4:
            printf ( "Exiting program. Goodbye!\n" );
            exitMenu = true;
            break;
        default:
            printf ( "Invalid choice. Please select a valid option.\n" );
            break;
        }
    }

    // Do vs while loop.
    printf ( "Do While: Hint, entering 42 will exit this loop\n" );
    int number;
    do 
    {
        printf ( "Guess a number:" );
        scanf( "%d", &number );
    } 
    while ( number != 42 );

    printf ( "While Equivlant: Hint, entering 42 will exit this loop\n" );
    number = 0;
    while ( number != 42 )
    {
        printf ( "Guess a number:" );
        scanf( "%d", &number );
    }

    // Nested loop for simple 2d list print
    printf ( "\nNested loop, 2d, example." );
    int rowCount = 3, columnCount = 3;
    int twoDList[3][3] = {{1, 2, 3}, {4, 5, 6}, {7, 8, 9}};
    
    for ( int rowIndex = 0; rowIndex < rowCount; rowIndex++ )
    {
        for ( int columnIndex = 0; columnIndex < columnCount; columnIndex++ )
        {
            printf ( "%d ", twoDList[rowIndex][columnIndex] );
        }
        printf ( "\n" );
    }

    // Nested loop to print character permutations
    printf ( "\nNested permutations loop.\n" );
    char letters[] = {'a', 'b', 'c', 'd', 'e'};
    int lettersSize = 5;
    
    for ( int firstCharIndex = 0; firstCharIndex < lettersSize; firstCharIndex++ )
    {
        for ( int secondCharIndex = 0; secondCharIndex < lettersSize; secondCharIndex++ )
        {
            printf ( "%c%c\n", letters[firstCharIndex], letters[secondCharIndex] );
        }
    }

    // Nested loop to order a list of numbers
    printf ( "\nNested loop for sorting numbers.\n" );
    int myList2[] = {6, 8, 9, 7, 4, 5, 0, 3, 1, 2};
    int myList2Size = 10;
    for ( int listIndex = 0; listIndex < myList2Size; listIndex++ )
    {
        for ( int swapIndex = 0; swapIndex < myList2Size - listIndex - 1; swapIndex++ )
        {
            if ( myList2[swapIndex] > myList2[swapIndex + 1] )
            {
                int swapValue = myList2[swapIndex];
                myList2[swapIndex] = myList2[swapIndex + 1];
                myList2[swapIndex + 1] = swapValue;
            }
        }
    }

    // Break within nested loop
    printf ( "\nUsing a break in a nested loop.\n" );
    int twoDList2[2][2] = {{1, 2}, {3, 4}};
    rowCount = 2;
    columnCount = 2;
    
    for ( int rowIndex = 0; rowIndex < rowCount; rowIndex++ )
    {
        for ( int columnIndex = 0; columnIndex < columnCount; columnIndex++ )
        {
            printf ( "%d\n", twoDList2[rowIndex][columnIndex] );
            break;
        }
    }

    // Off by 1 error - The first loop will exceed the array bounds, 
    // some languages will crash, others will access invalid memory.
    printf ( "\nOff by 1 loop error for array access.\n" );
    int myArray[] = {1, 2, 3, 4};
    int arraySize = 4;
    printf ( "Loop through array off by 1.\n" );
    for ( int loopIndex = 0; loopIndex <= arraySize; loopIndex++ )
    {
        int arrayValue = myArray[loopIndex];
        printf ( "%d %d\n", loopIndex, arrayValue );
    }

    printf ( "Loop through array correct.\n" );
    for ( int loopIndex = 0; loopIndex < arraySize; loopIndex++ )
    {
        printf ( "%d\n", myArray[loopIndex] );
    }

    // Off by 1 when performing a partial list sort - Will exceed list bounds
    printf ( "\nSort list with off by 1 error.\n" );
    int myList3[] = {4, 3, 2, 1};
    int myList3Size = 4;
    for ( int loopIndex = 0; loopIndex < myList3Size; loopIndex++ )
    {
        if ( myList3[loopIndex] > myList3[loopIndex + 1] )
        {
            int swapValue = myList3[loopIndex];
            myList3[loopIndex] = myList3[loopIndex + 1];
            myList3[loopIndex + 1] = swapValue;
        }
    }

    // Off by 1 when performing a partial list sort - Will NOT exceed list bounds
    printf ( "\nSort list without off by 1 error.\n" );
    for ( int loopIndex = 0; loopIndex < myList3Size - 1; loopIndex++ )
    {
        if ( myList3[loopIndex] > myList3[loopIndex + 1] )
        {
            int swapValue = myList3[loopIndex];
            myList3[loopIndex] = myList3[loopIndex + 1];
            myList3[loopIndex + 1] = swapValue;
        }
    }

    // Empty loop block - This issue is not as noticeable outside of C language format, 
    // we will look at how this occurs in each language, but it is primarily a C issue.
    printf ( "\nEmpty loop block.\n" );
    for ( int loopIndex = 0; loopIndex < 100; loopIndex++ );
    {
        printf ( "Hello\n" );
    }

    // Reused loop variables in a language that allows nested loops without blocks.
    // This primarily only can be explored in C/C++.
    printf ( "\nReused loop variables without code blocks.\n" );
    for ( int loopIndex = 0; loopIndex < 100; loopIndex++ )
        for ( int loopIndex = 0; loopIndex < 100; loopIndex++ )
            printf ( "%d\n", loopIndex );

    // Redeclaring loop variable in blocks.
    printf ( "\nRedeclared loop variable with loop blocks.\n" );
    for ( int loopIndex = 0; loopIndex < 100; loopIndex++ )
    {
        for ( int loopIndex = 0; loopIndex < 100; loopIndex++ )
        {
            printf ( "%d\n", loopIndex );
        }
    }

    // Loop condition untrue on entry
    printf ( "\nLoop condition untrue on entry.\n" );
    int currentTime = 5;
    int nextFrame = 5;
    while ( currentTime < nextFrame )
    {
        printf ( "Processing background tasks\n" );
        currentTime = currentTime + 1;
    }

    // Wrong reverse C style loop
    printf ( "\nWrong reverse C style loop.\n" );
    for ( int loopIndex = arraySize; loopIndex > 0; loopIndex-- )
    {
        printf ( "Loop index: %d\n", loopIndex );
    }
    // Right reverse C style loop
    for ( int loopIndex = arraySize - 1; loopIndex >= 0; loopIndex-- )
    {
        printf ( "Loop index: %d\n", loopIndex );
    }

    // C like loop - A 2 will be missed and index out of range should occur
    // Remember in C the < is used so no - 1
    printf ( "\nC like loop removing array elements while iterating - will array index out of bounds\n" );
    int myArray3[] = {5, 2, 8, 2, 2, 6, 3, 5};
    int myArray3Size = 8;
    for ( int loopIndex = 0; loopIndex < myArray3Size; loopIndex++ )
    {
        if ( myArray3[loopIndex] == 2 )
        {
            // C has no remove function so we have to move the data manually
            // Note the new variable for tracking the index being moved
            // Here we have to use size minus 1 as we are moving the next array entry to the current index
            for ( int moveIndex = loopIndex; moveIndex < myArray3Size - 1; moveIndex++ )
            {
                myArray3[moveIndex] = myArray3[moveIndex + 1];
            }

            // We also have no print array built into C, so do it manually
            printf ( "myArray at index %d: ", loopIndex );
            for ( int printIndex = 0; printIndex < myArray3Size; printIndex++ )
            {
                printf ( "%d, ", myArray3[printIndex] );
            }
            printf ( "\n" );
        }
    }

    // C like loop - Track removals
    printf ( "\nC like loop removing items while iterating array by tracking removals\n" );
    int myArray5[] = {5, 2, 8, 2, 2, 6, 3, 5};
    int myArray5Size = 8;
    for ( int loopIndex = 0; loopIndex < myArray5Size; loopIndex++ )
    {
        if ( myArray5[loopIndex] == 2 )
        {
            // C has no remove function so we have to move the data manually
            // Note the new variable for tracking the index being moved
            // Here we have to use size minus 1 as we are moving the next array entry to the current index
            for ( int moveIndex = loopIndex; moveIndex < myArray5Size - 1; moveIndex++ )
            {
                myArray5[moveIndex] = myArray5[moveIndex + 1];
            }

            // We also have no print array built into C, so do it manually
            printf ( "myArray at index %d: ", loopIndex );
            for ( int printIndex = 0; printIndex < myArray5Size; printIndex++ )
            {
                printf ( "%d, ", myArray5[printIndex] );
            }
            printf ( "\n" );

            // Note how we modify the array index and size for removals
            loopIndex = loopIndex - 1;
            myArray5Size = myArray5Size - 1;
        }
    }

    // C like loop - Track removals
    printf ( "C like while loop to remove array items.\n" );

    int myArray7[] = {5, 2, 8, 2, 2, 6, 3, 5};
    int myArray7Size = 8;
    int loopIndex3 = 0;
    while ( loopIndex3 < myArray7Size )
    {
        if ( myArray7[loopIndex3] == 2 )
        {
            // C has no remove function so we have to move the data manually
            // Note the new variable for tracking the index being moved
            // Here we have to use size minus 1 as we are moving the next array entry to the current index
            for ( int moveIndex = loopIndex3; moveIndex < myArray7Size - 1; moveIndex++ )
            {
                myArray7[moveIndex] = myArray7[moveIndex + 1];
            }

            // We also have no print array built into C, so do it manually
            printf ( "myArray at index %d: ", loopIndex3 );
            for ( int printIndex = 0; printIndex < myArray7Size; printIndex++ )
            {
                printf ( "%d, ", myArray7[printIndex] );
            }
            printf ( "\n" );

            // Note how we modify the array size for removals
            myArray7Size = myArray7Size - 1;
        }
        else
        {
            loopIndex3 = loopIndex3 + 1;
        }
    }

    // Loop index can't be larger than INT_MAX, this loop will try to do that 
    // but instead overflow the loop index back to 0, causing an infinite loop
    // We start the loop at a value of 1, loop index can only be 0 if overflow occurred.
    printf ( "Loop to int max.\n" );
    for ( int loopIndex = INT_MAX - 1000; loopIndex <= INT_MAX; loopIndex++ )
    {
        printf ( "Loop Index Comparison:%i %i\n", loopIndex, loopIndex <= 0 );
        if ( loopIndex <= 0 )
        {
            printf ( "Loop index was set negative!\n" );
            break;
        }
    }

    // This example will be limited to languages with unsigned variables.
    // Can never satisfy the loop condition, so we break out on rollover
    printf ( "\nUnsigned loop comparison, will always be true.\n" );
    for ( unsigned int loopIndex = 10; loopIndex >= 0; loopIndex-- )
    {
        printf ( "%u\n", loopIndex );
        if ( loopIndex > 10 )
        {
            printf ( "Loop index rolled over! %u\n", loopIndex );
            break;
        }
    }

    // Comma vs semicolon - This is a C specific error.
    // Other languages will show similar formatting errors that I can come up with
    printf ( "\nC loop formatting error, comma not semicolon.\n" );
    for ( int loopVariable = 0, loopVariable < 100, loopVariable++ )
    {
        printf ( "Loop Variable:%d\n", loopVariable );
    }

    // Uninitialized loop variables
    // Note that the call to a function that assigns some local variables should 
    // cause our unassigned variable to not be 0.  We will talk in more detail why
    // after functions when we talk about pointers and references, and heap vs
    // stack.
    printf ( "\nLoop with uninitialized variable.\n" );
    myStackFunction();
    int loopVariable;
    for ( ; loopVariable < 100; loopVariable++ )
    {
        printf ( "Loop Variable %d\n", loopVariable );
    }

    return 0;
}
