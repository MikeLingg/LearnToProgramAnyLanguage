using System;

class Program
{
    static int MyStackFunction()
    {
        int firstInt = 10;
        int secondInt = 20;
        int thirdInt = 30;
        int fourthInt = 40;
        int fifthInt = 50;
        
        return firstInt;
    }

    static void Main()
    {
        // Basic loop.
        Console.WriteLine ( "\nSimple 100 iteration loop." );
        for ( int loopIndex = 0; loopIndex < 100; loopIndex++ )
        {
            Console.WriteLine ( loopIndex );
        }
        Console.WriteLine ( "After Loop" );

        // Palindrome checker
        // It is a palindrome until we prove it is not.
        Console.WriteLine ( "\nPalindrome checker." );
        string possiblePalindrome = "step on no pets";
        bool isPalindrome = true;

        int halfLength = possiblePalindrome.Length / 2;

        for ( int palindromeIndex = 0; palindromeIndex < halfLength; palindromeIndex++ )
        {
            if ( possiblePalindrome[palindromeIndex] != possiblePalindrome[possiblePalindrome.Length - 1 - palindromeIndex] )
            {
                isPalindrome = false;
                break;
            }
        }

        if ( isPalindrome == true )
        {
            Console.WriteLine ( $"{possiblePalindrome} is a palindrome." );
        }
        else
        {
            Console.WriteLine ( $"{possiblePalindrome} is NOT a palindrome." );
        }

        // C# can have two loop variables using comma separator
        Console.WriteLine ( "\nC# style loop looping through two variables." );
        int palindromeFrontIndex, palindromeBackIndex;
        for ( palindromeFrontIndex = 0, palindromeBackIndex = possiblePalindrome.Length - 1; palindromeFrontIndex < halfLength; palindromeFrontIndex++, palindromeBackIndex-- )
        {
            if ( possiblePalindrome[palindromeFrontIndex] != possiblePalindrome[palindromeBackIndex] )
            {
                isPalindrome = false;
                break;
            }
        }

        // Two loops continuing a loop variable
        Console.WriteLine ( "\nLoops continuing a loop variable." );
        int continuedLoopIndex;
        for ( continuedLoopIndex = 5; continuedLoopIndex < 15; continuedLoopIndex++ )
        {
            Console.WriteLine ( $"Loop Index First Loop: {continuedLoopIndex}" );
        }
        for ( ; continuedLoopIndex < 55; continuedLoopIndex++ )
        {
            Console.WriteLine ( $"Loop Index Second Loop: {continuedLoopIndex}" );
        }

        // C# can perform a for loop with floating point loop variables
        Console.WriteLine ( "\nLooping through a float variable." );
        for ( float loopValue = 0.00f; loopValue < 100.00f; loopValue += 0.33f )
        {
            Console.WriteLine ( $"Loop Value: {loopValue}" );
        }
        Console.WriteLine ( "After Loop" );

        // C# has foreach for collections but no built-in range generator like Python
        Console.WriteLine ( "\nLooping over array indices." );
        int[] myList = {1, 2, 3, 4};
        for ( int loopIndex = 0; loopIndex < myList.Length; loopIndex++ )
        {
            Console.WriteLine ( $"Loop Index: {loopIndex}" );
        }
        Console.WriteLine ( "After Loop" );

        // Bad, myList is not a number - This will not compile
        Console.WriteLine ( "\nAttempting bad range usage - myList is not a number" );
        for ( int loopIndex = 0; loopIndex < myList; loopIndex++ )
        {
            Console.WriteLine ( $"Loop Index: {loopIndex}" );
        }
        Console.WriteLine ( "After Loop" );

        // Initialize all entries in a large array to 0
        Console.WriteLine ( "\nInitialize values in an array" );
        int arraySamples = 10000;
        int[] intArray = new int[arraySamples];

        // This is kind of pointless in C# as arrays are automatically initialized to 0
        // The above statement is much faster than this loop.
        for ( int arrayIndex = 0; arrayIndex < arraySamples; arrayIndex++ )
        {
            intArray[arrayIndex] = 0;
        }

        // Initialize all entries in a large array to index * 2 + 1
        for ( int arrayIndex = 0; arrayIndex < arraySamples; arrayIndex++ )
        {
            intArray[arrayIndex] = arrayIndex * 2 + 1;
        }

        // Example with string validation.
        // Loop through a string and ensure it contains a valid number.
        // Scientific notation is not considered, a single period is valid, no non numeric characters are valid
        Console.WriteLine ( "\nVerify string contains valid number." );
        string myString = "123.45";
        int periodCount = 0;
        bool validNumber = true;
        for ( int arrayIndex = 0; arrayIndex < myString.Length; arrayIndex++ )
        {
            char character = myString[arrayIndex];
            if ( character == '.' )
            {
                periodCount = periodCount + 1;
                if ( periodCount > 1 )
                {
                    validNumber = false;
                    break;
                }
            }
            else if ( character < '0' || character > '9' )
            {
                validNumber = false;
                break;
            }
        }

        Console.WriteLine ( $"String is valid number: {validNumber}" );

        // Basic condition loop
        Console.WriteLine ( "\nSimple conditional loop" );
        int intCount = 1000000;
        while ( true )
        {
            // Just print the hello once so the terminal isn't overwhelmed.
            if ( intCount == 1000000 )
            {
                Console.WriteLine ( "Hello" );
            }

            // Break added so this isn't an infinite loop
            intCount = intCount - 1;
            if ( intCount <= 0 )
            {
                break;
            }
        }

        // While loop executing like our basic counting loop example
        Console.WriteLine ( "\nExample while loop performing basic counting loop" );
        int whileIndex = 0;
        while ( whileIndex < 100 )
        {
            Console.WriteLine ( whileIndex );
            whileIndex = whileIndex + 1;
        }
        Console.WriteLine ( "After loop" );

        // More appropriate use of a while loop, when we don't know how many loops will be performed.
        Console.WriteLine ( "\nBetter while loop with length of loop unknown at start." );
        int[] dataArray = {1, 2, 3, 4, 5, 6, 5, 7, 5, 8, 5, 9, 5, 10, 11, 12, 13, 14, 15};
        int dataArraySize = dataArray.Length;
        whileIndex = 0;
        int foundCount = 0;
        bool fiveElementsFound = false;
        while ( ( whileIndex < dataArraySize ) && ( fiveElementsFound == false ) )
        {
            if ( dataArray[whileIndex] < 10 )
            {
                foundCount = foundCount + 1;
                if ( foundCount >= 5 )
                {
                    fiveElementsFound = true;
                }
            }
            whileIndex = whileIndex + 1;
        }

        // Using a loop to prompt user input until valid
        Console.WriteLine ( "\nConditional loop based on user input." );
        bool exitMenu = false;
        while ( exitMenu == false )
        {
            Console.WriteLine ( "Main Menu:" );
            Console.WriteLine ( "1. Start Game" );
            Console.WriteLine ( "2. Load Game" );
            Console.WriteLine ( "3. Show Help" );
            Console.WriteLine ( "4. Exit" );
            Console.Write ( "Enter your choice: " );
            
            string input = Console.ReadLine();
            int choice;
            if ( !int.TryParse ( input, out choice ) )
            {
                choice = 0;
            }

            // C# has switch statements
            switch ( choice )
            {
                case 1:
                    Console.WriteLine ( "Starting new game..." );
                    break;
                case 2:
                    Console.WriteLine ( "Loading saved game..." );
                    break;
                case 3:
                    Console.WriteLine ( "Help: Use the number keys to navigate the menu." );
                    break;
                case 4:
                    Console.WriteLine ( "Exiting program. Goodbye!" );
                    exitMenu = true;
                    break;
                default:
                    Console.WriteLine ( "Invalid choice. Please select a valid option." );
                    break;
            }
        }

        // C# has no do-while loop equivalent to C++
        Console.WriteLine ( "Do While: Hint, entering 42 will exit this loop" );
        int number;
        do
        {
            Console.Write ( "Guess a number: " );
            string numberInput = Console.ReadLine();
            if ( !int.TryParse ( numberInput, out number ) )
            {
                number = 0;
            }
        }
        while ( number != 42 );

        Console.WriteLine ( "While Equivalent: Hint, entering 42 will exit this loop" );
        number = 0;
        while ( number != 42 )
        {
            Console.Write ( "Guess a number: " );
            string numberInput = Console.ReadLine();
            if ( !int.TryParse ( numberInput, out number ) )
            {
                number = 0;
            }
        }

        // Nested loop for simple 2d array print
        Console.WriteLine ( "\nNested loop, 2d, example." );
        int rowCount = 3, columnCount = 3;
        int[,] twoDList = {{1, 2, 3}, {4, 5, 6}, {7, 8, 9}};
        
        for ( int rowIndex = 0; rowIndex < rowCount; rowIndex++ )
        {
            for ( int columnIndex = 0; columnIndex < columnCount; columnIndex++ )
            {
                Console.Write ( $"{twoDList[rowIndex, columnIndex]} " );
            }
            Console.WriteLine ();
        }

        // Nested loop to print character permutations
        Console.WriteLine ( "\nNested permutations loop." );
        char[] letters = {'a', 'b', 'c', 'd', 'e'};
        int lettersSize = letters.Length;
        
        for ( int firstCharIndex = 0; firstCharIndex < lettersSize; firstCharIndex++ )
        {
            for ( int secondCharIndex = 0; secondCharIndex < lettersSize; secondCharIndex++ )
            {
                Console.WriteLine ( $"{letters[firstCharIndex]}{letters[secondCharIndex]}" );
            }
        }

        // Nested loop to order a list of numbers
        Console.WriteLine ( "\nNested loop for sorting numbers." );
        int[] myList2 = {6, 8, 9, 7, 4, 5, 0, 3, 1, 2};
        int myList2Size = myList2.Length;
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
        Console.WriteLine ( "\nUsing a break in a nested loop." );
        int[,] twoDList2 = {{1, 2}, {3, 4}};
        rowCount = 2;
        columnCount = 2;
        
        for ( int rowIndex = 0; rowIndex < rowCount; rowIndex++ )
        {
            for ( int columnIndex = 0; columnIndex < columnCount; columnIndex++ )
            {
                Console.WriteLine ( twoDList2[rowIndex, columnIndex] );
                break;
            }
        }

        // Off by 1 error - The first loop will exceed the array bounds
        // C# will throw an IndexOutOfRangeException at runtime
        Console.WriteLine ( "\nOff by 1 loop error for array access." );
        int[] myArray = {1, 2, 3, 4};
        int arraySize = myArray.Length;
        Console.WriteLine ( "Loop through array off by 1." );
        for ( int loopIndex = 0; loopIndex <= arraySize; loopIndex++ )
        {
            int arrayValue = myArray[loopIndex];
            Console.WriteLine ( $"{loopIndex} {arrayValue}" );
        }

        Console.WriteLine ( "Loop through array correct." );
        for ( int loopIndex = 0; loopIndex < arraySize; loopIndex++ )
        {
            Console.WriteLine ( myArray[loopIndex] );
        }

        // Off by 1 when performing a partial list sort - Will exceed list bounds
        Console.WriteLine ( "\nSort list with off by 1 error." );
        int[] myList3 = {4, 3, 2, 1};
        int myList3Size = myList3.Length;
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
        Console.WriteLine ( "\nSort list without off by 1 error." );
        for ( int loopIndex = 0; loopIndex < myList3Size - 1; loopIndex++ )
        {
            if ( myList3[loopIndex] > myList3[loopIndex + 1] )
            {
                int swapValue = myList3[loopIndex];
                myList3[loopIndex] = myList3[loopIndex + 1];
                myList3[loopIndex + 1] = swapValue;
            }
        }

        // C# does not allow an empty loop block, will not provide example.

        // Redeclaring loop variable in blocks.
        Console.WriteLine ( "\nRedeclared loop variable with loop blocks." );
        for ( int loopIndex = 0; loopIndex < 10; loopIndex++ )
        {
            for ( int loopIndex = 0; loopIndex < 10; loopIndex++ )
            {
                Console.WriteLine ( $"Loop Index: {loopIndex}" );
            }
        }

        // Loop condition untrue on entry
        Console.WriteLine ( "\nLoop condition untrue on entry." );
        int currentTime = 5;
        int nextFrame = 5;
        while ( currentTime < nextFrame )
        {
            Console.WriteLine ( "Processing background tasks" );
            currentTime = currentTime + 1;
        }

        // Wrong reverse C style loop
        Console.WriteLine ( "\nWrong reverse C style loop." );
        for ( int loopIndex = arraySize; loopIndex > 0; loopIndex-- )
        {
            Console.WriteLine ( $"Loop index: {loopIndex}" );
        }
        // Right reverse C style loop
        for ( int loopIndex = arraySize - 1; loopIndex >= 0; loopIndex-- )
        {
            Console.WriteLine ( $"Loop index: {loopIndex}" );
        }

        // C# List - removing elements while iterating will cause index issues
        Console.WriteLine ( "\nList removing elements while iterating - will throw exception" );
        System.Collections.Generic.List<int> myList4 = new System.Collections.Generic.List<int> {5, 2, 8, 2, 2, 6, 3, 5};
        int originalLen = myList4.Count;
        for ( int index = 0; index < originalLen; index++ )
        {
            if ( myList4[index] == 2 )
            {
                myList4.RemoveAt ( index );
                Console.WriteLine ( $"myList at index {index}: [{string.Join ( ", ", myList4 )}]" );
            }
        }

        // C# List - reverse for removal (correct approach)
        Console.WriteLine ( "\nList removing items while iterating in reverse" );
        System.Collections.Generic.List<int> myList5 = new System.Collections.Generic.List<int> {5, 2, 8, 2, 2, 6, 3, 5};
        for ( int index = myList5.Count - 1; index >= 0; index-- )
        {
            if ( myList5[index] == 2 )
            {
                myList5.RemoveAt ( index );
                Console.WriteLine ( $"myList at index {index}: [{string.Join ( ", ", myList5 )}]" );
            }
        }

        // C# while loop for list removals
        Console.WriteLine ( "\nC# while loop to remove list items." );
        System.Collections.Generic.List<int> myList6 = new System.Collections.Generic.List<int> {5, 2, 8, 2, 2, 6, 3, 5};
        int index2 = 0;
        while ( index2 < myList6.Count )
        {
            if ( myList6[index2] == 2 )
            {
                myList6.RemoveAt ( index2 );
                Console.WriteLine ( $"myList at index {index2}: [{string.Join ( ", ", myList6 )}]" );
            }
            else
            {
                index2 = index2 + 1;
            }
        }

        // C# integers can overflow but will wrap around or throw exceptions depending on context
        Console.WriteLine ( "Loop to very large number." );
        int largeNumber = int.MaxValue - 1000;
        int count = 0;
        for ( int loopIndex = largeNumber; loopIndex <= int.MaxValue; loopIndex++ )
        {
            count = count + 1;
            if ( loopIndex < 0 )
            {
                Console.WriteLine ( $"Loop index went negative at count {count}, exiting!" );
                break;
            }
        }

    }
}
