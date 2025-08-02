using System;

class Program
{
    static void Main ()
    {
        // Note the structured example is assuming zero based indexing.
        // One based index languages will differ.
        // Also note how some references are array location, with ranges from first to last, 
        // while indexes being zero based are 0 to size - 1.
        
        int[ ] temperatures = { 55, 58, 60, 65, 70, 73, 76, 79, 81, 83, 84, 85, 85, 84, 83, 81, 78, 75, 72, 69, 65, 62, 59, 57 };
        int[ ] testScores = { 95, 75, 86, 86, 78, 94 };
        int[ ] bookNumber = { 12495, 35786, 15863, 84962, 42697 };
        
        // 10th entry (0-based index 9)
        Console.WriteLine ( $"Temperature at tenth hour is { temperatures[ 9 ] }" );
        // 4th entry (0-based index 3)
        Console.WriteLine ( $"Fourth student grade is { testScores[ 3 ] }" );
        
        // 2nd entry (0-based index 1)
        int bookTwoIndex = 1;
        Console.WriteLine ( $"Second book index is { bookNumber[ bookTwoIndex ] }" );
        
        // First and last 0 based indexes, 0 and array size - 1.
        int hourCount = 24;
        int firstTemperatureIndex = 0;
        int lastTemperatureIndex = hourCount - 1;
        Console.WriteLine ( $"First temperature is { temperatures[ firstTemperatureIndex ] }" );
        Console.WriteLine ( $"Last temperature is { temperatures[ lastTemperatureIndex ] }" );
        
        // set temperature first entry to 65
        temperatures[ 0 ] = 65;
        Console.WriteLine ( $"First temperature is now { temperatures[ 0 ] }" );
        
        // set testScores fourth entry to 99
        testScores[ 3 ] = 99;
        Console.WriteLine ( $"Fourth test score is now { testScores[ 3 ] }" );
        
        // set bookNumber at index third entry to 75681
        int bookIndex = 2;
        bookNumber[ bookIndex ] = 75681;
        Console.WriteLine ( $"Third book number is now { bookNumber[ bookIndex ] }" );
        
        // Large arrays - C# initializes to default values (0 for int, false for bool, 0.0 for double)
        int largeArraySize = 10000;
        bool[ ] largeArray = new bool[ largeArraySize ];
        int[ ] largeArray1 = new int[ 1000 ];
        double[ ] largeArray2 = new double[ 5000 ];
        
        Console.WriteLine ( $"First large array first and last initial values: { largeArray[ 0 ] } { largeArray[ largeArraySize - 1 ] }" );
        Console.WriteLine ( $"Second large array first and last initial values: { largeArray1[ 0 ] } { largeArray1[ 999 ] }" );
        Console.WriteLine ( $"Third large array first and last initial values: { largeArray2[ 0 ] } { largeArray2[ 4999 ] }" );
        
        // set largeArray first entry to True
        largeArray[ 0 ] = true;
        // set largeArray last entry to False
        largeArray[ largeArraySize - 1 ] = false;
        Console.WriteLine ( $"First large array first and last values: { largeArray[ 0 ] } { largeArray[ largeArraySize - 1 ] }" );
        
        // set largeArray1 first entry to 25
        largeArray1[ 0 ] = 25;
        // set largeArray1 last entry to 55
        largeArray1[ 999 ] = 55;
        Console.WriteLine ( $"Second large array first and last values: { largeArray1[ 0 ] } { largeArray1[ 999 ] }" );
        
        // set largeArray2 first entry to 27.5
        largeArray2[ 0 ] = 27.5;
        // set largeArray2 last entry to 58.25
        largeArray2[ 4999 ] = 58.25;
        Console.WriteLine ( $"Third large array first and last values: { largeArray2[ 0 ] } { largeArray2[ 4999 ] }" );
        
        // Character array (C# strings are immutable, but we can use char arrays)
        char[ ] myString = new char[ 100 ];
        myString[ 0 ] = 'H';
        myString[ 1 ] = 'e';
        myString[ 2 ] = 'l';
        myString[ 3 ] = 'l';
        myString[ 4 ] = 'o';
        myString[ 5 ] = ' ';
        myString[ 6 ] = 'W';
        myString[ 7 ] = 'o';
        myString[ 8 ] = 'r';
        myString[ 9 ] = 'l';
        myString[ 10 ] = 'd';
        myString[ 11 ] = '.';
        myString[ 12 ] = '\0';
        // C# can convert char array to string, but we need to specify length to avoid printing null chars
        Console.WriteLine ( new string( myString, 0, 12 ) );
        
        char[ ] myString1 = { 'H', 'e', 'l', 'l', 'o', ' ', 'W', 'o', 'r', 'l', 'd', '.', '\0' };
        Console.WriteLine ( new string( myString1, 0, 12 ) );
        
        // Note the \0 is not needed in C# strings
        string myString2 = "Hello World.";
        Console.WriteLine ( myString2 );

        // 2D Array (jagged array or multidimensional array)
        char[ , ] twoDArray = new char[ 4, 4 ];
        twoDArray[ 0, 0 ] = '0';
        twoDArray[ 0, 1 ] = '1';
        twoDArray[ 0, 2 ] = '2';
        twoDArray[ 0, 3 ] = '3';
        twoDArray[ 1, 0 ] = '4';
        twoDArray[ 1, 1 ] = '5';
        twoDArray[ 1, 2 ] = '6';
        twoDArray[ 1, 3 ] = '7';
        twoDArray[ 2, 0 ] = '8';
        twoDArray[ 2, 1 ] = '9';
        twoDArray[ 2, 2 ] = 'A';
        twoDArray[ 2, 3 ] = 'B';
        twoDArray[ 3, 0 ] = 'C';
        twoDArray[ 3, 1 ] = 'D';
        twoDArray[ 3, 2 ] = 'E';
        twoDArray[ 3, 3 ] = 'F';
        
        // Note: the actual implementation of this code will use some advanced 
        // techniques that will not be described, only the results of the code observed.
        Console.Write ( "twoDArray memory location as flat data: " );
        unsafe
        {
            fixed ( char* ptr = &twoDArray[ 0, 0 ] )
            {
                for ( int i = 0; i < 16; i++ )
                {
                    Console.Write ( ptr[ i ] );
                }
            }
        }
        Console.WriteLine ();
        
        // Note these are not defined as constant, but the capital naming 
        // indicates the values should not change.
        const int RED = 0;
        const int GREEN = 1;
        const int BLUE = 2;
        const int YELLOW = 3;
        const int CYAN = 4;
        const int MAGENTA = 5;
        const int WHITE = 6;
        
        // Columns: Red Intensity, Green Intensity, Blue Intensity
        int[ , ] colorTable = {
            { 255, 0,   0   },  // Red
            { 0,   255, 0   },  // Green
            { 0,   0,   255 },  // Blue
            { 255, 255, 0   },  // Yellow = Red + Green
            { 0,   255, 255 },  // Cyan   = Green + Blue
            { 255, 0,   255 },  // Magenta = Red + Blue
            { 255, 255, 255 }   // White = Red + Green + Blue
        };
        
        Console.WriteLine ( $"CYAN color values: { colorTable[ CYAN, 0 ] } { colorTable[ CYAN, 1 ] } { colorTable[ CYAN, 2 ] }" );
    }
}