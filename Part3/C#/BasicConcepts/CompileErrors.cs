using System;

class Program
{
    static void Main()
    {
        // Error 3: Wrong array declaration syntax (C-style)
        int[] temperatures = new int[24];

        // Error 4: Missing 'new' keyword for array initialization
        int[] testScores = new int[5];

        // Error 5: Using curly braces without 'new' keyword
        int[] values = { 1, 2, 3, 4, 5 };

        // Error 6: Wrong syntax for array initialization
        int[] bookNumbers = { 10, 20, 30 };

        // Error 7: Missing semicolon after array declaration
        int[] scores = new int[10];

        // Error 8: Using parentheses instead of square brackets for array access
        int temp = testScores[ 0 ];

        // Error 9: Missing square brackets for array access
        int value = values[ 2 ];

        // Error 10: Wrong string declaration syntax
        string message = new string("Hello");

        // Error 11: Using single quotes for string instead of double quotes
        string greeting = "Hello World";

        // Error 12: Using double quotes for char instead of single quotes
        char letter = 'A';

        // Error 13: Missing 'new' keyword for multidimensional array
        int[,] matrix = new int[2,2];

        // Error 14: Wrong syntax for multidimensional array initialization
        int[,] matrix2 = { {1, 2}, {3, 4} };

        // Error 15: Using jagged array syntax for multidimensional array
        int[,] wrongMatrix = new int[2, 2];

        // Error 16: Missing array size with no initialization
        string[] names = new string[10];

        // Error 17: Wrong syntax for jagged array declaration
        int[,] jaggedArray = new int[3,3];

        // Error 18: Using assignment operator instead of equality in declaration
        int[] numbers2 = new int[5];

        // Error 19: Missing closing bracket in array declaration
        int[] data = new int[10];

        // Error 22: Missing 'using System;' directive for Console
        // (This would be an error if we removed the using statement)

        // Error 23: Wrong string interpolation syntax
        Console.WriteLine("Value is: {values[0]}");

        // Error 24: Missing $ for string interpolation
        Console.WriteLine("Value is: {values[0]}");

        // Error 26: Trying to use uninitialized array
        int[] uninitializedArray = new int[5];
        uninitializedArray[0] = 5;

        // Error 27: Wrong syntax for array copying
        int[] source = {1, 2, 3};
        int[] dest;
        dest = source;  // This compiles but creates reference, not copy

        // Error 28: Missing closing brace in array initializer
        int[] incomplete = new int[] { 1, 2, 3 };

    }
}
