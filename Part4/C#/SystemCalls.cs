using System;

class Program
{
    static void Main ()
    {
        // Note: For this program to function as expected, the user will have to correctly enter the requested values.

        // Boolean strings of true/false cannot be converted to a bool variable without conditions,
        // so we will discuss how that works in the branches video coming soon.
        // Some languages will not even allow for reading values of 0 or 1 from the terminal as
        // booleans so we will identify which languages this fails with, and revisit how to make 
        // this work in the branches video.

        string userInput;

        Console.Write ( "Type true and press enter." );
        userInput = Console.ReadLine ();
        bool parseSuccess = bool.TryParse ( userInput, out bool enteredBoolean );
        Console.WriteLine ( "The user entered the boolean {0} (parse succeeded: {1})", enteredBoolean, parseSuccess );

        Console.Write ( "Type false and press enter." );
        userInput = Console.ReadLine ();
        parseSuccess = bool.TryParse ( userInput, out enteredBoolean );
        Console.WriteLine ( "The user entered the boolean {0} (parse succeeded: {1})", enteredBoolean, parseSuccess );

        Console.Write ( "Type True and press enter." );
        userInput = Console.ReadLine ();
        parseSuccess = bool.TryParse ( userInput, out enteredBoolean );
        Console.WriteLine ( "The user entered the boolean {0} (parse succeeded: {1})", enteredBoolean, parseSuccess );

        Console.Write ( "Type FALSE and press enter." );
        userInput = Console.ReadLine ();
        parseSuccess = bool.TryParse ( userInput, out enteredBoolean );
        Console.WriteLine ( "The user entered the boolean {0} (parse succeeded: {1})", enteredBoolean, parseSuccess );

        Console.Write ( "Type 0 and press enter." );
        userInput = Console.ReadLine ();
        parseSuccess = bool.TryParse ( userInput, out enteredBoolean );
        Console.WriteLine ( "The user entered the boolean {0} (parse succeeded: {1})", enteredBoolean, parseSuccess );

        Console.Write ( "Type 1 and press enter." );
        userInput = Console.ReadLine ();
        parseSuccess = bool.TryParse ( userInput, out enteredBoolean );
        Console.WriteLine ( "The user entered the boolean {0} (parse succeeded: {1})", enteredBoolean, parseSuccess );

        Console.Write ( "Type 11 and press enter." );
        userInput = Console.ReadLine ();
        parseSuccess = bool.TryParse ( userInput, out enteredBoolean );
        Console.WriteLine ( "The user entered the boolean {0} (parse succeeded: {1})", enteredBoolean, parseSuccess );

        Console.Write ( "Type -1 and press enter." );
        userInput = Console.ReadLine ();
        parseSuccess = bool.TryParse ( userInput, out enteredBoolean );
        Console.WriteLine ( "The user entered the boolean {0} (parse succeeded: {1})", enteredBoolean, parseSuccess );

        Console.Write ( "Type 55 and press enter." );
        userInput = Console.ReadLine ();
        bool intParseSuccess = int.TryParse ( userInput, out int enteredInteger );
        Console.WriteLine ( "The user entered the integer {0} (parse succeeded: {1})", enteredInteger, intParseSuccess );

        Console.Write ( "Type 55.5 and press enter." );
        userInput = Console.ReadLine ();
        bool floatParseSuccess = float.TryParse ( userInput, out float enteredFloat );
        Console.WriteLine ( "The user entered the float {0} (parse succeeded: {1})", enteredFloat, floatParseSuccess );

        Console.Write ( "Type Hello World! and press enter." );
        userInput = Console.ReadLine ();
        Console.WriteLine ( "The user entered the string {0}", userInput );

        Console.Write ( "Type 123abc and press enter." );
        userInput = Console.ReadLine ();
        intParseSuccess = int.TryParse ( userInput, out enteredInteger );
        Console.WriteLine ( "The user entered the integer {0} (parse succeeded: {1})", enteredInteger, intParseSuccess );

        Console.Write ( "Type 123.45 and press enter." );
        userInput = Console.ReadLine ();
        intParseSuccess = int.TryParse ( userInput, out enteredInteger );
        Console.WriteLine ( "The user entered the integer {0} (parse succeeded: {1})", enteredInteger, intParseSuccess );

        Console.Write ( "Type abc123 and press enter." );
        userInput = Console.ReadLine ();
        intParseSuccess = int.TryParse ( userInput, out enteredInteger );
        Console.WriteLine ( "The user entered the integer {0} (parse succeeded: {1})", enteredInteger, intParseSuccess );

        Console.Write ( "Type  567 and press enter." );
        userInput = Console.ReadLine ();
        intParseSuccess = int.TryParse ( userInput, out enteredInteger );
        Console.WriteLine ( "The user entered the integer {0} (parse succeeded: {1})", enteredInteger, intParseSuccess );

        Console.Write ( "Type +567 and press enter." );
        userInput = Console.ReadLine ();
        intParseSuccess = int.TryParse ( userInput, out enteredInteger );
        Console.WriteLine ( "The user entered the integer {0} (parse succeeded: {1})", enteredInteger, intParseSuccess );

        Console.Write ( "Type -567 and press enter." );
        userInput = Console.ReadLine ();
        intParseSuccess = int.TryParse ( userInput, out enteredInteger );
        Console.WriteLine ( "The user entered the integer {0} (parse succeeded: {1})", enteredInteger, intParseSuccess );

        // C# has Math.Abs() that works with multiple types
        Console.WriteLine ( "Abs of -5 is {0}", Math.Abs ( -5 ) );
        Console.WriteLine ( "Abs of -5.5 is {0}", Math.Abs ( -5.5 ) );
        Console.WriteLine ( "Abs of a is {0}", Math.Abs ( (int)'a' ) );

        Console.WriteLine ( "Pow of 2^5 is {0}", Math.Pow ( 2, 5 ) );
        Console.WriteLine ( "Pow of 2.2^5.2 is {0}", Math.Pow ( 2.2, 5.2 ) );
        Console.WriteLine ( "Pow of a^b is {0}", Math.Pow ( (double)'a', (double)'b' ) );

        // Note trig functions are almost always in radians, not degrees
        Console.WriteLine ( "Sin of 90 is {0}", Math.Sin ( 90 ) );
        Console.WriteLine ( "Sin of pi/2 is {0}", Math.Sin ( Math.PI / 2 ) );

        Console.WriteLine ( "Cos of 180 is {0}", Math.Cos ( 180 ) );
        Console.WriteLine ( "Cos of pi is {0}", Math.Cos ( Math.PI ) );

        // Rounding type functions are very useful for explicit float to int conversions
        Console.WriteLine ( "Floor of 5.5 is {0}", Math.Floor ( 5.5 ) );
        Console.WriteLine ( "Floor of -5.5 is {0}", Math.Floor ( -5.5 ) );

        Console.WriteLine ( "Ceil of 5.5 is {0}", Math.Ceiling ( 5.5 ) );
        Console.WriteLine ( "Ceil of -5.5 is {0}", Math.Ceiling ( -5.5 ) );

        Console.WriteLine ( "Round of 5.5 is {0}", Math.Round ( 5.5 ) );
        Console.WriteLine ( "Round of -5.5 is {0}", Math.Round ( -5.5 ) );

        Console.WriteLine ( "Trunc of 5.5 is {0}", Math.Truncate ( 5.5 ) );
        Console.WriteLine ( "Trunc of -5.5 is {0}", Math.Truncate ( -5.5 ) );

        // This will NOT crash in C# (TryParse returns false for invalid input)
        Console.Write ( "Type Hello World! and press enter." );
        userInput = Console.ReadLine ();
        intParseSuccess = int.TryParse ( userInput, out enteredInteger );
        Console.WriteLine ( "The user entered the integer {0} (parse succeeded: {1})", enteredInteger, intParseSuccess );

        // This will NOT crash in C# (TryParse returns false for invalid input)
        Console.Write ( "Type abc123 and press enter." );
        userInput = Console.ReadLine ();
        intParseSuccess = int.TryParse ( userInput, out enteredInteger );
        Console.WriteLine ( "The user entered the integer {0} (parse succeeded: {1})", enteredInteger, intParseSuccess );
    }
}
