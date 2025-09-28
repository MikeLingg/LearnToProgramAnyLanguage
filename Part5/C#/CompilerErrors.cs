using System;

class Program
{
    static void Main()
    {
        // Variable declarations
        int a = 5;
        int b = 10;
        double c = 3.14;
        string stringOne = "Hello";
        bool flag = true;
        int result;

        // ERROR: Missing semicolon after expression statement
        result = a + b

        // ERROR: Using assignment operator instead of comparison
        flag = (a = b);

        // ERROR: Cannot assign to literal/constant
        5 = a;

        // ERROR: Type mismatch - cannot implicitly convert double to int
        result = c;

        // ERROR: Cannot convert string to int
        result = stringOne;

        // ERROR: Using uninitialized variable
        int uninitialized;
        result = uninitialized;

        // ERROR: Wrong string comparison method for value types
        flag = (a.Equals("5"));

        // ERROR: Cannot modify readonly field outside constructor
        readonly int readonlyField = 10;
        readonlyField = 20;

        // ERROR: Division by literal zero
        result = a / 0;

        // ERROR: Cannot use void method in expression
        result = Console.WriteLine("Hello");

        // ERROR: Wrong format in string interpolation
        string badFormat = $"Value: {a:X2.2}";

        // ERROR: Cannot assign null to non-nullable value type
        result = null;

        // ERROR: Using logical operators on non-boolean types
        result = a && b;

        // ERROR: Cannot convert method group to non-delegate type
        result = Console.WriteLine;

        // ERROR: Using bitwise operator instead of logical in boolean context
        flag = (a & b) > 0;

        // ERROR: Attempting to use out parameter incorrectly
        int.TryParse("123", result);

        // ERROR: Cannot use ref/out with property
        string temp = "test";
        ref string refString = ref temp.Length;

        // ERROR: Missing cast for explicit conversion
        result = (int)c + 1.5;

        // ERROR: Using == with delegate types incorrectly
        Action action1 = () => Console.WriteLine("Hello");
        Action action2 = () => Console.WriteLine("Hello");
        flag = (action1 == action2);

        // ERROR: Cannot assign to expression
        (a + b) = 10;

        // ERROR: Using var without initialization
        var uninitializedVar;

        // ERROR: Cannot convert bool to int implicitly
        result = flag;

        // ERROR: Operator precedence confusion with null-coalescing
        string nullableString = null;
        result = nullableString?.Length ?? 0 + 5;

        Console.WriteLine("End of program");
    }
}
