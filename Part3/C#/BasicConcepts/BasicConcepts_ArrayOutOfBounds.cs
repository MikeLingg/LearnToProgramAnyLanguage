using System;

class Program
{
    // Struct to demonstrate buffer overflow concepts (safe version)
    public struct TestStruct
    {
        public int[ ] intArray;
        public int myInt;
    }

    static void Main ()
    {
        // Create the test struct to demonstrate bounds checking
        TestStruct testStruct = new TestStruct ();
        testStruct.intArray = new int[ 10 ];  // Initialize the array
        testStruct.myInt = 0;

        // In C#, buffer overflows are prevented by bounds checking in safe code
        // This code demonstrates what would happen in unsafe code
        // The following would throw IndexOutOfRangeException in safe C#:
        testStruct.intArray[ 10 ] = 55;  // This will throw exception
        
        Console.WriteLine ( $"myInt value: { testStruct.myInt }" );

        testStruct.myInt = testStruct.myInt + 1;

        // This will also throw an exception since we're accessing out of bounds
        Console.WriteLine ( $"Out of bounds array value: { testStruct.intArray[ 10 ] }" );
    }
}
