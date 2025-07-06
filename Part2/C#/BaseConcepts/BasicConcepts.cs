using System;

class Program
{
    static void Main()
    {
        // This signifies a number of basic concepts that would be included in one program, 
        // except some of these concepts will prevent compilation or crash the program. 
        // So this program will be broken up as specific languages require it.

        bool falseBoolean = true;
        bool trueBoolean = false;
        
        Console.WriteLine ( "Boolean range: {0} {1}", falseBoolean, trueBoolean );
        
        sbyte minSigned8 = -128;
        sbyte maxSigned8 = 127;
        byte minUnsigned8 = 0;
        byte maxUnsigned8 = 255;
        
        Console.WriteLine ( "8 bit signed int range: {0} {1}", minSigned8, maxSigned8 );
        Console.WriteLine ( "8 bit unsigned int range: {0} {1}", minUnsigned8, maxUnsigned8 );
        
        short minSigned16 = -32768;
        short maxSigned16 = 32767;
        ushort minUnsigned16 = 0;
        ushort maxUnsigned16 = 65535;
        
        Console.WriteLine ( "16 bit signed int range: {0} {1}", minSigned16, maxSigned16 );
        Console.WriteLine ( "16 bit unsigned int range: {0} {1}", minUnsigned16, maxUnsigned16 );
        
        int minSigned32 = -2147483648;
        int maxSigned32 = 2147483647;
        uint minUnsigned32 = 0;
        uint maxUnsigned32 = 4294967295;
        
        Console.WriteLine ( "32 bit signed int range: {0} {1}", minSigned32, maxSigned32 );
        Console.WriteLine ( "32 bit unsigned int range: {0} {1}", minUnsigned32, maxUnsigned32 );
        
        Console.WriteLine ( "Note: C# handles large integers within specified bit ranges." );
        long minSigned64 = -9223372036854775808;
        long maxSigned64 = 9223372036854775807;
        ulong minUnsigned64 = 0;
        ulong maxUnsigned64 = 18446744073709551615;
        
        Console.WriteLine ( "64 bit signed int range: {0} {1}", minSigned64, maxSigned64 );
        Console.WriteLine ( "64 bit unsigned int range: {0} {1}", minUnsigned64, maxUnsigned64 );
        
        float floatMax = float.MaxValue;
        float floatMin = float.Epsilon;
        
        Console.WriteLine ( "Note that scientific notation must be used to print such a small number." );
        Console.WriteLine ( "32 bit float: {0:E} {1}", floatMin, floatMax );
        
        float zeroPointOne = 0.1f;
        float zeroPointTwo = 0.2f;
        float zeroPointThree = 0.3f;
        
        // So let's look at how far off the actual floating point value is from the value it was set to.
        Console.WriteLine ( "Floating point 0.1, 0.2, 0.3 -> {0:F17} and {1:F17} and {2:F17}", 
                           zeroPointOne, zeroPointTwo, zeroPointThree );
        
        double doubleMax = double.MaxValue;
        double doubleMin = double.Epsilon;
        
        Console.WriteLine ( "Note that scientific notation must be used to print such a small number." );
        Console.WriteLine ( "64 bit float range: {0:E} {1}", doubleMin, doubleMax );
        
        // Shows the basics of ASCII characters, including special ones.
        // This should print 1 followed by a tab followed by 2, then on the next line print 3.
        char charOne = '1';
        char charTab = '\t';
        char charTwo = '2';
        char charNewLine = '\n';
        char charThree = '3';
        Console.WriteLine ( "Characters: {0}{1}{2}{3}{4}", charOne, charTab, charTwo, charNewLine, charThree );
        
        // Show how printing as an integer, not a character, can be confusing
        Console.WriteLine ( "charOne as an integer: {0}", (int)charOne );
        
        // Note some languages may not compile this code so it will be in a separate file, others may throw warnings.
        // C# doesn't allow implicit conversion from int to bool
        bool outOfRangeBoolean = true;
        
        Console.WriteLine ( "Out of range Boolean: {0}", outOfRangeBoolean );
        
        // Note some languages may not compile this code so it will be in a separate file, others may throw warnings.
        // C# will handle overflow differently based on checked/unchecked context
        short outOfRange = 32767;
        
        Console.WriteLine ( "Out of range value: {0}", outOfRange );
        
        Console.WriteLine ( "Note that adding a small amount to float max is lost in the precision, so using infinity." );
        float outOfRangeFloat = float.PositiveInfinity;
        double outOfRangeDouble = double.PositiveInfinity;
        
        Console.WriteLine ( "Out of range float and double: {0} {1}", outOfRangeFloat, outOfRangeDouble );

        // C# char is 2 bytes (UTF-16) and can handle Unicode
        char outOfRangeChar = (char)257;

        Console.WriteLine ( "Out of range char: {0}", outOfRangeChar );
    }
}
