using System;

class Program
{
    const string COLOR_RESET = "\x1b[0m";
    const string COLOR_BOLD = "\x1b[1m";

    static void PrintBgRGB(byte r, byte g, byte b, string text)
    {
        Console.Write($"\x1b[48;2;{r};{g};{b}m{text}{COLOR_RESET}");
    }

    static (byte, byte, byte) ValueToColor(double normalized)
    {
        byte r, g, b;
        if (normalized < 0.5)
        {
            // Blue to Green
            double t = normalized * 2.0;
            r = 0;
            g = (byte)(t * 255);
            b = (byte)((1.0 - t) * 255);
        }
        else
        {
            // Green to Red
            double t = (normalized - 0.5) * 2.0;
            r = (byte)(t * 255);
            g = (byte)((1.0 - t) * 255);
            b = 0;
        }
        return (r, g, b);
    }

    static void Demonstrate8Bit()
    {
        Console.WriteLine($"\n{COLOR_BOLD}=== 8-BIT UNSIGNED (0-255) ==={COLOR_RESET}");
        Console.WriteLine("256 possible values - Coarse granularity\n");

        ushort[] values = { 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 127, 255 };

        foreach (var val in values)
        {
            double normalized = val / 255.0;
            var (r, g, b) = ValueToColor(normalized);

            Console.Write($"  {val,3}: ");
            PrintBgRGB(r, g, b, "    ");
            Console.WriteLine($" RGB({r,3},{g,3},{b,3})");
        }
    }

    static void Demonstrate16Bit()
    {
        Console.WriteLine($"\n{COLOR_BOLD}=== 16-BIT UNSIGNED (0-65535) ==={COLOR_RESET}");
        Console.WriteLine("65,536 possible values - 256x finer than 8-bit\n");

        uint[] values = { 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 3000, 6000, 9000, 12000, 15000, 18000, 21000, 24000, 27000, 32767, 65535 };

        foreach (var val in values)
        {
            double normalized = val / 65535.0;
            var (r, g, b) = ValueToColor(normalized);

            Console.Write($"  {val,5}: ");
            PrintBgRGB(r, g, b, "    ");
            Console.WriteLine($" RGB({r,3},{g,3},{b,3})");
        }
    }

    static void Demonstrate32Bit()
    {
        Console.WriteLine($"\n{COLOR_BOLD}=== 32-BIT UNSIGNED (0-4294967295) ==={COLOR_RESET}");
        Console.WriteLine("4,294,967,296 possible values - 65,536x finer than 16-bit\n");

        ulong[] values = { 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 250000000, 500000000, 1000000000, 1500000000, 2000000000,
                          2147483647, 4294967295 };

        foreach (var val in values)
        {
            double normalized = val / 4294967295.0;
            var (r, g, b) = ValueToColor(normalized);

            Console.Write($"  {val,10}: ");
            PrintBgRGB(r, g, b, "    ");
            Console.WriteLine($" RGB({r,3},{g,3},{b,3})");
        }
    }

    static void Demonstrate64Bit()
    {
        Console.WriteLine($"\n{COLOR_BOLD}=== 64-BIT UNSIGNED (0-18446744073709551615) ==={COLOR_RESET}");
        Console.WriteLine("18,446,744,073,709,551,616 possible values - 4,294,967,296x finer than 32-bit\n");

        ulong[] values = { 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 625000000000000000, 1250000000000000000, 2500000000000000000, 5000000000000000000,
                          9223372036854775807, 18446744073709551615 };

        foreach (var val in values)
        {
            double normalized = val / 18446744073709551615.0;
            var (r, g, b) = ValueToColor(normalized);

            Console.Write($"  {val,20}: ");
            PrintBgRGB(r, g, b, "    ");
            Console.WriteLine($" RGB({r,3},{g,3},{b,3})");
        }
    }

    static void Main()
    {
        // This signifies a number of basic concepts that would be included in one program, 
        // except some of these concepts will prevent compilation or crash the program. 
        // So this program will be broken up as specific languages require it.

        bool falseBoolean = false;
        bool trueBoolean = true;

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
        
        Demonstrate8Bit();
        Demonstrate16Bit();
        Demonstrate32Bit();
        Demonstrate64Bit();
        
        float floatMax = float.MaxValue;
        float floatMin = float.Epsilon;
        
        Console.WriteLine ( "Note that scientific notation must be used to print such a small number." );
        Console.WriteLine ( "32 bit float: {0:E} {1}", floatMin, floatMax );
        
        float zeroPointOne = 0.1f;
        float zeroPointTwo = 0.2f;
        float zeroPointThree = 0.3f;
        
        // So let's look at how far off the actual floating point value is from the value it was set to.
        Console.WriteLine ( "Floating point 0.1, 0.2, 0.3 -> {0:G17} and {1:G17} and {2:G17}", 
                           zeroPointOne, zeroPointTwo, zeroPointThree );
        
        double doubleMax = double.MaxValue;
        double doubleMin = double.Epsilon;
        
        Console.WriteLine ( "Note that scientific notation must be used to print such a small number." );
        Console.WriteLine ( "64 bit float range: {0:E} {1}", doubleMin, doubleMax );
        
        // Shows the basics of ASCII characters, including special ones.
        // This should print 1 followed by a tab followed by 2, then on the next line print 3.
        char charOne = '1';
        char charTab = '\t';
        char singleQuotes = '\'';
        char charNewLine = '\n';
        char doubleQuotes = '\"';
        Console.WriteLine ( "Characters: {0}{1}{2}{3}{4}", charOne, charTab, singleQuotes, charNewLine, doubleQuotes );
        
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
        float outOfRangeFloat = float.MaxValue + float.MaxValue; //float.PositiveInfinity;
        double outOfRangeDouble = -double.MaxValue - double.MaxValue;
        
        Console.WriteLine ( "Out of range float and double: {0} {1}", outOfRangeFloat, outOfRangeDouble );

        // C# char is 2 bytes (UTF-16) and can handle Unicode
        char outOfRangeChar = (char)257;

        Console.WriteLine ( "Out of range char: {0}", outOfRangeChar );
    }
}
