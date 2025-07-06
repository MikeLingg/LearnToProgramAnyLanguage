using System;

class Program
{
    static void Main()
    {
        // Do not mix your types in many languages
        // C# is strongly typed but allows some implicit conversions
        // These will cause compile errors due to incompatible types
        int myInt = 123.45;
        float myFloat = 'a';
        char myChar = 543.21;

        Console.WriteLine("myInt: {0}", myInt);
        Console.WriteLine("myFloat: {0}", myFloat);
        Console.WriteLine("myChar: {0}", myChar);
    }
}
