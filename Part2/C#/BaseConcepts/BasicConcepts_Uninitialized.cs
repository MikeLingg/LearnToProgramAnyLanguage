using System;

class Program
{
    static void Main()
    {
        // I think in some programs this will cause a crash or failure to compile, so it will be in a separate program.
        // C# doesn't allow uninitialized local variables - this will cause compile error
        char myCharacter;
        Console.WriteLine("myCharacter: {0} as int: {1}", myCharacter, (int)myCharacter);
    }
}
