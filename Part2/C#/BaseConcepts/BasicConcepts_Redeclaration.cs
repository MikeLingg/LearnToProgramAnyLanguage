using System;

class Program
{
    static void Main()
    {
        // Do not redeclare variable names in most languages:
        char duplicateCharacter = 'a';
        // This will cause compile error - variable already declared
        char duplicateCharacter = 'b';

        Console.WriteLine("duplicateCharacter: {0}", duplicateCharacter);
    }
}
