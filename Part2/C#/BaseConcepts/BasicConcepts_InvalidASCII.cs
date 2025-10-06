using System;

class Program
{
    static void Main()
    {
        // So the ASCII table shows the tab symbol as TAB, but this doesn't work in programming.
        // I think in some programs this will crash, so it will be in a separate program.
        // C# doesn't allow multi-character literals - this will cause compile error
        char charInvalid = 'TAB';
        Console.WriteLine ( "Invalid char: {0}", charInvalid );
    }
}
