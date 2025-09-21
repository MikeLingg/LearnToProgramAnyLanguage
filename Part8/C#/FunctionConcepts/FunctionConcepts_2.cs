// An example of function variable scope
using System;

class Program
{
    static int GlobalVariable = 15;
    static int GlobalToBeShadowed = 5;

    static void MyFunction ()
    {
        int myVariable = 55;
        GlobalVariable = 42;
        int GlobalToBeShadowed = 15;
    }

    static void Main ()
    {
        Console.WriteLine ( "Global variable: {0}", GlobalVariable );
        Console.WriteLine ( "Global shadowed: {0}", GlobalToBeShadowed );
        MyFunction ();
        Console.WriteLine ( "Function variable: {0}", myVariable );
        Console.WriteLine ( "Global variable: {0}", GlobalVariable );
        Console.WriteLine ( "Global shadowed: {0}", GlobalToBeShadowed );
    }
}
