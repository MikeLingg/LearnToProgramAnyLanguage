// This example will not compile as we cannot call a variable like a function
using System;

class Program
{
    static void MyFunction ()
    {
        Console.WriteLine ( "Called MyFunction" );
    }

    static void Main ()
    {
        int myVariable = 5;
        myVariable ();
    }
}
