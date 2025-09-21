// Example of function overloading
using System;

class Program
{
    static void MyFunction ( int IntParameter_Par )
    {
        Console.WriteLine ( "Int version of my function called {0}", IntParameter_Par );
    }

    static void MyFunction ( double DoubleParameter_Par )
    {
        Console.WriteLine ( "Double version of my function called {0:F1}", DoubleParameter_Par );
    }

    static void Main ()
    {
        MyFunction ( IntParameter_Par: 5 );
        MyFunction ( DoubleParameter_Par: 5.5 );
    }
}
