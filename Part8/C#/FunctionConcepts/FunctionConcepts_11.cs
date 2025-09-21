// Recursive function call to compute factorial
using System;

class Program
{
    static int Factorial ( int FactorialNumber_Par )
    {
        if ( FactorialNumber_Par <= 1 )
        {
            return 1;
        }
        
        return FactorialNumber_Par * Factorial ( FactorialNumber_Par - 1 );
    }

    static void Main ()
    {
        int factorialResult = Factorial ( FactorialNumber_Par: 10 );
        Console.WriteLine ( "Factorial of 10 is: {0}", factorialResult );
    }
}
