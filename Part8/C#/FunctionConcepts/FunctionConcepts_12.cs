// Computing factorial with loop
using System;

class Program
{
    static int Factorial ( int FactorialNumber_Par )
    {
        int totalFactorial = 1;
        
        for ( int factorialNumber = 1; factorialNumber <= FactorialNumber_Par; factorialNumber++ )
        {
            totalFactorial = totalFactorial * factorialNumber;
        }
        
        return totalFactorial;
    }

    static void Main ()
    {
        int factorialResult = Factorial ( FactorialNumber_Par: 10 );
        Console.WriteLine ( "Factorial of 10 is: {0}", factorialResult );
    }
}
