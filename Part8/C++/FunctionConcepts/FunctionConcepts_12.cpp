// Computing factorial with loop
#include <iostream>
#include <cstdio>
using namespace std;

int factorial ( int FactorialNumber_Par )
{
    int totalFactorial = 1;
    
    for ( int factorialNumber = 1; factorialNumber <= FactorialNumber_Par; factorialNumber++ )
    {
        totalFactorial = totalFactorial * factorialNumber;
    }
    
    return totalFactorial;
}

int main ()
{
    int factorialResult = factorial ( 10 );
    printf ( "Factorial of 10 is: %d\n", factorialResult );
    return 0;
}
