// Recursive function call to compute factorial
#include <iostream>
#include <cstdio>
using namespace std;

int factorial ( int FactorialNumber_Par )
{
    if ( FactorialNumber_Par <= 1 )
    {
        return 1;
    }
    
    return FactorialNumber_Par * factorial ( FactorialNumber_Par - 1 );
}

int main ()
{
    int factorialResult = factorial ( 10 );
    printf ( "Factorial of 10 is: %d\n", factorialResult );
    return 0;
}
