// Example of function overloading
#include <iostream>
#include <cstdio>
using namespace std;

void myFunction ( int IntParameter_Par )
{
    printf ( "Int version of my function called %d\n", IntParameter_Par );
}

void myFunction ( double DoubleParameter_Par )
{
    printf ( "Double version of my function called %.1f\n", DoubleParameter_Par );
}

int main ()
{
    myFunction ( 5 );
    myFunction ( 5.5 );
    return 0;
}
