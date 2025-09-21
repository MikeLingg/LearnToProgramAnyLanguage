// Example of function overloading
// C does not support function overloading, so this will not compile
#include <stdio.h>

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
