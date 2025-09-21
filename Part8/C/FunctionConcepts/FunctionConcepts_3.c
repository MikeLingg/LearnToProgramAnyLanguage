// This example will not compile as we cannot call a variable like a function
#include <stdio.h>

void myFunction ()
{
    printf ( "Called myFunction\n" );
}

int main ()
{
    int myVariable = 5;
    myVariable ();
    return 0;
}
