// This example will not compile as we cannot call a variable like a function
#include <iostream>
#include <cstdio>
using namespace std;

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
