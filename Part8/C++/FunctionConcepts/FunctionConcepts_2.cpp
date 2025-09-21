// An example of function variable scope
#include <iostream>
#include <cstdio>
using namespace std;

int GlobalVariable = 15;
int GlobalToBeShadowed = 5;

void myFunction ()
{
    int myVariable = 55;
    GlobalVariable = 42;
    int globalToBeShadowed = 15;
}

int main ()
{
    printf ( "Global variable: %d\n", GlobalVariable );
    printf ( "Global shadowed: %d\n", GlobalToBeShadowed );
    myFunction ();
    printf ( "Function variable: %d\n", myVariable );
    printf ( "Global variable: %d\n", GlobalVariable );
    printf ( "Global shadowed: %d\n", GlobalToBeShadowed );
    return 0;
}
