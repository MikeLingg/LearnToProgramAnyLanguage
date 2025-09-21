// Infinite recursive function
#include <iostream>
#include <cstdio>
using namespace std;

void recursiveFunction ()
{
    recursiveFunction ();
}

int main ()
{
    recursiveFunction ();
    return 0;
}
