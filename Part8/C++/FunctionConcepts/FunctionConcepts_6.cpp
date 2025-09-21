// Example with three default parameters
#include <iostream>
#include <cstdio>
using namespace std;

void threeDefaultParameters ( int FirstParameter_Par = 5, int SecondParameter_Par = 10, int ThirdParameter_Par = 15 )
{
    printf ( "Parameters: %d %d %d\n", FirstParameter_Par, SecondParameter_Par, ThirdParameter_Par );
}

int main ()
{
    threeDefaultParameters ( 20, 25, 30 );
    // Some languages *do not* have named parameters so can set the first parameter alone, not the second.
    threeDefaultParameters ( 25 );
    return 0;
}
