// Example with three default parameters
// C does not support default parameters, so this demonstrates limitation
#include <stdio.h>

void threeDefaultParameters ( int FirstParameter_Par, int SecondParameter_Par, int ThirdParameter_Par )
{
    printf ( "Parameters: %d %d %d\n", FirstParameter_Par, SecondParameter_Par, ThirdParameter_Par );
}

int main ()
{
    threeDefaultParameters ( 20, 25, 30 );
    // C does not support default parameters, must provide all arguments
    threeDefaultParameters ( 5, 10, 15 );
    return 0;
}
