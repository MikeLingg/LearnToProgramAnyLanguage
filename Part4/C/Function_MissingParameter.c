#include <stdio.h>
#include <stdlib.h>

int main()
{
    char userInput[ 100 ];
    
    printf( "Type 1 and press enter.\n" );
    fgets( userInput, sizeof( userInput ), stdin );
    
    // Function requires 1 parameter, but we're passing 0
    int enteredInteger = atoi();
    
    printf( "The user entered the integer %d\n", enteredInteger );
    
    return 0;
}
