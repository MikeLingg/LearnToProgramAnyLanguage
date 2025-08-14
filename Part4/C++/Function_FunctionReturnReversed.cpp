#include <stdio.h>
#include <stdlib.h>

int main()
{
    char userInput[ 100 ];
    int enteredInteger;
    
    printf( "Type 1 and press enter.\n" );
    fgets( userInput, sizeof( userInput ), stdin );
    
    // Trying to assign to function name instead of calling function
    atoi = enteredInteger( userInput );
    
    printf( "The user entered the integer %d\n", enteredInteger );
    
    return 0;
}
