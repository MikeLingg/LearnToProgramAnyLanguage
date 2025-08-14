#include <stdio.h>
#include <stdlib.h>

int main()
{
    char userInput[ 100 ];
    
    printf( "Type 1 and press enter.\n" );
    fgets( userInput, sizeof( userInput ), stdin );
    
    // Assignment operator placed incorrectly in declaration
    int enteredInteger atoi = ( userInput );
    
    printf( "The user entered the integer %d\n", enteredInteger );
    
    return 0;
}
