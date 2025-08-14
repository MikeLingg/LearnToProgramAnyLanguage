#include <stdio.h>
#include <stdlib.h>

int main()
{
    int extraInput = 5;
    char userInput[ 100 ];
    
    printf( "Type 1 and press enter.\n" );
    fgets( userInput, sizeof( userInput ), stdin );
    
    // Function only takes 1 parameter, but we're passing 2
    int enteredInteger = atoi( userInput, extraInput );
    
    printf( "The user entered the integer %d\n", enteredInteger );
    
    return 0;
}
