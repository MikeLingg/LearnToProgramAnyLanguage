#include <stdio.h>
#include <stdlib.h>

int main()
{
    char userInput[ 100 ];
    int enteredInteger;
    
    printf( "Type 1 and press enter.\n" );
    fgets( userInput, sizeof( userInput ), stdin );
    
    // Function called but return value ignored
    atoi( userInput );
    
    // enteredInteger was never assigned a value
    printf( "The user entered the integer %d\n", enteredInteger );
    
    return 0;
}
