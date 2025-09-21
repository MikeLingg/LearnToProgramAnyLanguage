// Main function with parameters to access command line arguments
#include <stdio.h>

int main ( int argc, char* argv[] )
{
    printf ( "Number of arguments: %d\n", argc );
    printf ( "Arguments:\n" );
    for ( int i = 0; i < argc; i++ )
    {
        printf ( "\tArgument %d: %s\n", i, argv[i] );
    }
    return 0;
}
