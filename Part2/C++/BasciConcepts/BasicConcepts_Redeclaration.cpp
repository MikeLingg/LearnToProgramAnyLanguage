#include <cstdio>
#include <climits>
#include <cfloat>

int main()
{
    // Do not redeclare variable names in most languages:
    char duplicateCharacter = 'a';
    char duplicateCharacter = 'b';
    
    printf ( "duplicateCharacter: %c\n", duplicateCharacter ) ;
    
    return 0;
}
