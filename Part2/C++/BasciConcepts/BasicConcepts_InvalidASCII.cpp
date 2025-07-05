#include <cstdio>
#include <climits>
#include <cfloat>

int main()
{
    // So the ASCII table shows the tab symbol as TAB, but this doesn't work in programming.
    // I think in some programs this will crash, so it will be in a separate program.
    char charInvalid = 'TAB';
    printf ( "Invalid char: %c\n", charInvalid ) ;

    return 0;
}
