#include <cstdio>
#include <climits>
#include <cfloat>

int main()
{
    // Do not mix your types in many languages
    int myInt = 123.45;
    float myFloat = 'a';
    char myChar = 543.21;
    
    printf ( "myInt: %d\n", myInt );
    printf ( "myFloat: %f\n", myFloat );
    printf ( "myChar: %c\n", myChar );
    
    return 0;
}
