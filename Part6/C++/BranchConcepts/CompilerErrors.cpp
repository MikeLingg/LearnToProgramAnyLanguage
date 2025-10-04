#include <iostream>
#include <string>
using namespace std;

int main()
{
    int score = 85;
    bool isValid = true;
    int value = 5;
    
    // ERROR 1: Missing parentheses around condition
    if score >= 90
    {
        printf( "You got an A\n" );
    }
    
    // ERROR 2: Using 'then' keyword (from other languages like Python, BASIC)
    if ( score >= 90 ) then
    {
        printf( "You got an A\n" );
    }
    
    // ERROR 3: Using 'elif' instead of 'else if' (Python influence)
    if ( score >= 90 )
    {
        printf( "A\n" );
    }
    elif ( score >= 80 )
    {
        printf( "B\n" );
    }
    
    // ERROR 4: Using 'elsif' instead of 'else if' (Ruby/Perl influence)
    if ( score >= 90 )
    {
        printf( "A\n" );
    }
    elsif ( score >= 80 )
    {
        printf( "B\n" );
    }
    
    // ERROR 5: Missing colon after case in switch
    switch ( value )
    {
        case 1
            printf( "One\n" );
            break;
    }
    
    // ERROR 6: Using equals sign instead of colon in case
    switch ( value )
    {
        case = 1:
            printf( "One\n" );
            break;
    }
    
    // ERROR 7: Using string in switch
    string text = "hello";
    switch ( text )
    {
        case "hello":
            printf( "Hello\n" );
            break;
    }
    
    // ERROR 8: Using float/double in switch
    float temp = 98.6;
    switch ( temp )
    {
        case 98.6:
            printf( "Normal\n" );
            break;
    }
    
    // ERROR 9: Multiple default cases in switch
    switch ( value )
    {
        case 1:
            printf( "One\n" );
            break;
        default:
            printf( "First default\n" );
            break;
        default:
            printf( "Second default\n" );
            break;
    }
    
    // ERROR 10: Duplicate case values in switch
    switch ( value )
    {
        case 1:
            printf( "First one\n" );
            break;
        case 1:
            printf( "Second one\n" );
            break;
    }
    
    // ERROR 11: Wrong comparison operator (=> instead of >=)
    if ( score => 90 )
    {
        printf( "A\n" );
    }
    
    // ERROR 12: Wrong comparison operator (=< instead of <=)
    if ( score =< 90 )
    {
        printf( "Too low\n" );
    }
    
    // ERROR 13: Variable declaration in case without braces (crosses initialization)
    switch ( value )
    {
        case 1:
            int result = 10;
            printf( "Result: %d\n", result );
            break;
        case 2:
            result = 20;
            break;
    }
    
    // ERROR 14: Using if without condition
    if
    {
        printf( "Hello\n" );
    }
    
    // ERROR 15: Case outside of switch
    case 1:
        printf( "One\n" );
        break;
    
    // ERROR 16: Default outside of switch
    default:
        printf( "Default\n" );
        break;
    
    // ERROR 17: Break outside of switch or loop
    if ( score >= 90 )
    {
        printf( "A\n" );
        break;
    }

    return 0;
}
