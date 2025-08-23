#include <stdio.h>
#include <string.h>
#include <stdbool.h>

int main()
{
    // Use nested conditions to test a palindrome up to a set length
    char testString[] = "radar";
    bool isAPalindrome = false;
    unsigned leftCharIndex = 0;
    unsigned rightCharIndex = strlen( testString ) - 1;
    
    if ( testString[leftCharIndex] == testString[rightCharIndex] )
    {
        leftCharIndex = leftCharIndex + 1;
        rightCharIndex = rightCharIndex - 1;
        if ( testString[leftCharIndex] == testString[rightCharIndex] )
        {
            leftCharIndex = leftCharIndex + 1;
            rightCharIndex = rightCharIndex - 1;
            if ( testString[leftCharIndex] == testString[rightCharIndex] )
            {
                isAPalindrome = true;
            }
        }
    }
    
    printf( "Input string is a palindrome: %s\n", isAPalindrome ? "true" : "false" );
    
    // Guarded conditions version with length protections
    strcpy( testString, "radar" );
    bool notAPalindrome = false;
    leftCharIndex = 0;
    rightCharIndex = strlen( testString ) - 1;
    
    if ( leftCharIndex < strlen( testString ) )
    {
        if ( testString[leftCharIndex] != testString[rightCharIndex] )
        {
            notAPalindrome = true;
        }
    }
    
    if ( leftCharIndex < strlen( testString ) )
    {
        if ( notAPalindrome != true )
        {
            leftCharIndex = leftCharIndex + 1;
            rightCharIndex = rightCharIndex - 1;
            if ( testString[leftCharIndex] != testString[rightCharIndex] )
            {
                notAPalindrome = true;
            }
        }
    }
    
    if ( leftCharIndex < strlen( testString ) )
    {
        if ( notAPalindrome != true )
        {
            leftCharIndex = leftCharIndex + 1;
            rightCharIndex = rightCharIndex - 1;
            if ( testString[leftCharIndex] != testString[rightCharIndex] )
            {
                notAPalindrome = true;
            }
        }
    }
    
    printf( "Input string is a palindrome: %s\n", (!notAPalindrome) ? "true" : "false" );
    
    return 0;
}
