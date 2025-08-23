#include <iostream>
#include <string>
#include <cstdio>

using namespace std;

int main()
{
    // Use nested conditions to test a palindrome up to a set length
    string testString = "radar";
    bool isAPalindrome = false;
    unsigned leftCharIndex = 0;
    unsigned rightCharIndex = testString.length() - 1;
    
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
    testString = "radar";
    bool notAPalindrome = false;
    leftCharIndex = 0;
    rightCharIndex = testString.length() - 1;
    
    if ( leftCharIndex < testString.length() )
    {
        if ( testString[leftCharIndex] != testString[rightCharIndex] )
        {
            notAPalindrome = true;
        }
    }
    
    if ( leftCharIndex < testString.length() )
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
    
    if ( leftCharIndex < testString.length() )
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