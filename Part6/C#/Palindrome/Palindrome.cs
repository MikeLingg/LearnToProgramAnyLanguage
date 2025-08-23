using System;

class Program
{
    static void Main( string[] args )
    {
        // Use nested conditions to test a palindrome up to a set length
        string testString = "radar";
        bool isAPalindrome = false;
        int leftCharIndex = 0;
        int rightCharIndex = testString.Length - 1;
        
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
        
        Console.Write( $"Input string is a palindrome: {isAPalindrome}\n" );
        
        // Guarded conditions version with length protections
        testString = "radar";
        bool notAPalindrome = false;
        leftCharIndex = 0;
        rightCharIndex = testString.Length - 1;
        
        if ( leftCharIndex < testString.Length )
        {
            if ( testString[leftCharIndex] != testString[rightCharIndex] )
            {
                notAPalindrome = true;
            }
        }
        
        if ( leftCharIndex < testString.Length )
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
        
        if ( leftCharIndex < testString.Length )
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
        
        Console.Write( $"Input string is a palindrome: {!notAPalindrome}\n" );
    }
}
