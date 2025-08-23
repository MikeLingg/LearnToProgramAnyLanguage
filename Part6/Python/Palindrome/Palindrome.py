def main():
    # Use nested conditions to test a palindrome up to a set length
    testString = "radar"
    isAPalindrome = False
    leftCharIndex = 0
    rightCharIndex = len( testString ) - 1
    
    if ( testString[leftCharIndex] == testString[rightCharIndex] ):
        leftCharIndex = leftCharIndex + 1
        rightCharIndex = rightCharIndex - 1
        if ( testString[leftCharIndex] == testString[rightCharIndex] ):
            leftCharIndex = leftCharIndex + 1
            rightCharIndex = rightCharIndex - 1
            if ( testString[leftCharIndex] == testString[rightCharIndex] ):
                isAPalindrome = True
    
    print( f"Input string is a palindrome: {isAPalindrome}" )
    
    # Guarded conditions version with length protections
    testString = "radar"
    notAPalindrome = False
    leftCharIndex = 0
    rightCharIndex = len( testString ) - 1
    
    if ( leftCharIndex < len( testString ) ):
        if ( testString[leftCharIndex] != testString[rightCharIndex] ):
            notAPalindrome = True
    
    if ( leftCharIndex < len( testString ) ):
        if ( notAPalindrome != True ):
            leftCharIndex = leftCharIndex + 1
            rightCharIndex = rightCharIndex - 1
            if ( testString[leftCharIndex] != testString[rightCharIndex] ):
                notAPalindrome = True
    
    if ( leftCharIndex < len( testString ) ):
        if ( notAPalindrome != True ):
            leftCharIndex = leftCharIndex + 1
            rightCharIndex = rightCharIndex - 1
            if ( testString[leftCharIndex] != testString[rightCharIndex] ):
                notAPalindrome = True
    
    print( f"Input string is a palindrome: {not notAPalindrome}" )

if __name__ == "__main__":
    main()
