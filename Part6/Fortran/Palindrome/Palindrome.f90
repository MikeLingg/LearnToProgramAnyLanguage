program palindrome_check
    implicit none
    
    character(len=5) :: testString
    logical :: isAPalindrome, notAPalindrome
    integer :: leftCharIndex, rightCharIndex
    
    ! Use nested conditions to test a palindrome up to a set length
    testString = 'radar'
    isAPalindrome = .false.
    leftCharIndex = 1
    rightCharIndex = len_trim( testString )
    
    if ( testString( leftCharIndex:leftCharIndex ) == &
         testString( rightCharIndex:rightCharIndex ) ) then
        leftCharIndex = leftCharIndex + 1
        rightCharIndex = rightCharIndex - 1
        if ( testString( leftCharIndex:leftCharIndex ) == &
             testString( rightCharIndex:rightCharIndex ) ) then
            leftCharIndex = leftCharIndex + 1
            rightCharIndex = rightCharIndex - 1
            if ( testString( leftCharIndex:leftCharIndex ) == &
                 testString( rightCharIndex:rightCharIndex ) ) then
                isAPalindrome = .true.
            end if
        end if
    end if
    
    write( *, '(A,L1)' ) 'Input string is a palindrome: ', isAPalindrome
    
    ! Guarded conditions version with length protections
    testString = 'radar'
    notAPalindrome = .false.
    leftCharIndex = 1
    rightCharIndex = len_trim( testString )
    
    if ( leftCharIndex <= len_trim( testString ) ) then
        if ( testString( leftCharIndex:leftCharIndex ) /= &
             testString( rightCharIndex:rightCharIndex ) ) then
            notAPalindrome = .true.
        end if
    end if
    
    if ( leftCharIndex <= len_trim( testString ) ) then
        if ( notAPalindrome .neqv. .true. ) then
            leftCharIndex = leftCharIndex + 1
            rightCharIndex = rightCharIndex - 1
            if ( testString( leftCharIndex:leftCharIndex ) /= &
                 testString( rightCharIndex:rightCharIndex ) ) then
                notAPalindrome = .true.
            end if
        end if
    end if
    
    if ( leftCharIndex <= len_trim( testString ) ) then
        if ( notAPalindrome .neqv. .true. ) then
            leftCharIndex = leftCharIndex + 1
            rightCharIndex = rightCharIndex - 1
            if ( testString( leftCharIndex:leftCharIndex ) /= &
                 testString( rightCharIndex:rightCharIndex ) ) then
                notAPalindrome = .true.
            end if
        end if
    end if
    
    write( *, '(A,L1)' ) 'Input string is a palindrome: ', .not. notAPalindrome
    
end program palindrome_check
