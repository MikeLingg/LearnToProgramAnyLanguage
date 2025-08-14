program error2_wrong_capitalization
    implicit none
    character( len=100 ) :: user_input
    integer :: entered_integer, ios
    
    WRITE( *, * ) 'Type 1 and press enter.'
    READ( *, '(A)' ) user_input
    
    ! Using inconsistent case (this actually works in Fortran but shows style issue)
    entered_integer = INT_CONVERT( user_input )
    
    write( *, * ) 'The user entered the integer ', entered_integer
    
end program error2_wrong_capitalization
