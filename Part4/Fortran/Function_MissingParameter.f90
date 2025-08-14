program function_missing_parameter
    implicit none
    character( len=100 ) :: user_input
    integer :: entered_integer, ios
    
    write( *, * ) 'Type 1 and press enter.'
    read( *, '(A)' ) user_input
    
    ! Internal read statement missing the input variable
    read( user_input, *, iostat=ios )
    
    write( *, * ) 'The user entered the integer ', entered_integer
    
end program function_missing_parameter
