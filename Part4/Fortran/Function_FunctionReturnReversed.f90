program function_statement_variable_reversed
    implicit none
    character( len=100 ) :: user_input
    integer :: entered_integer, ios
    
    write( *, * ) 'Type 1 and press enter.'
    read( *, '(A)' ) user_input
    
    ! Trying to assign to statement instead of using statement properly
    entered_integer = read( user_input, *, iostat=ios )
    
    write( *, * ) 'The user entered the integer ', entered_integer
    
end program function_statement_variable_reversed
