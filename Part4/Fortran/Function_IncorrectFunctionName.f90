program function_incorrect_function_name
    implicit none
    character( len=100 ) :: user_input
    integer :: entered_integer
    
    write( *, * ) 'Type 1 and press enter.'
    read( *, '(A)' ) user_input
    
    ! Function name is wrong - should use internal read
    entered_integer = string_to_number( user_input )
    
    write( *, * ) 'The user entered the integer ', entered_integer
    
end program function_incorrect_function_name
