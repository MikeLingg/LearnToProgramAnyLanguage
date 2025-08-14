program function_extra_parameter
    implicit none
    character( len=100 ) :: user_input
    integer :: entered_integer, extra_input, ios
    
    extra_input = 5
    
    write( *, * ) 'Type 1 and press enter.'
    read( *, '(A)' ) user_input
    
    ! Internal read statement with too many parameters
    read( user_input, *, iostat=ios, extra_input ) entered_integer
    
    write( *, * ) 'The user entered the integer ', entered_integer
    
end program function_extra_parameter
