program function_ignored_return
    implicit none
    character( len=100 ) :: user_input
    integer :: entered_integer, ios
    
    write( *, * ) 'Type 1 and press enter.'
    read( *, '(A)' ) user_input
    
    ! Function called but return value ignored
    read( user_input, *, iostat=ios )
    
    write( *, * ) 'The user entered the integer ', entered_integer
    
end program function_ignored_return
