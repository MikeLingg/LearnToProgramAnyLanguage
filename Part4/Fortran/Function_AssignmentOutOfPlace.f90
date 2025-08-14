program function_assignment_wrong_place
    implicit none
    character( len=100 ) :: user_input
    integer :: entered_integer, ios
    
    write( *, * ) 'Type 1 and press enter.'
    read( *, '(A)' ) user_input
    
    ! Assignment operator placed incorrectly in statement
    integer :: entered_integer read = ( user_input, *, iostat=ios ) entered_integer
    
    write( *, * ) 'The user entered the integer ', entered_integer
    
end program function_assignment_wrong_place
