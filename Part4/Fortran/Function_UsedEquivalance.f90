program function_equivalence_instead_assignment
    implicit none
    character( len=100 ) :: user_input
    integer :: entered_integer, ios
    
    write( *, * ) 'Type 1 and press enter.'
    read( *, '(A)' ) user_input
    
    ! Using == (comparison) instead of = (assignment)
    read( user_input, *, iostat=ios ) entered_integer == 42
    
    write( *, * ) 'The user entered the integer ', entered_integer
    
end program function_equivalence_instead_assignment
