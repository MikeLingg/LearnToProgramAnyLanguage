program BranchErrors
    implicit none
    
    integer :: score, value
    logical :: isValid
    
    score = 85
    isValid = .true.
    value = 5
    
    ! ERROR 1: Missing 'then' keyword
    if ( score >= 90 )
        write( *, '(A)' ) 'You got an A'
    end if
    
    ! ERROR 2: Missing 'end if'
    if ( score >= 90 ) then
        write( *, '(A)' ) 'You got an A'
    
    ! ERROR 3: Using 'elif' instead of 'else if'
    if ( score >= 90 ) then
        write( *, '(A)' ) 'A'
    elif ( score >= 80 ) then
        write( *, '(A)' ) 'B'
    end if
    
    ! ERROR 4: Using 'elsif' instead of 'else if'
    if ( score >= 90 ) then
        write( *, '(A)' ) 'A'
    elsif ( score >= 80 ) then
        write( *, '(A)' ) 'B'
    end if
    
    ! ERROR 5: Using 'elseif' (one word) instead of 'else if'
    if ( score >= 90 ) then
        write( *, '(A)' ) 'A'
    elseif ( score >= 80 ) then
        write( *, '(A)' ) 'B'
    end if
    
    ! ERROR 6: Using 'switch' instead of 'select case'
    switch ( value )
        case ( 1 )
            write( *, '(A)' ) 'One'
        case default
            write( *, '(A)' ) 'Other'
    end select
    
    ! ERROR 7: Missing 'end select'
    select case ( value )
        case ( 1 )
            write( *, '(A)' ) 'One'
        case default
            write( *, '(A)' ) 'Other'
    
    ! ERROR 8: Using 'default' instead of 'case default'
    select case ( value )
        case ( 1 )
            write( *, '(A)' ) 'One'
        default
            write( *, '(A)' ) 'Other'
    end select
    
    ! ERROR 9: Duplicate case values
    select case ( value )
        case ( 1 )
            write( *, '(A)' ) 'First one'
        case ( 1 )
            write( *, '(A)' ) 'Second one'
        case default
            write( *, '(A)' ) 'Other'
    end select
    
    ! ERROR 10: Using 'break' (not valid in Fortran)
    select case ( value )
        case ( 1 )
            write( *, '(A)' ) 'One'
            break
        case default
            write( *, '(A)' ) 'Other'
    end select
    
    ! ERROR 11: case outside of select
    case ( 1 )
        write( *, '(A)' ) 'One'
    
    ! ERROR 12: else if without preceding if
    else if ( score >= 80 ) then
        write( *, '(A)' ) 'B'
    end if
    
    ! ERROR 13: else without preceding if
    else
        write( *, '(A)' ) 'Failed'
    end if

end program BranchErrors
