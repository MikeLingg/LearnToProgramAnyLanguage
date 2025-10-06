program InvalidASCII
    implicit none
    
    ! So the ASCII table shows the tab symbol as TAB, but this doesn't work in programming.
    ! I think in some programs this will crash, so it will be in a separate program.
    ! Fortran doesn't allow multi-character literals in single quotes - this will cause compile error
    character ( len=1 ) :: charInvalid = 'TAB'
    write ( *,* ) 'Invalid char:', charInvalid

end program InvalidASCII
