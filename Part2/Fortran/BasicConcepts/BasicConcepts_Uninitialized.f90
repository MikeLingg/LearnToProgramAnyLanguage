program Uninitialized
    implicit none
    
    ! I think in some programs this will cause a crash or failure to compile, so it will be in a separate program.
    ! Fortran allows uninitialized variables - they contain undefined values
    character(len=1) :: myCharacter
    write(*,*) 'myCharacter:', myCharacter, 'as int:', ichar(myCharacter)

end program Uninitialized
