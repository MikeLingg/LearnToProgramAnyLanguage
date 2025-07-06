program Redeclaration
    implicit none
    
    ! Do not redeclare variable names in most languages:
    character(len=1) :: duplicateCharacter = 'a'
    ! This will cause compile error - variable already declared
    character(len=1) :: duplicateCharacter = 'b'
    
    write(*,*) 'duplicateCharacter:', duplicateCharacter

end program Redeclaration
