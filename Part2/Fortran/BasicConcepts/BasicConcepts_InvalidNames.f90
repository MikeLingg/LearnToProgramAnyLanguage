program InvalidNames
    implicit none
    
    ! Don't forget to declare your variables as appropriate to the language, some languages will fail to compile with this program
    character(len=1) :: validName = 'a'
    character(len=1) :: wrongCase = 'a'
    character(len=1) :: wrOngLetter = 'a'
    
    ! This will cause compile error - undefined variable
    write(*,*) invalidName
    ! This will cause compile error - undefined variable
    write(*,*) validname
    ! This will cause compile error - undefined variable
    write(*,*) wrongcase
    ! This will cause compile error - undefined variable
    write(*,*) wr0ngLetter
 
    ! Don't start your variables with numbers or use hyphens
    ! This will cause compile error - invalid identifier
    integer :: 2NameInvalid = 5
    ! This will cause compile error - invalid identifier
    character(len=1) :: invalid-name = 'a'
    
    write(*,*) 2NameInvalid
    write(*,*) invalid-name
    
    ! Note, this causes a compile error because fortran requires all declarations before any statements.
    ! Also avoid using keywords already reserved by the programming language
    ! This will cause compile error - reserved keyword
    integer :: program = 1
    ! This will cause compile error - reserved keyword
    integer :: end = 2
    
    write(*,*) program
    write(*,*) end

end program InvalidNames
