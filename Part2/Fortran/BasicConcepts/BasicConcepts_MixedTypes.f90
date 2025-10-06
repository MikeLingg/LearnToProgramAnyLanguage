program MixedTypes
    implicit none
    
    ! Do not mix your types in many languages
    ! Fortran is strongly typed and will not allow implicit type conversions
    ! These will cause compile errors due to type mismatches
    integer :: myInt = 123.45
    real :: myFloat = 'a'
    character(len=1) :: myChar = 543.21
    
    write(*,*) 'myInt:', myInt
    write(*,*) 'myFloat:', myFloat
    write(*,*) 'myChar:', myChar

end program MixedTypes
