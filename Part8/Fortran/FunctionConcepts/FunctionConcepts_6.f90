! Example of function overloading
! Fortran does not support function overloading, this will not compile
program main
    call my_function ( 5 )
    call my_function ( 5.5 )
    
contains
    subroutine my_function ( Int_Parameter_Par )
        integer, intent(in) :: Int_Parameter_Par
        print *, "Int version of my function called", Int_Parameter_Par
    end subroutine my_function
    
    subroutine my_function ( Real_Parameter_Par )
        real, intent(in) :: Real_Parameter_Par
        print *, "Real version of my function called", Real_Parameter_Par
    end subroutine my_function
end program main
