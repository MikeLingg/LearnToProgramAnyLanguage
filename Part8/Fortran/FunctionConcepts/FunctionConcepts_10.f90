! Recursive function call to compute factorial
program main
    integer :: Factorial_Result
    
    Factorial_Result = factorial ( 10 )
    print *, "Factorial of 10 is:", Factorial_Result
    
contains
    recursive function factorial ( Factorial_Number_Par ) result ( Factorial_Result )
        integer, intent(in) :: Factorial_Number_Par
        integer :: Factorial_Result
        
        if ( Factorial_Number_Par <= 1 ) then
            Factorial_Result = 1
        else
            Factorial_Result = Factorial_Number_Par * factorial ( Factorial_Number_Par - 1 )
        end if
    end function factorial
end program main
