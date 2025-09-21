! Computing factorial with loop
program main
    integer :: Factorial_Result
    
    Factorial_Result = factorial ( 10 )
    print *, "Factorial of 10 is:", Factorial_Result
    
contains
    function factorial ( Factorial_Number_Par ) result ( Factorial_Result )
        integer, intent(in) :: Factorial_Number_Par
        integer :: Factorial_Result
        integer :: Total_Factorial = 1
        integer :: Factorial_Number
        
        do Factorial_Number = 1, Factorial_Number_Par
            Total_Factorial = Total_Factorial * Factorial_Number
        end do
        
        Factorial_Result = Total_Factorial
    end function factorial
end program main
