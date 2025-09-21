! This example will not compile as we cannot call a variable like a function
program main
    integer :: My_Variable = 5
    call My_Variable ()
contains
    subroutine my_function ()
        print *, "Called my_function"
    end subroutine my_function
end program main
