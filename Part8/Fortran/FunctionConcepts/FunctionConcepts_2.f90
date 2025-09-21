! An example of function variable scope
program main
    integer :: Global_Variable = 15
    integer :: Global_To_Be_Shadowed = 5
    
    print *, "Global variable:", Global_Variable
    print *, "Global shadowed:", Global_To_Be_Shadowed
    call my_function ()
    print *, "Function variable:", My_Variable
    print *, "Global variable:", Global_Variable
    print *, "Global shadowed:", Global_To_Be_Shadowed
    
contains
    subroutine my_function ()
        integer :: My_Variable = 55
        integer :: Global_To_Be_Shadowed = 15
        Global_Variable = 42
    end subroutine my_function
end program main
