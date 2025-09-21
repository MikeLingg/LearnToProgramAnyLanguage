! Fortran does not pass command line argmuents to main, uses functions.
program main
    integer :: Arg_Count
    integer :: I
    character(len=256) :: Arg_Value
    
    Arg_Count = command_argument_count ()
    print *, "Number of arguments:", Arg_Count + 1
    print *, "Arguments:"
    
    do I = 0, Arg_Count
        call get_command_argument ( I, Arg_Value )
        print *, char(9), "Argument", I, ":", trim ( Arg_Value )
    end do
end program main
