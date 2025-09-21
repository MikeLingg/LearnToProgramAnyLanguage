! Infinite recursive function
program main
    call recursive_function ()
    
contains
    recursive subroutine recursive_function ()
        call recursive_function ()
    end subroutine recursive_function
end program main
