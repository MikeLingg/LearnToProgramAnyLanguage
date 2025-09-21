! Basic function with no parameters or return value
program main
    call print_hello ()
contains
    subroutine print_hello ()
        print *, "Hello"
    end subroutine print_hello
end program main
