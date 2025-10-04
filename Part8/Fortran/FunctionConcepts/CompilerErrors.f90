! Fortran Function Errors - All errors active for compilation

program FortranFunctionErrors
    implicit none
    
    ! Error: Missing 'end subroutine' statement
    subroutine printHello()
        print *, "Hello"
    
    ! Error: Wrong end identifier (doesn't match subroutine name)
    subroutine sayHi()
        print *, "Hi"
    end subroutine greet
    
    ! Error: Using 'end' without specifying 'subroutine' or 'function'
    subroutine display()
        print *, "Display"
    end
    
    ! Error: Missing return type in function declaration
    function getValue()
        getValue = 42
    end function getValue
    
    ! Error: Missing 'result' clause but assigning to wrong variable
    function getNumber() result(num)
        getNumber = 5
    end function getNumber
    
    ! Error: Using 'return' with value (Fortran doesn't work this way)
    integer function calculate()
        return 42
    end function calculate
    
    ! Error: Missing function assignment
    real function getPrice()
        real :: price
        price = 15.99
    end function getPrice
    
    ! Error: Using 'void' or 'none' instead of subroutine
    void printMessage()
        print *, "Message"
    end void printMessage
    
    none doSomething()
        print *, "Something"
    end none doSomething
    
    ! Error: Using 'call' with a function (should only call subroutines)
    integer function add(a, b)
        integer, intent(in) :: a, b
        add = a + b
    end function add
    
    subroutine testCall()
        integer :: result
        call add(5, 10)
    end subroutine testCall
    
    ! Error: Missing 'call' keyword for subroutine
    subroutine process()
        print *, "Processing"
    end subroutine process
    
    subroutine execute()
        process()
    end subroutine execute
    
    ! Error: Using parentheses in subroutine call with no arguments
    subroutine show()
        print *, "Showing"
    end subroutine show
    
    subroutine run()
        call show()
    end subroutine run
    
    ! Error: Missing comma between parameters
    subroutine setPizza(size toppings)
        character(len=*), intent(in) :: size
        integer, intent(in) :: toppings
        print *, size, toppings
    end subroutine setPizza
    
    ! Error: Using semicolon instead of comma between parameters
    real function pizzaCost(size; toppings)
        character(len=*), intent(in) :: size
        integer, intent(in) :: toppings
        pizzaCost = 15.99
    end function pizzaCost
    
    ! Error: Missing intent specification (not an error but bad practice, but wrong syntax here)
    subroutine updateValue(count in out)
        integer :: count
        count = 5
    end subroutine updateValue
    
    ! Error: Using 'default' for optional parameters
    subroutine orderPizza(size, toppings default 2)
        character(len=*), intent(in) :: size
        integer, intent(in) :: toppings
        print *, "Order placed"
    end subroutine orderPizza
    
    ! Error: Named arguments using := instead of =
    subroutine makePizza(size, count)
        character(len=*), intent(in) :: size
        integer, intent(in) :: count
        print *, size, count
    end subroutine makePizza
    
    subroutine placeOrder()
        call makePizza(size := "large", count := 3)
    end subroutine placeOrder
    
    ! Error: Using 'begin' instead of no keyword after declaration
    subroutine calculate() begin
        integer :: x
        x = 5
    end subroutine calculate
    
    ! Error: Missing parameter type declarations inside subroutine
    subroutine getValue(number)
        print *, number
    end subroutine getValue
    
    ! Error: Function name doesn't match in 'end function'
    integer function factorial(n)
        integer, intent(in) :: n
        if (n <= 1) then
            factorial = 1
        else
            factorial = n * factorial(n - 1)
        end if
    end function calculate
    
    ! Error: Using 'procedure' keyword incorrectly
    procedure myProc()
        print *, "Procedure"
    end procedure myProc
    
    ! Error: Parameter type after name instead of with double colon
    subroutine setData(count integer, price real)
        count = 5
        price = 10.0
    end subroutine setData
    
    ! Error: Using 'with parameters' syntax
    subroutine doTask()
        print *, "Task"
    end subroutine doTask
    
    subroutine runTask()
        call doTask with parameters()
    end subroutine runTask

end program FortranFunctionErrors
