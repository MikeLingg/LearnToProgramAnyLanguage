program statement_errors
    implicit none
    
    ! Variable declarations
    integer :: a = 5
    integer :: b = 10
    real :: c = 3.14
    logical :: flag = .true.
    character(len=5) :: stringOne = "Hello"
    integer :: result
    
    ! ERROR: Using wrong logical operators (C-style instead of Fortran)
    flag = .true. && .false.
    
    ! ERROR: Using = instead of == for comparison
    flag = (a = b)
    
    ! ERROR: Wrong logical literal format
    flag = true
    
    ! ERROR: Type mismatch in arithmetic operations
    result = a + flag
    
    ! ERROR: Missing periods around logical operators
    flag = a .gt b
    
    ! ERROR: Wrong format in write statement
    write(*, '(I)') a
    
    ! ERROR: String concatenation with wrong operator
    stringOne = "Hi" + "There"
    
    ! ERROR: Using C-style comments
    // This is a C comment, not Fortran
    
    ! ERROR: Format mismatch in write statement
    write(*, '(F5.2)') a
    
    ! ERROR: Logical operation on non-logical types
    result = a .and. b
    
    ! ERROR: Character variable used in arithmetic
    result = stringOne + 5
    
    ! ERROR: Missing continuation character for long expression
    result = a + b + c + a + b + c + a + b + c + a + b + c + a + b + c
    
    ! ERROR: Wrong case sensitivity in type declarations
    INTEGER :: wrong_case
    
    ! ERROR: Division by literal zero
    result = a / 0
    
    ! ERROR: Array notation on scalar variable
    result = a(1)
    
end program statement_errors
