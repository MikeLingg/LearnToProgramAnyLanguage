program BranchConcepts
    implicit none
    
    ! Variable declarations
    logical :: myVariable, heaterOn, temperatureCold, budgetOverrun
    logical :: isLoggedIn, isBanned, resourceIsAvailable, userExists, passwordValid
    logical :: readBool, readValidBool, inputMatchesFalse
    integer :: temperature, score, age, switchVariable, choice
    integer :: time, distance, speed, readInt, subStringLength
    real :: budget, buffer, estimatedCost, overrunAmount
    real :: firstFloat, secondFloat, sum, thirdFloat, tolerance, difference
    character(len=5) :: role
    character(len=4) :: method
    character(len=5) :: falseString
    character(len=256) :: inputString
    integer :: variable, outerVariable, innerVariable
    integer :: ios
    
    ! Stubbed out functions are implemented as internal procedures at the end
    
    ! Basic if statements with hard coded conditions
    write( *, '(A)' ) 'Before Conditions'
    if ( .true. ) then
        write( *, '(A)' ) 'Branch Executed'
    end if

    if ( .false. ) then
        write( *, '(A)' ) 'Branch Not Executed'
    end if

    write( *, '(A)' ) 'After Conditions'

    ! If with hard coded assignment - Fortran doesn't allow assignment as condition
    myVariable = .true.
    if ( myVariable ) then
        write( *, '(A)' ) 'Branch Executed'
    end if

    ! If with variable assignment (using function result) - Fortran doesn't allow assignment as condition
    myVariable = someFunctionResult()
    if ( myVariable ) then
        write( *, '(A)' ) 'Branch Executed'
    end if

    ! More proper conditional branch
    temperature = 64

    heaterOn = .false.
    temperatureCold = ( temperature < 70 )
    if ( temperatureCold .eqv. .true. ) then
        heaterOn = .true.
    end if

    write( *, '(A,L1)' ) 'Heater is on: ', heaterOn

    ! Alternate code to evaluate a conditional branch
    heaterOn = .false.
    if ( temperature < 70 ) then
        heaterOn = .true.
    end if

    write( *, '(A,L1)' ) 'Heater is on: ', heaterOn

    ! Using a code block that executes if a condition is true.
    budget = 5000.00
    buffer = 500.00
    estimatedCost = 4750.00

    budgetOverrun = .false.

    if ( ( estimatedCost + buffer ) > budget ) then
        overrunAmount = ( estimatedCost + buffer ) - budget
        write( *, '(A,F0.2)' ) 'Overrun Amount: ', overrunAmount
        buffer = 50.0
        estimatedCost = budget - buffer
        budgetOverrun = .true.
    end if

    write( *, '(A,L1)' ) 'Budget overrun occurred: ', budgetOverrun

    ! Variable scope in code blocks.
    ! Many languages will fail trying to access innerVariable outside of the block.
    ! In most languages I can even take the if True line out and just have the code block.
    ! Fortran has different scoping rules - variables are typically program-wide
    !outerVariable = 10
    !if ( .true. ) then
    !    innerVariable = 20
    !end if
    !write( *, '(A,I0,A,I0)' ) 'Variables: ', outerVariable, ' : ', innerVariable

    ! Variable scope: Shadowed variables
    ! Fortran doesn't have block-level scoping like C/C++, so no true shadowing
    variable = 10
    if ( .true. ) then
        variable = 20  ! This modifies the same variable
        write( *, '(A,I0)' ) 'Variable inside: ', variable
    end if

    write( *, '(A,I0)' ) 'Variable outside: ', variable

    ! Error of line being outside of if block, note the lack of begin/end block.
    ! Different languages handle this differently.
    ! Fortran requires then/end if for all if statements
    !if ( .false. ) &
    !    write( *, '(A)' ) 'Statement One'
    !    write( *, '(A)' ) 'Statement Two'

    if ( .false. ) then
        write( *, '(A)' ) 'Statement Three'
        write( *, '(A)' ) 'Statement Four'
    end if

    ! Good in Fortran
    if ( .true. ) then
        write( *, '(A)' ) 'Good branch!'
    end if

    ! Bad in Fortran
    !if true then
    !    write( *, '(A)' ) 'Bad branch!'
    !end if

    ! If disconnected from the block, note the semicolon.
    ! This behavior is going to vary from language to language
    ! Fortran prevents this pattern - it requires proper if statement syntax
    !if ( .false. ); then
    !    write( *, '(A)' ) 'Hello'
    !end if

    ! Multiple separate if statements with overlapping conditions
    score = 85

    if ( score >= 90 .and. score <= 100 ) then
        write( *, '(A)' ) 'You got an A'
    end if
    if ( score >= 80 .and. score < 90 ) then
        write( *, '(A)' ) 'You got a B'
    end if
    if ( score >= 70 .and. score < 80 ) then
        write( *, '(A)' ) 'You got a C'
    end if
    if ( score >= 60 .and. score < 70 ) then
        write( *, '(A)' ) 'You got a D'
    end if
    if ( score < 60 ) then
        write( *, '(A)' ) 'You failed the test'
    end if

    ! Else if example (else if in Fortran)
    if ( score >= 90 ) then
        write( *, '(A)' ) 'You got an A'
    else if ( score >= 80 ) then
        write( *, '(A)' ) 'You got a B'
    else if ( score >= 70 ) then
        write( *, '(A)' ) 'You got a C'
    else if ( score >= 60 ) then
        write( *, '(A)' ) 'You got a D'
    else if ( score < 60 ) then
        write( *, '(A)' ) 'You failed the test'
    end if

    ! Forgetting the else on an else if
    if ( score >= 90 ) then
        write( *, '(A)' ) 'You got an A'
    end if
    if ( score >= 80 ) then
        write( *, '(A)' ) 'You got a B'
    else if ( score >= 70 ) then
        write( *, '(A)' ) 'You got a C'
    end if

    ! Else if without if, this will not compile in most languages.
    !else if ( score >= 90 ) then
    !    write( *, '(A)' ) 'You got an A'
    !end if

    ! Adding an else to our if/else if
    if ( score >= 90 ) then
        write( *, '(A)' ) 'You got an A'
    else if ( score >= 80 ) then
        write( *, '(A)' ) 'You got a B'
    else if ( score >= 70 ) then
        write( *, '(A)' ) 'You got a C'
    else if ( score >= 60 ) then
        write( *, '(A)' ) 'You got a D'
    else
        write( *, '(A)' ) 'You failed the test'
    end if

    ! Unreachable else, programming languages may not warn about this
    age = 125

    if ( ( age > 0 ) .or. ( age < 100 ) ) then
        write( *, '(A)' ) 'Valid age'
    else
        write( *, '(A)' ) 'Invalid age'
    end if

    ! Else not following If or Else If, very uncompilable.
    !else
    !    write( *, '(A)' ) 'Hello from else!'
    !else if ( .true. ) then
    !    write( *, '(A)' ) 'Hello from else if!'
    !if ( .true. ) then
    !    write( *, '(A)' ) 'Hello from if!'
    !end if

    ! Example of a complex condition that could be made a nested if
    isLoggedIn = .true.
    role = 'admin'
    method = 'POST'
    isBanned = .false.
    resourceIsAvailable = .true.

    if ( ( isLoggedIn .eqv. .true. ) .and. ( role == 'admin' ) .and. &
         ( method == 'POST' ) .and. ( isBanned .eqv. .false. ) .and. &
         ( resourceIsAvailable .eqv. .true. ) ) then
        write( *, '(A)' ) 'Access granted'
    end if

    ! Breaking the complex condition into a nested if
    if ( isLoggedIn .eqv. .true. ) then
        if ( role == 'admin' ) then
            if ( method == 'POST' ) then
                if ( isBanned .eqv. .false. ) then
                    if ( resourceIsAvailable .eqv. .true. ) then
                        write( *, '(A)' ) 'Access granted'
                    else
                        write( *, '(A)' ) 'Resource Unavailable'
                    end if
                else
                    write( *, '(A)' ) 'User is Banned'
                end if
            else
                write( *, '(A)' ) 'Wrong Method'
            end if
        else
            write( *, '(A)' ) 'Wrong User Level'
        end if
    else
        write( *, '(A)' ) 'Please Log In'
    end if

    ! Dangling Else - How this is handled will differ in different languages
    ! Fortran prevents this pattern - it requires then/end if for all if statements
    userExists = .true.
    passwordValid = .true.

    !if ( userExists .eqv. .true. ) &
    !    if ( passwordValid .eqv. .true. ) &
    !        write( *, '(A)' ) 'Access granted'
    !else
    !    write( *, '(A)' ) 'Retry user name and password'
    !end if

    ! No dangling else with blocks explicitly defined
    if ( userExists .eqv. .true. ) then
        if ( passwordValid .eqv. .true. ) then
            write( *, '(A)' ) 'Access granted'
        else
            write( *, '(A)' ) 'Retry password'
        end if
    end if

    ! Basic switch statement (using select case in Fortran)
    switchVariable = 2

    select case ( switchVariable )
        case ( 1 )
            write( *, '(A)' ) 'Variable is 1'
        case ( 2 )
            write( *, '(A)' ) 'Variable is 2'
        case default
            write( *, '(A)' ) 'Variable is unexpected value!'
    end select

    ! Switch on user input
    write( *, '(A)' ) 'Main Menu:'
    write( *, '(A)' ) '1. Start Game'
    write( *, '(A)' ) '2. Load Game'
    write( *, '(A)' ) '3. Show Help'
    write( *, '(A)' ) '4. Exit'
    write( *, '(A)', advance='no' ) 'Enter your choice: '

    choice = userInput()

    select case ( choice )
        case ( 1 )
            write( *, '(A)' ) 'Starting new game...'
        case ( 2 )
            write( *, '(A)' ) 'Loading saved game...'
        case ( 3 )
            write( *, '(A)' ) 'Help: Use the number keys to navigate the menu.'
        case ( 4 )
            write( *, '(A)' ) 'Exiting program. Goodbye!'
        case default
            write( *, '(A)' ) 'Invalid choice. Please select a valid option.'
    end select

    ! Divide by zero defensive condition
    time = 0
    distance = 100
    speed = 0
    if ( time /= 0 ) then
        speed = distance / time
    end if
    write( *, '(A,I0)' ) 'Speed: ', speed

    ! Handling both valid and invalid user inputs converted to booleans
    write( *, '(A)' ) 'Enter a number:'
    read( *, '(A)', iostat=ios ) inputString
    
    read( inputString, *, iostat=ios ) readInt
    if ( ios == 0 ) then
        write( *, '(A)' ) 'User entered a valid number'
    else
        readInt = -1
        write( *, '(A)' ) 'Invalid number entered.'
    end if

    ! Method 1 of parsing an input string to a boolean
    write( *, '(A)' ) 'Enter a boolean:'
    read( *, '(A)' ) inputString
    
    readValidBool = .false.

    read( inputString, *, iostat=ios ) readInt
    if ( ios == 0 ) then
        readValidBool = .true.
        if ( readInt == 0 ) then
            readBool = .false.
        else
            readBool = .true.
        end if
    else
        ! Convert to lowercase for comparison
        call toLower( inputString )
        if ( trim( inputString ) == 'false' ) then
            readBool = .false.
            readValidBool = .true.
        else if ( trim( inputString ) == 'true' ) then
            readBool = .true.
            readValidBool = .true.
        else
            write( *, '(A)' ) 'Invalid boolean entered'
        end if
    end if

    if ( readValidBool .eqv. .true. ) then
        write( *, '(A)' ) 'Entered boolean is valid'
    end if

    ! Alternate method of parsing a string to boolean using guard clauses instead of nested conditions
    write( *, '(A)' ) 'Enter a boolean:'
    read( *, '(A)' ) inputString
    
    ! Trim the input
    inputString = trim( inputString )
    readValidBool = .false.

    read( inputString, *, iostat=ios ) readInt
    if ( ios == 0 ) then
        readValidBool = .true.
        if ( readInt == 0 ) then
            readBool = .false.
        else
            readBool = .true.
        end if
    end if

    if ( readValidBool .eqv. .false. ) then
        call toLower( inputString )
        if ( trim( inputString ) == 'false' ) then
            readBool = .false.
            readValidBool = .true.
        else if ( trim( inputString ) == 'true' ) then
            readBool = .true.
            readValidBool = .true.
        end if
    end if

    if ( readValidBool .eqv. .true. ) then
        write( *, '(A)' ) 'Valid boolean entered'
    else
        write( *, '(A)' ) 'Invalid boolean entered'
    end if

    ! Compare two strings, only up to the length of the shortest string
    falseString = 'false'
    
    inputMatchesFalse = ( index( inputString, falseString ) == 1 )

    ! Make sure both strings have the appropriate length
    falseString = 'false'
    subStringLength = len_trim( falseString )

    if ( subStringLength > len_trim( inputString ) ) then
        subStringLength = len_trim( inputString )
    end if

    inputMatchesFalse = ( inputString( 1:subStringLength ) == &
                         falseString( 1:subStringLength ) )

    write( *, '(A,L1)' ) 'False Entered ', inputMatchesFalse

    ! Float comparison with both positive and negative differences
    firstFloat = 0.1
    secondFloat = 0.2
    sum = firstFloat + secondFloat
    thirdFloat = 0.3

    tolerance = 0.000001

    difference = sum - thirdFloat

    if ( ( difference > -tolerance ) .and. ( difference < tolerance ) ) then
        write( *, '(A)' ) 'First float plus second float is equal to third float.'
    else
        write( *, '(A)' ) 'First float plus second float is NOT equal to third float.'
    end if

    ! Float comparison with condition ensuring positive difference
    firstFloat = 0.1
    secondFloat = 0.2
    sum = firstFloat + secondFloat
    thirdFloat = 0.3

    tolerance = 0.000001

    difference = sum - thirdFloat
    if ( difference < 0.0 ) then
        difference = -difference
    end if

    if ( difference < tolerance ) then
        write( *, '(A)' ) 'First float plus second float is equal to third float.'
    else
        write( *, '(A)' ) 'First float plus second float is NOT equal to third float.'
    end if

    ! Float comparison using abs to ensure positive difference
    firstFloat = 0.1
    secondFloat = 0.2
    sum = firstFloat + secondFloat
    thirdFloat = 0.3

    tolerance = 0.000001

    difference = abs( sum - thirdFloat )

    if ( difference < tolerance ) then
        write( *, '(A)' ) 'First float plus second float is equal to third float.'
    else
        write( *, '(A)' ) 'First float plus second float is NOT equal to third float.'
    end if

contains

    ! Stubbed out functions
    function userInput() result( value )
        integer :: value
        value = 3
    end function userInput

    function someFunctionResult() result( value )
        logical :: value
        value = .true.
    end function someFunctionResult

    ! Helper subroutine to convert string to lowercase
    subroutine toLower( str )
        character(len=*), intent(inout) :: str
        integer :: i, ic
        
        do i = 1, len_trim( str )
            ic = iachar( str(i:i) )
            if ( ic >= 65 .and. ic <= 90 ) then
                str(i:i) = achar( ic + 32 )
            end if
        end do
    end subroutine toLower

end program BranchConcepts
