program LoopConcepts
    implicit none
    
    ! Function declaration
    integer :: myStackFunction
    
    ! Variable declarations
    integer :: loopIndex, palindromeIndex, halfLength
    integer :: rowIndex, columnIndex
    integer :: palindromeFrontIndex, palindromeBackIndex
    integer :: arrayIndex, arraySamples, periodCount
    integer :: intCount, foundCount, dataArraySize
    integer :: choice, number, rowCount, columnCount
    integer :: firstCharIndex, secondCharIndex, lettersSize
    integer :: listIndex, swapIndex, myList2Size, swapValue
    integer :: arraySize, myList3Size
    integer :: currentTime, nextFrame
    integer :: index, originalLen, count, largeNumber
    integer :: searchValue
    character(len=15) :: possiblePalindrome
    character(len=6) :: myString
    character :: character
    logical :: isPalindrome, validNumber, exitMenu
    logical :: fiveElementsFound, found
    integer, parameter :: maxInt = huge(1)
    integer, dimension(4) :: myList = [1, 2, 3, 4]
    integer, dimension(10000) :: intArray
    integer, dimension(19) :: dataArray = [1, 2, 3, 4, 5, 6, 5, 7, 5, 8, 5, 9, 5, 10, 11, 12, 13, 14, 15]
    integer, dimension(3,3) :: twoDList = reshape([1, 2, 3, 4, 5, 6, 7, 8, 9], [3, 3])
    character, dimension(5) :: letters = ['a', 'b', 'c', 'd', 'e']
    integer, dimension(10) :: myList2 = [6, 8, 9, 7, 4, 5, 0, 3, 1, 2]
    integer, dimension(2,2) :: twoDList2 = reshape([1, 2, 3, 4], [2, 2])
    integer, dimension(4) :: myArray = [1, 2, 3, 4]
    integer, dimension(4) :: myList3 = [4, 3, 2, 1]
    integer, dimension(8) :: myList4 = [5, 2, 8, 2, 2, 6, 3, 5]
    integer, dimension(5) :: searchArray = [1, 3, 5, 7, 9]

    ! Main program
    write(*,*) ''
    write(*,*) 'Simple 100 iteration loop.'
    do loopIndex = 0, 99
        write(*,*) loopIndex
    end do
    write(*,*) 'After Loop'

    ! Palindrome checker
    ! It is a palindrome until we prove it is not.
    write(*,*) ''
    write(*,*) 'Palindrome checker.'
    possiblePalindrome = 'step on no pets'
    isPalindrome = .true.
    halfLength = len_trim(possiblePalindrome) / 2

    do palindromeIndex = 1, halfLength
        if (possiblePalindrome(palindromeIndex:palindromeIndex) /= &
            possiblePalindrome(len_trim(possiblePalindrome) - &
              palindromeIndex + 1:len_trim(possiblePalindrome) - &
              palindromeIndex + 1)) then
            isPalindrome = .false.
            exit
        end if
    end do

    if (isPalindrome .eqv. .true.) then
        write(*,*) trim(possiblePalindrome), ' is a palindrome.'
    else
        write(*,*) trim(possiblePalindrome), ' is NOT a palindrome.'
    end if

    ! Fortran cannot have two loop variables in a single DO loop
    ! Must use separate variables

    ! Two loops continuing a loop variable
    write(*,*) ''
    write(*,*) 'Loops continuing a loop variable.'
    do loopIndex = 5, 14
        write(*,*) 'Loop Index First Loop:', loopIndex
    end do
    do loopIndex = loopIndex + 1, 54
        write(*,*) 'Loop Index Second Loop:', loopIndex
    end do

    ! Fortran can NOT perform floating point loops

    ! Fortran range loops
    write(*,*) ''
    write(*,*) 'Looping over a range of values'
    do loopIndex = 0, 99
        write(*,*) 'Loop Index:', loopIndex
    end do
    write(*,*) 'After Loop'

    ! Range variants
    write(*,*) ''
    write(*,*) 'Different methods of looping over a range.'
    write(*,*) ''
    write(*,*) 'Start at different index'
    write(*,*) ''
    do loopIndex = 1, 10
        write(*,*) 'Loop Index:', loopIndex
    end do
    write(*,*) 'After Loop'

    write(*,*) ''
    write(*,*) 'Different loop increment'
    write(*,*) ''
    do loopIndex = 5, 11, 2
        write(*,*) 'Loop Index:', loopIndex
    end do
    write(*,*) 'After Loop'

    write(*,*) ''
    write(*,*) 'Reverse increment'
    write(*,*) ''
    do loopIndex = 9, 2, -1
        write(*,*) 'Loop Index:', loopIndex
    end do
    write(*,*) 'After Loop'

    ! Looping over array indices
    write(*,*) 'Looping over a range of indices in an array'
    do loopIndex = 1, size(myList)
        write(*,*) 'Loop Index:', loopIndex
    end do
    write(*,*) 'After Loop'

    ! Bad, myList is not a number - This will not compile
    write(*,*) ''
    write(*,*) 'Attempting bad range usage - myList is not a number'
    !do loopIndex = 1, myList
    !    write(*,*) 'Loop Index:', loopIndex
    !end do
    !write(*,*) 'After Loop'

    ! Initialize all entries in a large array
    write(*,*) ''
    write(*,*) 'Initialize values in an array'
    arraySamples = 10000

    ! This is pointless in Fortran as arrays can be initialized efficiently
    ! The assignment statement is much faster than this loop.
    do arrayIndex = 1, arraySamples
        intArray(arrayIndex) = 0
    end do

    ! Initialize all entries to index * 2 + 1
    do arrayIndex = 1, arraySamples
        intArray(arrayIndex) = arrayIndex * 2 + 1
    end do

    ! Example with string validation
    ! Loop through a string and ensure it contains a valid number
    write(*,*) ''
    write(*,*) 'Verify string contains valid number.'
    myString = '123.45'
    periodCount = 0
    validNumber = .true.
    do arrayIndex = 1, len_trim(myString)
        character = myString(arrayIndex:arrayIndex)
        if (character == '.') then
            periodCount = periodCount + 1
            if (periodCount > 1) then
                validNumber = .false.
                exit
            end if
        else if (character < '0' .or. character > '9') then
            validNumber = .false.
            exit
        end if
    end do

    write(*,*) 'String is valid number:', validNumber

    ! Basic condition loop
    write(*,*) ''
    write(*,*) 'Simple conditional loop'
    intCount = 1000000
    do while (.true.)
        ! Just print the hello once so the terminal isn't overwhelmed.
        if (intCount == 1000000) then
            write(*,*) 'Hello'
        end if

        ! Exit added so this isn't an infinite loop
        intCount = intCount - 1
        if (intCount <= 0) exit
    end do

    ! Do while loop executing like our basic counting loop example
    write(*,*) ''
    write(*,*) 'Example do while loop performing basic counting loop'
    loopIndex = 0
    do while (loopIndex < 100)
        write(*,*) loopIndex
        loopIndex = loopIndex + 1
    end do
    write(*,*) 'After loop'

    ! More appropriate use of a do while loop
    write(*,*) ''
    write(*,*) 'Better do while loop with length of loop unknown at start.'
    dataArraySize = size(dataArray)
    loopIndex = 1
    foundCount = 0
    fiveElementsFound = .false.
    do while ((loopIndex <= dataArraySize) .and. (fiveElementsFound .eqv. .false.))
        if (dataArray(loopIndex) < 10) then
            foundCount = foundCount + 1
            if (foundCount >= 5) then
                fiveElementsFound = .true.
            end if
        end if
        loopIndex = loopIndex + 1
    end do

    ! Using a loop to prompt user input until valid
    write(*,*) ''
    write(*,*) 'Conditional loop based on user input.'
    exitMenu = .false.
    do while (exitMenu .eqv. .false.)
        write(*,*) 'Main Menu:'
        write(*,*) '1. Start Game'
        write(*,*) '2. Load Game'
        write(*,*) '3. Show Help'
        write(*,*) '4. Exit'
        write(*,'(A)', advance='no') 'Enter your choice: '
        
        read(*,*) choice

        ! Fortran has select case statements
        select case (choice)
        case (1)
            write(*,*) 'Starting new game...'
        case (2)
            write(*,*) 'Loading saved game...'
        case (3)
            write(*,*) 'Help: Use the number keys to navigate the menu.'
        case (4)
            write(*,*) 'Exiting program. Goodbye!'
            exitMenu = .true.
        case default
            write(*,*) 'Invalid choice. Please select a valid option.'
        end select
    end do

    ! Fortran has no do-while equivalent, but can simulate
    write(*,*) 'Do while equivalent example: Hint, entering 42 will exit this loop'
    do
        write(*,'(A)', advance='no') 'Guess a number: '
        read(*,*) number
        if (number == 42) exit
    end do

    ! Nested loop for simple 2d array print
    write(*,*) ''
    write(*,*) 'Nested loop, 2d, example.'
    rowCount = 3
    columnCount = 3
    
    do rowIndex = 1, rowCount
        do columnIndex = 1, columnCount
            write(*,'(I0,A)', advance='no') twoDList(rowIndex, columnIndex), ' '
        end do
        write(*,*) ''
    end do

    ! Nested loop to print character permutations
    write(*,*) ''
    write(*,*) 'Nested permutations loop.'
    lettersSize = size(letters)
    
    do firstCharIndex = 1, lettersSize
        do secondCharIndex = 1, lettersSize
            write(*,*) letters(firstCharIndex), letters(secondCharIndex)
        end do
    end do

    ! Nested loop to order a list of numbers
    write(*,*) ''
    write(*,*) 'Nested loop for sorting numbers.'
    myList2Size = size(myList2)
    do listIndex = 1, myList2Size
        do swapIndex = 1, myList2Size - listIndex
            if (myList2(swapIndex) > myList2(swapIndex + 1)) then
                swapValue = myList2(swapIndex)
                myList2(swapIndex) = myList2(swapIndex + 1)
                myList2(swapIndex + 1) = swapValue
            end if
        end do
    end do

    ! Exit within nested loop
    write(*,*) ''
    write(*,*) 'Using an exit in a nested loop.'
    rowCount = 2
    columnCount = 2
    
    do rowIndex = 1, rowCount
        do columnIndex = 1, columnCount
            write(*,*) twoDList2(rowIndex, columnIndex)
            exit
        end do
    end do

    ! Off by 1 error - Will cause runtime error
    write(*,*) ''
    write(*,*) 'Off by 1 loop error for array access.'
    arraySize = size(myArray)
    write(*,*) 'Loop through array off by 1.'
    do loopIndex = 1, arraySize + 1
        write(*,*) loopIndex, myArray(loopIndex)
    end do

    write(*,*) 'Loop through array correct.'
    do loopIndex = 1, arraySize
        write(*,*) myArray(loopIndex)
    end do

    ! Off by 1 when performing a partial list sort - Will exceed bounds
    write(*,*) ''
    write(*,*) 'Sort list with off by 1 error.'
    myList3Size = size(myList3)
    do loopIndex = 1, myList3Size
        if (myList3(loopIndex) > myList3(loopIndex + 1)) then
            swapValue = myList3(loopIndex)
            myList3(loopIndex) = myList3(loopIndex + 1)
            myList3(loopIndex + 1) = swapValue
        end if
    end do

    ! Off by 1 when performing a partial list sort - Will NOT exceed bounds
    write(*,*) ''
    write(*,*) 'Sort list without off by 1 error.'
    do loopIndex = 1, myList3Size - 1
        if (myList3(loopIndex) > myList3(loopIndex + 1)) then
            swapValue = myList3(loopIndex)
            myList3(loopIndex) = myList3(loopIndex + 1)
            myList3(loopIndex + 1) = swapValue
        end if
    end do

    ! Fortran does not allow empty loop blocks

    ! Redeclaring loop variable - Does not compile in Fortran
    !write(*,*) ''
    !write(*,*) 'Redeclared loop variable.'
    !do loopIndex = 1, 10
    !    do loopIndex = 1, 10
    !        write(*,*) 'Loop Index:', loopIndex
    !    end do
    !end do

    ! Loop condition untrue on entry
    write(*,*) ''
    write(*,*) 'Loop condition untrue on entry.'
    currentTime = 5
    nextFrame = 5
    do while (currentTime < nextFrame)
        write(*,*) 'Processing background tasks'
        currentTime = currentTime + 1
    end do

    ! Wrong reverse range loop
    write(*,*) ''
    write(*,*) 'Wrong reverse range loop.'
    arraySize = 100
    do loopIndex = arraySize, 1  ! This won't loop because start > end
        write(*,*) 'Loop index:', loopIndex
    end do

    ! Right reverse range loop
    write(*,*) 'Reverse range loop correct.'
    do loopIndex = arraySize, 1, -1
        write(*,*) 'Loop index:', loopIndex
    end do

    ! Fortran arrays are fixed size, limited dynamic array operations
    ! Would need allocatable arrays for dynamic operations

    ! Fortran integers can overflow
    write(*,*) 'Loop to very large number.'
    largeNumber = maxInt - 1000
    count = 0
    do loopIndex = largeNumber, maxInt
        count = count + 1
        if (count > 10) then
            write(*,*) 'Stopped early at loop index:', loopIndex
            exit
        end if
    end do

end program LoopConcepts
