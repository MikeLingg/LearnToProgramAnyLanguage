program LoopErrors
    implicit none
    
    integer :: i, j, k, counter, value
    integer :: arr(5)
    
    ! ERROR: Missing 'do' keyword
    i = 1, 10
        write(*,*) i
    end do

    ! ERROR: Using 'for' instead of 'do' (from C-style languages)
    for (i = 1, 10)
        write(*,*) i
    end do

    ! ERROR: Using parentheses around loop range (C-style)
    do (i = 1, 10)
        write(*,*) i
    end do

    ! ERROR: Missing 'end do'
    do i = 1, 10
        write(*,*) i

    ! ERROR: Using 'end loop' instead of 'end do'
    do i = 1, 10
        write(*,*) i
    end loop

    ! ERROR: Using semicolons instead of commas in range
    do i = 1; 10
        write(*,*) i
    end do

    ! ERROR: Using 'to' instead of comma (from other languages)
    do i = 1 to 10
        write(*,*) i
    end do

    ! ERROR: Using '..' for range (from Ada/others)
    do i = 1 .. 10
        write(*,*) i
    end do

    ! ERROR: Missing comma in range specification
    do i = 1 10
        write(*,*) i
    end do

    ! ERROR: Using assignment operator = instead of comma
    do i = 1 = 10
        write(*,*) i
    end do

    ! ERROR: Using ++ or += for increment (C-style)
    do i = 1, 10
        write(*,*) i
        i++
    end do

    ! ERROR: Missing 'while' keyword in do while loop
    counter = 0
    do (counter < 10)
        write(*,*) counter
        counter = counter + 1
    end do

    ! ERROR: Using 'loop' instead of 'do' (Ada style)
    loop i = 1, 10
        write(*,*) i
    end loop

    ! ERROR: Using 'break' instead of 'exit'
    do i = 1, 10
        if (i == 5) break
        write(*,*) i
    end do

    ! ERROR: Using 'continue' for loop control (wrong context)
    do i = 1, 10
        if (i == 5) continue
        write(*,*) i
    end do

    ! ERROR: Missing 'end if' inside loop
    do i = 1, 10
        if (i > 5) then
            write(*,*) 'Greater than 5'
        write(*,*) i
    end do

    ! ERROR: Using == instead of .eq. or proper comparison
    do i = 1, 10
        if (i == 5) then
            exit
        end if
    end do

    ! ERROR: Missing 'then' keyword in if statement
    do i = 1, 10
        if (i > 5)
            write(*,*) 'Greater'
        end if
    end do

    ! ERROR: Using curly braces instead of do/end do
    do i = 1, 10 {
        write(*,*) i
    }

    ! ERROR: Missing space before step value
    do i = 1, 10,2
        write(*,*) i
    end do

    ! ERROR: Using 'step' keyword (VB/other languages)
    do i = 1, 10, step 2
        write(*,*) i
    end do

    ! ERROR: Using 'by' for increment (not valid Fortran)
    do i = 1, 10 by 2
        write(*,*) i
    end do

    ! ERROR: Wrong order for reverse loop (missing negative step)
    do i = 10, 1
        write(*,*) i
    end do

    ! ERROR: Using 'downto' (Pascal style)
    do i = 10 downto 1
        write(*,*) i
    end do

    ! ERROR: Using 'in' keyword (Python/Ruby style)
    arr = [1, 2, 3, 4, 5]
    do i in arr
        write(*,*) i
    end do

    ! ERROR: Using 'foreach' (C#/other languages)
    foreach i in arr
        write(*,*) i
    end foreach

    ! ERROR: Missing opening 'do' statement with just condition
    while (counter < 10)
        write(*,*) counter
        counter = counter + 1
    end do

    ! ERROR: Using 'wend' instead of 'end do' (BASIC style)
    do while (counter < 10)
        write(*,*) counter
        counter = counter + 1
    wend

    ! ERROR: Using := for assignment
    do i := 1, 10
        write(*,*) i
    end do

    ! ERROR: Using floating point in loop range
    do i = 1.0, 10.0
        write(*,*) i
    end do

    ! ERROR: Using real variable as loop counter
    real :: x
    do x = 1.0, 10.0
        write(*,*) x
    end do

    ! ERROR: Missing condition in do while
    do while
        write(*,*) 'Hello'
        if (counter > 10) exit
    end do

    ! ERROR: Using parentheses incorrectly in range
    do i = (1), (10)
        write(*,*) i
    end do

    ! ERROR: Using 'until' instead of 'while' (other languages)
    do until (counter >= 10)
        write(*,*) counter
        counter = counter + 1
    end do

    ! ERROR: Missing parentheses around while condition
    do while counter < 10
        write(*,*) counter
        counter = counter + 1
    end do

    ! ERROR: Using 'loop' with 'end loop' (Ada style)
    loop
        write(*,*) 'Hello'
        if (counter > 10) exit
    end loop

    ! ERROR: Using 'repeat' (other languages)
    repeat
        write(*,*) counter
        counter = counter + 1
    until (counter >= 10)

    ! ERROR: Using label incorrectly with exit
    outer: do i = 1, 10
        inner: do j = 1, 10
            if (j == 5) exit outer
        end do inner
    end do outer

    ! ERROR: Wrong syntax for named loop construct
    outerloop do i = 1, 10
        write(*,*) i
    end do outerloop

    ! ERROR: Using ampersand incorrectly in loop
    do i = 1, 10 &
    write(*,*) i
    end do

    ! ERROR: Missing logical operators in compound condition
    do while (i < 10 j > 0)
        write(*,*) i
        i = i + 1
    end do

    ! ERROR: Using && or || instead of .and. .or.
    do while (i < 10 && j > 0)
        write(*,*) i
        i = i + 1
    end do

    ! ERROR: Using 'else if' as two words instead of 'else if'
    do i = 1, 10
        if (i < 5) then
            write(*,*) 'Less'
        elseif (i > 5) then
            write(*,*) 'Greater'
        end if
    end do

    ! ERROR: Missing colon after loop label
    myloop do i = 1, 10
        write(*,*) i
    end do myloop

    ! ERROR: Using C-style increment in step
    do i = 1, 10, i++
        write(*,*) i
    end do

    ! ERROR: Using array as loop bound without size()
    do i = 1, arr
        write(*,*) i
    end do

    ! ERROR: Using 'next' instead of 'end do' (BASIC/VB)
    do i = 1, 10
        write(*,*) i
    next i

    ! ERROR: Using 'end for' instead of 'end do'
    do i = 1, 10
        write(*,*) i
    end for

    ! ERROR: Missing increment specification entirely
    do i = 1, 10,
        write(*,*) i
    end do

end program LoopErrors
