program Main
    implicit none
    
    ! Initialization code:
    ! Cell indexes:
    ! +-+-+-+
    ! |0|1|2|
    ! +-+-+-+
    ! |3|4|5|
    ! +-+-+-+
    ! |6|7|8|
    ! +-+-+-+
    
    ! Interior wall indexes:
    ! +-+-+-+
    ! | 0 1 |
    ! +2+3+4+
    ! | 5 6 |
    ! +7+8+9+
    ! | 10 11 |
    ! +-+-+-+
    
    ! Define directions
    integer, parameter :: Left = 0
    integer, parameter :: Up = 1
    integer, parameter :: Right = 2
    integer, parameter :: Down = 3
    
    ! For each cell 0-8, indicate if a wall is exterior and cannot be removed (-1) or its interior index for each of
    ! the four directions, LEFT, UP, RIGHT, DOWN.
    integer, dimension (0:8, 0:3) :: cellToWallLUT
    
    ! 12 interior walls in a 3x3 maze. Start with all the walls up.
    logical, dimension (0:11) :: wallList
    
    ! Variables for maze processing
    integer :: currentInteriorWall
    integer :: cellIndex
    integer :: wallToRemove
    logical :: wallRemoved
    integer :: direction
    integer :: i
    real :: startTime, currentTime
    
    ! Initialize cellToWallLUT
    cellToWallLUT (0, :) = (/ -1, -1,  0,  2 /)
    cellToWallLUT (1, :) = (/  0, -1,  1,  3 /)
    cellToWallLUT (2, :) = (/  1, -1, -1,  4 /)
    cellToWallLUT (3, :) = (/ -1,  2,  5,  7 /)
    cellToWallLUT (4, :) = (/  5,  3,  6,  8 /)
    cellToWallLUT (5, :) = (/  6,  4, -1,  9 /)
    cellToWallLUT (6, :) = (/ -1,  7, 10, -1 /)
    cellToWallLUT (7, :) = (/ 10,  8, 11, -1 /)
    cellToWallLUT (8, :) = (/ 11,  9, -1, -1 /)
    
    ! Initialize all walls as up
    wallList = .true.
    
    ! Print initial maze
    write ( *, '(A)' ) "Initial Maze:"
    
    ! Print out the maze, this is a rather painful copy/paste job without loops and functions.
    ! Note write is meant to print characters with no newlines using advance='no'.
    ! Horizontal walls above row 1 - All are exterior walls, no conditions.
    currentInteriorWall = 0
    
    ! +-+-+-+
    write ( *, '(A)', advance='no' ) '+'
    write ( *, '(A)', advance='no' ) '-'
    write ( *, '(A)', advance='no' ) '+'
    write ( *, '(A)', advance='no' ) '-'
    write ( *, '(A)', advance='no' ) '+'
    write ( *, '(A)', advance='no' ) '-'
    write ( *, '(A)' ) '+'
    
    ! Vertical walls and cells row 1.
    ! The left and right vertical walls are exterior, always up.
    ! | | | |
    write ( *, '(A)', advance='no' ) '|'
    write ( *, '(A)', advance='no' ) ' '
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '|'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) ' '
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '|'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) ' '
    write ( *, '(A)' ) '|'
    
    ! Horizontal walls above row 2
    ! +-+-+-+
    write ( *, '(A)', advance='no' ) '+'
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '-'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) '+'
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '-'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) '+'
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '-'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)' ) '+'
    
    ! Vertical walls and cells row 2.
    ! The left and right vertical walls are exterior, always up.
    ! | | | |
    write ( *, '(A)', advance='no' ) '|'
    write ( *, '(A)', advance='no' ) ' '
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '|'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) ' '
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '|'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) ' '
    write ( *, '(A)' ) '|'
    
    ! Horizontal walls above row 3
    ! +-+-+-+
    write ( *, '(A)', advance='no' ) '+'
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '-'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) '+'
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '-'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) '+'
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '-'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)' ) '+'
    
    ! Vertical walls and cells row 3.
    ! The left and right vertical walls are exterior, always up.
    ! | | | |
    write ( *, '(A)', advance='no' ) '|'
    write ( *, '(A)', advance='no' ) ' '
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '|'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) ' '
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '|'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) ' '
    write ( *, '(A)' ) '|'
    
    ! Horizontal walls below row 3 - All are exterior walls, no conditions.
    ! +-+-+-+
    write ( *, '(A)', advance='no' ) '+'
    write ( *, '(A)', advance='no' ) '-'
    write ( *, '(A)', advance='no' ) '+'
    write ( *, '(A)', advance='no' ) '-'
    write ( *, '(A)', advance='no' ) '+'
    write ( *, '(A)', advance='no' ) '-'
    write ( *, '(A)' ) '+'
    
    ! Now process each cell 0-8
    ! Cell 0
    write ( *, '(A)' ) ""
    write ( *, '(A)' ) "Removing wall from cell 0:"
    
    ! Remove wall code:
    ! Remove a cell wall if possible for cell 0
    cellIndex = 0
    
    wallRemoved = .false.
    direction = Left
    
    ! If the wall in this direction is NOT an exterior wall
    if ( cellToWallLUT (cellIndex, direction) >= 0 ) then
        wallToRemove = cellToWallLUT (cellIndex, direction)
        ! If this wall has not been already removed
        if ( wallList (wallToRemove) .eqv. .true. ) then
            ! Remove the wall and indicate a wall was successfully removed
            wallList (wallToRemove) = .false.
            wallRemoved = .true.
        end if
    end if
    
    ! If we haven't removed a wall yet, try to remove one in the next direction
    ! Check the guard, have we removed one wall
    if ( wallRemoved .eqv. .false. ) then
        direction = direction + 1
        ! If the wall in this direction is NOT an exterior wall
        if ( cellToWallLUT (cellIndex, direction) >= 0 ) then
            wallToRemove = cellToWallLUT (cellIndex, direction)
            ! If this wall has not been already removed
            if ( wallList (wallToRemove) .eqv. .true. ) then
                ! Remove the wall and indicate a wall was successfully removed
                wallList (wallToRemove) = .false.
                wallRemoved = .true.
            end if
        end if
    end if
    
    ! If we haven't removed a wall yet, try to remove one in the next direction
    ! Check the guard, have we removed one wall
    if ( wallRemoved .eqv. .false. ) then
        direction = direction + 1
        ! If the wall in this direction is NOT an exterior wall
        if ( cellToWallLUT (cellIndex, direction) >= 0 ) then
            wallToRemove = cellToWallLUT (cellIndex, direction)
            ! If this wall has not been already removed
            if ( wallList (wallToRemove) .eqv. .true. ) then
                ! Remove the wall and indicate a wall was successfully removed
                wallList (wallToRemove) = .false.
                wallRemoved = .true.
            end if
        end if
    end if
    
    ! If we haven't removed a wall yet, try to remove one in the next direction
    ! Check the guard, have we removed one wall
    if ( wallRemoved .eqv. .false. ) then
        direction = direction + 1
        ! If the wall in this direction is NOT an exterior wall
        if ( cellToWallLUT (cellIndex, direction) >= 0 ) then
            wallToRemove = cellToWallLUT (cellIndex, direction)
            ! If this wall has not been already removed
            if ( wallList (wallToRemove) .eqv. .true. ) then
                ! Remove the wall and indicate a wall was successfully removed
                wallList (wallToRemove) = .false.
                wallRemoved = .true.
            end if
        end if
    end if
    
    ! Print maze after cell 0
    ! Simple delay using busy wait (mimicking the original thread sleep)
    call cpu_time ( startTime )
    do
        call cpu_time ( currentTime )
        if ( currentTime - startTime >= 0.5 ) exit
    end do
    
    ! Print maze (duplicate of above print code)
    currentInteriorWall = 0
    
    ! +-+-+-+
    write ( *, '(A)', advance='no' ) '+'
    write ( *, '(A)', advance='no' ) '-'
    write ( *, '(A)', advance='no' ) '+'
    write ( *, '(A)', advance='no' ) '-'
    write ( *, '(A)', advance='no' ) '+'
    write ( *, '(A)', advance='no' ) '-'
    write ( *, '(A)' ) '+'
    
    ! Row 1 vertical walls and cells
    write ( *, '(A)', advance='no' ) '|'
    write ( *, '(A)', advance='no' ) ' '
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '|'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) ' '
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '|'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) ' '
    write ( *, '(A)' ) '|'
    
    ! Row 2 horizontal walls
    write ( *, '(A)', advance='no' ) '+'
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '-'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) '+'
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '-'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) '+'
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '-'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)' ) '+'
    
    ! Row 2 vertical walls and cells
    write ( *, '(A)', advance='no' ) '|'
    write ( *, '(A)', advance='no' ) ' '
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '|'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) ' '
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '|'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) ' '
    write ( *, '(A)' ) '|'
    
    ! Row 3 horizontal walls
    write ( *, '(A)', advance='no' ) '+'
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '-'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) '+'
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '-'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) '+'
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '-'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)' ) '+'
    
    ! Row 3 vertical walls and cells
    write ( *, '(A)', advance='no' ) '|'
    write ( *, '(A)', advance='no' ) ' '
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '|'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) ' '
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '|'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) ' '
    write ( *, '(A)' ) '|'
    
    ! Bottom border
    write ( *, '(A)', advance='no' ) '+'
    write ( *, '(A)', advance='no' ) '-'
    write ( *, '(A)', advance='no' ) '+'
    write ( *, '(A)', advance='no' ) '-'
    write ( *, '(A)', advance='no' ) '+'
    write ( *, '(A)', advance='no' ) '-'
    write ( *, '(A)' ) '+'
    
    ! Cell 1
    write ( *, '(A)' ) ""
    write ( *, '(A)' ) "Removing wall from cell 1:"
    
    ! Remove wall code:
    ! Remove a cell wall if possible for cell 1
    cellIndex = 1
    
    wallRemoved = .false.
    direction = Left
    
    ! If the wall in this direction is NOT an exterior wall
    if ( cellToWallLUT (cellIndex, direction) >= 0 ) then
        wallToRemove = cellToWallLUT (cellIndex, direction)
        ! If this wall has not been already removed
        if ( wallList (wallToRemove) .eqv. .true. ) then
            ! Remove the wall and indicate a wall was successfully removed
            wallList (wallToRemove) = .false.
            wallRemoved = .true.
        end if
    end if
    
    ! If we haven't removed a wall yet, try to remove one in the next direction
    ! Check the guard, have we removed one wall
    if ( wallRemoved .eqv. .false. ) then
        direction = direction + 1
        ! If the wall in this direction is NOT an exterior wall
        if ( cellToWallLUT (cellIndex, direction) >= 0 ) then
            wallToRemove = cellToWallLUT (cellIndex, direction)
            ! If this wall has not been already removed
            if ( wallList (wallToRemove) .eqv. .true. ) then
                ! Remove the wall and indicate a wall was successfully removed
                wallList (wallToRemove) = .false.
                wallRemoved = .true.
            end if
        end if
    end if
    
    ! If we haven't removed a wall yet, try to remove one in the next direction
    ! Check the guard, have we removed one wall
    if ( wallRemoved .eqv. .false. ) then
        direction = direction + 1
        ! If the wall in this direction is NOT an exterior wall
        if ( cellToWallLUT (cellIndex, direction) >= 0 ) then
            wallToRemove = cellToWallLUT (cellIndex, direction)
            ! If this wall has not been already removed
            if ( wallList (wallToRemove) .eqv. .true. ) then
                ! Remove the wall and indicate a wall was successfully removed
                wallList (wallToRemove) = .false.
                wallRemoved = .true.
            end if
        end if
    end if
    
    ! If we haven't removed a wall yet, try to remove one in the next direction
    ! Check the guard, have we removed one wall
    if ( wallRemoved .eqv. .false. ) then
        direction = direction + 1
        ! If the wall in this direction is NOT an exterior wall
        if ( cellToWallLUT (cellIndex, direction) >= 0 ) then
            wallToRemove = cellToWallLUT (cellIndex, direction)
            ! If this wall has not been already removed
            if ( wallList (wallToRemove) .eqv. .true. ) then
                ! Remove the wall and indicate a wall was successfully removed
                wallList (wallToRemove) = .false.
                wallRemoved = .true.
            end if
        end if
    end if
    
    ! Print maze after cell 1
    ! Simple delay using busy wait (mimicking the original thread sleep)
    call cpu_time ( startTime )
    do
        call cpu_time ( currentTime )
        if ( currentTime - startTime >= 0.5 ) exit
    end do
    
    ! Print maze (duplicate of above print code)
    currentInteriorWall = 0
    
    ! +-+-+-+
    write ( *, '(A)', advance='no' ) '+'
    write ( *, '(A)', advance='no' ) '-'
    write ( *, '(A)', advance='no' ) '+'
    write ( *, '(A)', advance='no' ) '-'
    write ( *, '(A)', advance='no' ) '+'
    write ( *, '(A)', advance='no' ) '-'
    write ( *, '(A)' ) '+'
    
    ! Row 1 vertical walls and cells
    write ( *, '(A)', advance='no' ) '|'
    write ( *, '(A)', advance='no' ) ' '
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '|'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) ' '
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '|'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) ' '
    write ( *, '(A)' ) '|'
    
    ! Row 2 horizontal walls
    write ( *, '(A)', advance='no' ) '+'
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '-'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) '+'
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '-'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) '+'
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '-'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)' ) '+'
    
    ! Row 2 vertical walls and cells
    write ( *, '(A)', advance='no' ) '|'
    write ( *, '(A)', advance='no' ) ' '
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '|'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) ' '
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '|'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) ' '
    write ( *, '(A)' ) '|'
    
    ! Row 3 horizontal walls
    write ( *, '(A)', advance='no' ) '+'
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '-'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) '+'
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '-'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) '+'
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '-'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)' ) '+'
    
    ! Row 3 vertical walls and cells
    write ( *, '(A)', advance='no' ) '|'
    write ( *, '(A)', advance='no' ) ' '
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '|'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) ' '
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '|'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) ' '
    write ( *, '(A)' ) '|'
    
    ! Bottom border
    write ( *, '(A)', advance='no' ) '+'
    write ( *, '(A)', advance='no' ) '-'
    write ( *, '(A)', advance='no' ) '+'
    write ( *, '(A)', advance='no' ) '-'
    write ( *, '(A)', advance='no' ) '+'
    write ( *, '(A)', advance='no' ) '-'
    write ( *, '(A)' ) '+'
    
    ! Cell 2
    write ( *, '(A)' ) ""
    write ( *, '(A)' ) "Removing wall from cell 2:"
    
    ! Remove wall code:
    ! Remove a cell wall if possible for cell 2
    cellIndex = 2
    
    wallRemoved = .false.
    direction = Left
    
    ! If the wall in this direction is NOT an exterior wall
    if ( cellToWallLUT (cellIndex, direction) >= 0 ) then
        wallToRemove = cellToWallLUT (cellIndex, direction)
        ! If this wall has not been already removed
        if ( wallList (wallToRemove) .eqv. .true. ) then
            ! Remove the wall and indicate a wall was successfully removed
            wallList (wallToRemove) = .false.
            wallRemoved = .true.
        end if
    end if
    
    ! If we haven't removed a wall yet, try to remove one in the next direction
    ! Check the guard, have we removed one wall
    if ( wallRemoved .eqv. .false. ) then
        direction = direction + 1
        ! If the wall in this direction is NOT an exterior wall
        if ( cellToWallLUT (cellIndex, direction) >= 0 ) then
            wallToRemove = cellToWallLUT (cellIndex, direction)
            ! If this wall has not been already removed
            if ( wallList (wallToRemove) .eqv. .true. ) then
                ! Remove the wall and indicate a wall was successfully removed
                wallList (wallToRemove) = .false.
                wallRemoved = .true.
            end if
        end if
    end if
    
    ! If we haven't removed a wall yet, try to remove one in the next direction
    ! Check the guard, have we removed one wall
    if ( wallRemoved .eqv. .false. ) then
        direction = direction + 1
        ! If the wall in this direction is NOT an exterior wall
        if ( cellToWallLUT (cellIndex, direction) >= 0 ) then
            wallToRemove = cellToWallLUT (cellIndex, direction)
            ! If this wall has not been already removed
            if ( wallList (wallToRemove) .eqv. .true. ) then
                ! Remove the wall and indicate a wall was successfully removed
                wallList (wallToRemove) = .false.
                wallRemoved = .true.
            end if
        end if
    end if
    
    ! If we haven't removed a wall yet, try to remove one in the next direction
    ! Check the guard, have we removed one wall
    if ( wallRemoved .eqv. .false. ) then
        direction = direction + 1
        ! If the wall in this direction is NOT an exterior wall
        if ( cellToWallLUT (cellIndex, direction) >= 0 ) then
            wallToRemove = cellToWallLUT (cellIndex, direction)
            ! If this wall has not been already removed
            if ( wallList (wallToRemove) .eqv. .true. ) then
                ! Remove the wall and indicate a wall was successfully removed
                wallList (wallToRemove) = .false.
                wallRemoved = .true.
            end if
        end if
    end if
    
    ! Print maze after cell 2
    ! Simple delay using busy wait (mimicking the original thread sleep)
    call cpu_time ( startTime )
    do
        call cpu_time ( currentTime )
        if ( currentTime - startTime >= 0.5 ) exit
    end do
    
    ! Print maze (duplicate of above print code)
    currentInteriorWall = 0
    
    ! +-+-+-+
    write ( *, '(A)', advance='no' ) '+'
    write ( *, '(A)', advance='no' ) '-'
    write ( *, '(A)', advance='no' ) '+'
    write ( *, '(A)', advance='no' ) '-'
    write ( *, '(A)', advance='no' ) '+'
    write ( *, '(A)', advance='no' ) '-'
    write ( *, '(A)' ) '+'
    
    ! Row 1 vertical walls and cells
    write ( *, '(A)', advance='no' ) '|'
    write ( *, '(A)', advance='no' ) ' '
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '|'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) ' '
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '|'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) ' '
    write ( *, '(A)' ) '|'
    
    ! Row 2 horizontal walls
    write ( *, '(A)', advance='no' ) '+'
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '-'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) '+'
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '-'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) '+'
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '-'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)' ) '+'
    
    ! Row 2 vertical walls and cells
    write ( *, '(A)', advance='no' ) '|'
    write ( *, '(A)', advance='no' ) ' '
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '|'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) ' '
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '|'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) ' '
    write ( *, '(A)' ) '|'
    
    ! Row 3 horizontal walls
    write ( *, '(A)', advance='no' ) '+'
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '-'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) '+'
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '-'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) '+'
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '-'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)' ) '+'
    
    ! Row 3 vertical walls and cells
    write ( *, '(A)', advance='no' ) '|'
    write ( *, '(A)', advance='no' ) ' '
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '|'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) ' '
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '|'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) ' '
    write ( *, '(A)' ) '|'
    
    ! Bottom border
    write ( *, '(A)', advance='no' ) '+'
    write ( *, '(A)', advance='no' ) '-'
    write ( *, '(A)', advance='no' ) '+'
    write ( *, '(A)', advance='no' ) '-'
    write ( *, '(A)', advance='no' ) '+'
    write ( *, '(A)', advance='no' ) '-'
    write ( *, '(A)' ) '+'
    
    ! Cell 3
    write ( *, '(A)' ) ""
    write ( *, '(A)' ) "Removing wall from cell 3:"
    
    ! Remove wall code:
    ! Remove a cell wall if possible for cell 3
    cellIndex = 3
    
    wallRemoved = .false.
    direction = Left
    
    ! If the wall in this direction is NOT an exterior wall
    if ( cellToWallLUT (cellIndex, direction) >= 0 ) then
        wallToRemove = cellToWallLUT (cellIndex, direction)
        ! If this wall has not been already removed
        if ( wallList (wallToRemove) .eqv. .true. ) then
            ! Remove the wall and indicate a wall was successfully removed
            wallList (wallToRemove) = .false.
            wallRemoved = .true.
        end if
    end if
    
    ! If we haven't removed a wall yet, try to remove one in the next direction
    ! Check the guard, have we removed one wall
    if ( wallRemoved .eqv. .false. ) then
        direction = direction + 1
        ! If the wall in this direction is NOT an exterior wall
        if ( cellToWallLUT (cellIndex, direction) >= 0 ) then
            wallToRemove = cellToWallLUT (cellIndex, direction)
            ! If this wall has not been already removed
            if ( wallList (wallToRemove) .eqv. .true. ) then
                ! Remove the wall and indicate a wall was successfully removed
                wallList (wallToRemove) = .false.
                wallRemoved = .true.
            end if
        end if
    end if
    
    ! If we haven't removed a wall yet, try to remove one in the next direction
    ! Check the guard, have we removed one wall
    if ( wallRemoved .eqv. .false. ) then
        direction = direction + 1
        ! If the wall in this direction is NOT an exterior wall
        if ( cellToWallLUT (cellIndex, direction) >= 0 ) then
            wallToRemove = cellToWallLUT (cellIndex, direction)
            ! If this wall has not been already removed
            if ( wallList (wallToRemove) .eqv. .true. ) then
                ! Remove the wall and indicate a wall was successfully removed
                wallList (wallToRemove) = .false.
                wallRemoved = .true.
            end if
        end if
    end if
    
    ! If we haven't removed a wall yet, try to remove one in the next direction
    ! Check the guard, have we removed one wall
    if ( wallRemoved .eqv. .false. ) then
        direction = direction + 1
        ! If the wall in this direction is NOT an exterior wall
        if ( cellToWallLUT (cellIndex, direction) >= 0 ) then
            wallToRemove = cellToWallLUT (cellIndex, direction)
            ! If this wall has not been already removed
            if ( wallList (wallToRemove) .eqv. .true. ) then
                ! Remove the wall and indicate a wall was successfully removed
                wallList (wallToRemove) = .false.
                wallRemoved = .true.
            end if
        end if
    end if
    
    ! Print maze after cell 3
    ! Simple delay using busy wait (mimicking the original thread sleep)
    call cpu_time ( startTime )
    do
        call cpu_time ( currentTime )
        if ( currentTime - startTime >= 0.5 ) exit
    end do
    
    ! Print maze (duplicate of above print code)
    currentInteriorWall = 0
    
    ! +-+-+-+
    write ( *, '(A)', advance='no' ) '+'
    write ( *, '(A)', advance='no' ) '-'
    write ( *, '(A)', advance='no' ) '+'
    write ( *, '(A)', advance='no' ) '-'
    write ( *, '(A)', advance='no' ) '+'
    write ( *, '(A)', advance='no' ) '-'
    write ( *, '(A)' ) '+'
    
    ! Row 1 vertical walls and cells
    write ( *, '(A)', advance='no' ) '|'
    write ( *, '(A)', advance='no' ) ' '
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '|'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) ' '
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '|'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) ' '
    write ( *, '(A)' ) '|'
    
    ! Row 2 horizontal walls
    write ( *, '(A)', advance='no' ) '+'
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '-'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) '+'
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '-'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) '+'
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '-'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)' ) '+'
    
    ! Row 2 vertical walls and cells
    write ( *, '(A)', advance='no' ) '|'
    write ( *, '(A)', advance='no' ) ' '
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '|'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) ' '
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '|'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) ' '
    write ( *, '(A)' ) '|'
    
    ! Row 3 horizontal walls
    write ( *, '(A)', advance='no' ) '+'
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '-'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) '+'
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '-'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) '+'
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '-'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)' ) '+'
    
    ! Row 3 vertical walls and cells
    write ( *, '(A)', advance='no' ) '|'
    write ( *, '(A)', advance='no' ) ' '
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '|'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) ' '
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '|'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) ' '
    write ( *, '(A)' ) '|'
    
    ! Bottom border
    write ( *, '(A)', advance='no' ) '+'
    write ( *, '(A)', advance='no' ) '-'
    write ( *, '(A)', advance='no' ) '+'
    write ( *, '(A)', advance='no' ) '-'
    write ( *, '(A)', advance='no' ) '+'
    write ( *, '(A)', advance='no' ) '-'
    write ( *, '(A)' ) '+'
    
    ! Cell 4
    write ( *, '(A)' ) ""
    write ( *, '(A)' ) "Removing wall from cell 4:"
    
    ! Remove wall code:
    ! Remove a cell wall if possible for cell 4
    cellIndex = 4
    
    wallRemoved = .false.
    direction = Left
    
    ! If the wall in this direction is NOT an exterior wall
    if ( cellToWallLUT (cellIndex, direction) >= 0 ) then
        wallToRemove = cellToWallLUT (cellIndex, direction)
        ! If this wall has not been already removed
        if ( wallList (wallToRemove) .eqv. .true. ) then
            ! Remove the wall and indicate a wall was successfully removed
            wallList (wallToRemove) = .false.
            wallRemoved = .true.
        end if
    end if
    
    ! If we haven't removed a wall yet, try to remove one in the next direction
    ! Check the guard, have we removed one wall
    if ( wallRemoved .eqv. .false. ) then
        direction = direction + 1
        ! If the wall in this direction is NOT an exterior wall
        if ( cellToWallLUT (cellIndex, direction) >= 0 ) then
            wallToRemove = cellToWallLUT (cellIndex, direction)
            ! If this wall has not been already removed
            if ( wallList (wallToRemove) .eqv. .true. ) then
                ! Remove the wall and indicate a wall was successfully removed
                wallList (wallToRemove) = .false.
                wallRemoved = .true.
            end if
        end if
    end if
    
    ! If we haven't removed a wall yet, try to remove one in the next direction
    ! Check the guard, have we removed one wall
    if ( wallRemoved .eqv. .false. ) then
        direction = direction + 1
        ! If the wall in this direction is NOT an exterior wall
        if ( cellToWallLUT (cellIndex, direction) >= 0 ) then
            wallToRemove = cellToWallLUT (cellIndex, direction)
            ! If this wall has not been already removed
            if ( wallList (wallToRemove) .eqv. .true. ) then
                ! Remove the wall and indicate a wall was successfully removed
                wallList (wallToRemove) = .false.
                wallRemoved = .true.
            end if
        end if
    end if
    
    ! If we haven't removed a wall yet, try to remove one in the next direction
    ! Check the guard, have we removed one wall
    if ( wallRemoved .eqv. .false. ) then
        direction = direction + 1
        ! If the wall in this direction is NOT an exterior wall
        if ( cellToWallLUT (cellIndex, direction) >= 0 ) then
            wallToRemove = cellToWallLUT (cellIndex, direction)
            ! If this wall has not been already removed
            if ( wallList (wallToRemove) .eqv. .true. ) then
                ! Remove the wall and indicate a wall was successfully removed
                wallList (wallToRemove) = .false.
                wallRemoved = .true.
            end if
        end if
    end if
    
    ! Print maze after cell 4
    ! Simple delay using busy wait (mimicking the original thread sleep)
    call cpu_time ( startTime )
    do
        call cpu_time ( currentTime )
        if ( currentTime - startTime >= 0.5 ) exit
    end do
    
    ! Print maze (duplicate of above print code)
    currentInteriorWall = 0
    
    ! +-+-+-+
    write ( *, '(A)', advance='no' ) '+'
    write ( *, '(A)', advance='no' ) '-'
    write ( *, '(A)', advance='no' ) '+'
    write ( *, '(A)', advance='no' ) '-'
    write ( *, '(A)', advance='no' ) '+'
    write ( *, '(A)', advance='no' ) '-'
    write ( *, '(A)' ) '+'
    
    ! Row 1 vertical walls and cells
    write ( *, '(A)', advance='no' ) '|'
    write ( *, '(A)', advance='no' ) ' '
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '|'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) ' '
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '|'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) ' '
    write ( *, '(A)' ) '|'
    
    ! Row 2 horizontal walls
    write ( *, '(A)', advance='no' ) '+'
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '-'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) '+'
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '-'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) '+'
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '-'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)' ) '+'
    
    ! Row 2 vertical walls and cells
    write ( *, '(A)', advance='no' ) '|'
    write ( *, '(A)', advance='no' ) ' '
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '|'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) ' '
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '|'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) ' '
    write ( *, '(A)' ) '|'
    
    ! Row 3 horizontal walls
    write ( *, '(A)', advance='no' ) '+'
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '-'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) '+'
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '-'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) '+'
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '-'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)' ) '+'
    
    ! Row 3 vertical walls and cells
    write ( *, '(A)', advance='no' ) '|'
    write ( *, '(A)', advance='no' ) ' '
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '|'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) ' '
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '|'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) ' '
    write ( *, '(A)' ) '|'
    
    ! Bottom border
    write ( *, '(A)', advance='no' ) '+'
    write ( *, '(A)', advance='no' ) '-'
    write ( *, '(A)', advance='no' ) '+'
    write ( *, '(A)', advance='no' ) '-'
    write ( *, '(A)', advance='no' ) '+'
    write ( *, '(A)', advance='no' ) '-'
    write ( *, '(A)' ) '+'
    
    ! Cell 5
    write ( *, '(A)' ) ""
    write ( *, '(A)' ) "Removing wall from cell 5:"
    
    ! Remove wall code:
    ! Remove a cell wall if possible for cell 5
    cellIndex = 5
    
    wallRemoved = .false.
    direction = Left
    
    ! If the wall in this direction is NOT an exterior wall
    if ( cellToWallLUT (cellIndex, direction) >= 0 ) then
        wallToRemove = cellToWallLUT (cellIndex, direction)
        ! If this wall has not been already removed
        if ( wallList (wallToRemove) .eqv. .true. ) then
            ! Remove the wall and indicate a wall was successfully removed
            wallList (wallToRemove) = .false.
            wallRemoved = .true.
        end if
    end if
    
    ! If we haven't removed a wall yet, try to remove one in the next direction
    ! Check the guard, have we removed one wall
    if ( wallRemoved .eqv. .false. ) then
        direction = direction + 1
        ! If the wall in this direction is NOT an exterior wall
        if ( cellToWallLUT (cellIndex, direction) >= 0 ) then
            wallToRemove = cellToWallLUT (cellIndex, direction)
            ! If this wall has not been already removed
            if ( wallList (wallToRemove) .eqv. .true. ) then
                ! Remove the wall and indicate a wall was successfully removed
                wallList (wallToRemove) = .false.
                wallRemoved = .true.
            end if
        end if
    end if
    
    ! If we haven't removed a wall yet, try to remove one in the next direction
    ! Check the guard, have we removed one wall
    if ( wallRemoved .eqv. .false. ) then
        direction = direction + 1
        ! If the wall in this direction is NOT an exterior wall
        if ( cellToWallLUT (cellIndex, direction) >= 0 ) then
            wallToRemove = cellToWallLUT (cellIndex, direction)
            ! If this wall has not been already removed
            if ( wallList (wallToRemove) .eqv. .true. ) then
                ! Remove the wall and indicate a wall was successfully removed
                wallList (wallToRemove) = .false.
                wallRemoved = .true.
            end if
        end if
    end if
    
    ! If we haven't removed a wall yet, try to remove one in the next direction
    ! Check the guard, have we removed one wall
    if ( wallRemoved .eqv. .false. ) then
        direction = direction + 1
        ! If the wall in this direction is NOT an exterior wall
        if ( cellToWallLUT (cellIndex, direction) >= 0 ) then
            wallToRemove = cellToWallLUT (cellIndex, direction)
            ! If this wall has not been already removed
            if ( wallList (wallToRemove) .eqv. .true. ) then
                ! Remove the wall and indicate a wall was successfully removed
                wallList (wallToRemove) = .false.
                wallRemoved = .true.
            end if
        end if
    end if
    
    ! Print maze after cell 5
    ! Simple delay using busy wait (mimicking the original thread sleep)
    call cpu_time ( startTime )
    do
        call cpu_time ( currentTime )
        if ( currentTime - startTime >= 0.5 ) exit
    end do
    
    ! Print maze (duplicate of above print code)
    currentInteriorWall = 0
    
    ! +-+-+-+
    write ( *, '(A)', advance='no' ) '+'
    write ( *, '(A)', advance='no' ) '-'
    write ( *, '(A)', advance='no' ) '+'
    write ( *, '(A)', advance='no' ) '-'
    write ( *, '(A)', advance='no' ) '+'
    write ( *, '(A)', advance='no' ) '-'
    write ( *, '(A)' ) '+'
    
    ! Row 1 vertical walls and cells
    write ( *, '(A)', advance='no' ) '|'
    write ( *, '(A)', advance='no' ) ' '
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '|'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) ' '
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '|'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) ' '
    write ( *, '(A)' ) '|'
    
    ! Row 2 horizontal walls
    write ( *, '(A)', advance='no' ) '+'
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '-'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) '+'
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '-'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) '+'
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '-'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)' ) '+'
    
    ! Row 2 vertical walls and cells
    write ( *, '(A)', advance='no' ) '|'
    write ( *, '(A)', advance='no' ) ' '
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '|'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) ' '
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '|'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) ' '
    write ( *, '(A)' ) '|'
    
    ! Row 3 horizontal walls
    write ( *, '(A)', advance='no' ) '+'
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '-'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) '+'
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '-'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) '+'
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '-'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)' ) '+'
    
    ! Row 3 vertical walls and cells
    write ( *, '(A)', advance='no' ) '|'
    write ( *, '(A)', advance='no' ) ' '
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '|'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) ' '
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '|'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) ' '
    write ( *, '(A)' ) '|'
    
    ! Bottom border
    write ( *, '(A)', advance='no' ) '+'
    write ( *, '(A)', advance='no' ) '-'
    write ( *, '(A)', advance='no' ) '+'
    write ( *, '(A)', advance='no' ) '-'
    write ( *, '(A)', advance='no' ) '+'
    write ( *, '(A)', advance='no' ) '-'
    write ( *, '(A)' ) '+'
    
    ! Cell 6
    write ( *, '(A)' ) ""
    write ( *, '(A)' ) "Removing wall from cell 6:"
    
    ! Remove wall code:
    ! Remove a cell wall if possible for cell 6
    cellIndex = 6
    
    wallRemoved = .false.
    direction = Left
    
    ! If the wall in this direction is NOT an exterior wall
    if ( cellToWallLUT (cellIndex, direction) >= 0 ) then
        wallToRemove = cellToWallLUT (cellIndex, direction)
        ! If this wall has not been already removed
        if ( wallList (wallToRemove) .eqv. .true. ) then
            ! Remove the wall and indicate a wall was successfully removed
            wallList (wallToRemove) = .false.
            wallRemoved = .true.
        end if
    end if
    
    ! If we haven't removed a wall yet, try to remove one in the next direction
    ! Check the guard, have we removed one wall
    if ( wallRemoved .eqv. .false. ) then
        direction = direction + 1
        ! If the wall in this direction is NOT an exterior wall
        if ( cellToWallLUT (cellIndex, direction) >= 0 ) then
            wallToRemove = cellToWallLUT (cellIndex, direction)
            ! If this wall has not been already removed
            if ( wallList (wallToRemove) .eqv. .true. ) then
                ! Remove the wall and indicate a wall was successfully removed
                wallList (wallToRemove) = .false.
                wallRemoved = .true.
            end if
        end if
    end if
    
    ! If we haven't removed a wall yet, try to remove one in the next direction
    ! Check the guard, have we removed one wall
    if ( wallRemoved .eqv. .false. ) then
        direction = direction + 1
        ! If the wall in this direction is NOT an exterior wall
        if ( cellToWallLUT (cellIndex, direction) >= 0 ) then
            wallToRemove = cellToWallLUT (cellIndex, direction)
            ! If this wall has not been already removed
            if ( wallList (wallToRemove) .eqv. .true. ) then
                ! Remove the wall and indicate a wall was successfully removed
                wallList (wallToRemove) = .false.
                wallRemoved = .true.
            end if
        end if
    end if
    
    ! If we haven't removed a wall yet, try to remove one in the next direction
    ! Check the guard, have we removed one wall
    if ( wallRemoved .eqv. .false. ) then
        direction = direction + 1
        ! If the wall in this direction is NOT an exterior wall
        if ( cellToWallLUT (cellIndex, direction) >= 0 ) then
            wallToRemove = cellToWallLUT (cellIndex, direction)
            ! If this wall has not been already removed
            if ( wallList (wallToRemove) .eqv. .true. ) then
                ! Remove the wall and indicate a wall was successfully removed
                wallList (wallToRemove) = .false.
                wallRemoved = .true.
            end if
        end if
    end if
    
    ! Print maze after cell 6
    ! Simple delay using busy wait (mimicking the original thread sleep)
    call cpu_time ( startTime )
    do
        call cpu_time ( currentTime )
        if ( currentTime - startTime >= 0.5 ) exit
    end do
    
    ! Print maze (duplicate of above print code)
    currentInteriorWall = 0
    
    ! +-+-+-+
    write ( *, '(A)', advance='no' ) '+'
    write ( *, '(A)', advance='no' ) '-'
    write ( *, '(A)', advance='no' ) '+'
    write ( *, '(A)', advance='no' ) '-'
    write ( *, '(A)', advance='no' ) '+'
    write ( *, '(A)', advance='no' ) '-'
    write ( *, '(A)' ) '+'
    
    ! Row 1 vertical walls and cells
    write ( *, '(A)', advance='no' ) '|'
    write ( *, '(A)', advance='no' ) ' '
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '|'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) ' '
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '|'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) ' '
    write ( *, '(A)' ) '|'
    
    ! Row 2 horizontal walls
    write ( *, '(A)', advance='no' ) '+'
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '-'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) '+'
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '-'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) '+'
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '-'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)' ) '+'
    
    ! Row 2 vertical walls and cells
    write ( *, '(A)', advance='no' ) '|'
    write ( *, '(A)', advance='no' ) ' '
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '|'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) ' '
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '|'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) ' '
    write ( *, '(A)' ) '|'
    
    ! Row 3 horizontal walls
    write ( *, '(A)', advance='no' ) '+'
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '-'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) '+'
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '-'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) '+'
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '-'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)' ) '+'
    
    ! Row 3 vertical walls and cells
    write ( *, '(A)', advance='no' ) '|'
    write ( *, '(A)', advance='no' ) ' '
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '|'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) ' '
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '|'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) ' '
    write ( *, '(A)' ) '|'
    
    ! Bottom border
    write ( *, '(A)', advance='no' ) '+'
    write ( *, '(A)', advance='no' ) '-'
    write ( *, '(A)', advance='no' ) '+'
    write ( *, '(A)', advance='no' ) '-'
    write ( *, '(A)', advance='no' ) '+'
    write ( *, '(A)', advance='no' ) '-'
    write ( *, '(A)' ) '+'
    
    ! Cell 7
    write ( *, '(A)' ) ""
    write ( *, '(A)' ) "Removing wall from cell 7:"
    
    ! Remove wall code:
    ! Remove a cell wall if possible for cell 7
    cellIndex = 7
    
    wallRemoved = .false.
    direction = Left
    
    ! If the wall in this direction is NOT an exterior wall
    if ( cellToWallLUT (cellIndex, direction) >= 0 ) then
        wallToRemove = cellToWallLUT (cellIndex, direction)
        ! If this wall has not been already removed
        if ( wallList (wallToRemove) .eqv. .true. ) then
            ! Remove the wall and indicate a wall was successfully removed
            wallList (wallToRemove) = .false.
            wallRemoved = .true.
        end if
    end if
    
    ! If we haven't removed a wall yet, try to remove one in the next direction
    ! Check the guard, have we removed one wall
    if ( wallRemoved .eqv. .false. ) then
        direction = direction + 1
        ! If the wall in this direction is NOT an exterior wall
        if ( cellToWallLUT (cellIndex, direction) >= 0 ) then
            wallToRemove = cellToWallLUT (cellIndex, direction)
            ! If this wall has not been already removed
            if ( wallList (wallToRemove) .eqv. .true. ) then
                ! Remove the wall and indicate a wall was successfully removed
                wallList (wallToRemove) = .false.
                wallRemoved = .true.
            end if
        end if
    end if
    
    ! If we haven't removed a wall yet, try to remove one in the next direction
    ! Check the guard, have we removed one wall
    if ( wallRemoved .eqv. .false. ) then
        direction = direction + 1
        ! If the wall in this direction is NOT an exterior wall
        if ( cellToWallLUT (cellIndex, direction) >= 0 ) then
            wallToRemove = cellToWallLUT (cellIndex, direction)
            ! If this wall has not been already removed
            if ( wallList (wallToRemove) .eqv. .true. ) then
                ! Remove the wall and indicate a wall was successfully removed
                wallList (wallToRemove) = .false.
                wallRemoved = .true.
            end if
        end if
    end if
    
    ! If we haven't removed a wall yet, try to remove one in the next direction
    ! Check the guard, have we removed one wall
    if ( wallRemoved .eqv. .false. ) then
        direction = direction + 1
        ! If the wall in this direction is NOT an exterior wall
        if ( cellToWallLUT (cellIndex, direction) >= 0 ) then
            wallToRemove = cellToWallLUT (cellIndex, direction)
            ! If this wall has not been already removed
            if ( wallList (wallToRemove) .eqv. .true. ) then
                ! Remove the wall and indicate a wall was successfully removed
                wallList (wallToRemove) = .false.
                wallRemoved = .true.
            end if
        end if
    end if
    
    ! Print maze after cell 7
    ! Simple delay using busy wait (mimicking the original thread sleep)
    call cpu_time ( startTime )
    do
        call cpu_time ( currentTime )
        if ( currentTime - startTime >= 0.5 ) exit
    end do
    
    ! Print maze (duplicate of above print code)
    currentInteriorWall = 0
    
    ! +-+-+-+
    write ( *, '(A)', advance='no' ) '+'
    write ( *, '(A)', advance='no' ) '-'
    write ( *, '(A)', advance='no' ) '+'
    write ( *, '(A)', advance='no' ) '-'
    write ( *, '(A)', advance='no' ) '+'
    write ( *, '(A)', advance='no' ) '-'
    write ( *, '(A)' ) '+'
    
    ! Row 1 vertical walls and cells
    write ( *, '(A)', advance='no' ) '|'
    write ( *, '(A)', advance='no' ) ' '
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '|'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) ' '
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '|'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) ' '
    write ( *, '(A)' ) '|'
    
    ! Row 2 horizontal walls
    write ( *, '(A)', advance='no' ) '+'
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '-'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) '+'
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '-'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) '+'
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '-'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)' ) '+'
    
    ! Row 2 vertical walls and cells
    write ( *, '(A)', advance='no' ) '|'
    write ( *, '(A)', advance='no' ) ' '
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '|'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) ' '
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '|'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) ' '
    write ( *, '(A)' ) '|'
    
    ! Row 3 horizontal walls
    write ( *, '(A)', advance='no' ) '+'
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '-'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) '+'
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '-'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) '+'
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '-'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)' ) '+'
    
    ! Row 3 vertical walls and cells
    write ( *, '(A)', advance='no' ) '|'
    write ( *, '(A)', advance='no' ) ' '
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '|'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) ' '
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '|'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) ' '
    write ( *, '(A)' ) '|'
    
    ! Bottom border
    write ( *, '(A)', advance='no' ) '+'
    write ( *, '(A)', advance='no' ) '-'
    write ( *, '(A)', advance='no' ) '+'
    write ( *, '(A)', advance='no' ) '-'
    write ( *, '(A)', advance='no' ) '+'
    write ( *, '(A)', advance='no' ) '-'
    write ( *, '(A)' ) '+'
    
    ! Cell 8
    write ( *, '(A)' ) ""
    write ( *, '(A)' ) "Removing wall from cell 8:"
    
    ! Remove wall code:
    ! Remove a cell wall if possible for cell 8
    cellIndex = 8
    
    wallRemoved = .false.
    direction = Left
    
    ! If the wall in this direction is NOT an exterior wall
    if ( cellToWallLUT (cellIndex, direction) >= 0 ) then
        wallToRemove = cellToWallLUT (cellIndex, direction)
        ! If this wall has not been already removed
        if ( wallList (wallToRemove) .eqv. .true. ) then
            ! Remove the wall and indicate a wall was successfully removed
            wallList (wallToRemove) = .false.
            wallRemoved = .true.
        end if
    end if
    
    ! If we haven't removed a wall yet, try to remove one in the next direction
    ! Check the guard, have we removed one wall
    if ( wallRemoved .eqv. .false. ) then
        direction = direction + 1
        ! If the wall in this direction is NOT an exterior wall
        if ( cellToWallLUT (cellIndex, direction) >= 0 ) then
            wallToRemove = cellToWallLUT (cellIndex, direction)
            ! If this wall has not been already removed
            if ( wallList (wallToRemove) .eqv. .true. ) then
                ! Remove the wall and indicate a wall was successfully removed
                wallList (wallToRemove) = .false.
                wallRemoved = .true.
            end if
        end if
    end if
    
    ! If we haven't removed a wall yet, try to remove one in the next direction
    ! Check the guard, have we removed one wall
    if ( wallRemoved .eqv. .false. ) then
        direction = direction + 1
        ! If the wall in this direction is NOT an exterior wall
        if ( cellToWallLUT (cellIndex, direction) >= 0 ) then
            wallToRemove = cellToWallLUT (cellIndex, direction)
            ! If this wall has not been already removed
            if ( wallList (wallToRemove) .eqv. .true. ) then
                ! Remove the wall and indicate a wall was successfully removed
                wallList (wallToRemove) = .false.
                wallRemoved = .true.
            end if
        end if
    end if
    
    ! If we haven't removed a wall yet, try to remove one in the next direction
    ! Check the guard, have we removed one wall
    if ( wallRemoved .eqv. .false. ) then
        direction = direction + 1
        ! If the wall in this direction is NOT an exterior wall
        if ( cellToWallLUT (cellIndex, direction) >= 0 ) then
            wallToRemove = cellToWallLUT (cellIndex, direction)
            ! If this wall has not been already removed
            if ( wallList (wallToRemove) .eqv. .true. ) then
                ! Remove the wall and indicate a wall was successfully removed
                wallList (wallToRemove) = .false.
                wallRemoved = .true.
            end if
        end if
    end if
    
    ! Print maze after cell 8
    ! Simple delay using busy wait (mimicking the original thread sleep)
    call cpu_time ( startTime )
    do
        call cpu_time ( currentTime )
        if ( currentTime - startTime >= 0.5 ) exit
    end do
    
    ! Print maze (duplicate of above print code)
    currentInteriorWall = 0
    
    ! +-+-+-+
    write ( *, '(A)', advance='no' ) '+'
    write ( *, '(A)', advance='no' ) '-'
    write ( *, '(A)', advance='no' ) '+'
    write ( *, '(A)', advance='no' ) '-'
    write ( *, '(A)', advance='no' ) '+'
    write ( *, '(A)', advance='no' ) '-'
    write ( *, '(A)' ) '+'
    
    ! Row 1 vertical walls and cells
    write ( *, '(A)', advance='no' ) '|'
    write ( *, '(A)', advance='no' ) ' '
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '|'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) ' '
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '|'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) ' '
    write ( *, '(A)' ) '|'
    
    ! Row 2 horizontal walls
    write ( *, '(A)', advance='no' ) '+'
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '-'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) '+'
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '-'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) '+'
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '-'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)' ) '+'
    
    ! Row 2 vertical walls and cells
    write ( *, '(A)', advance='no' ) '|'
    write ( *, '(A)', advance='no' ) ' '
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '|'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) ' '
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '|'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) ' '
    write ( *, '(A)' ) '|'
    
    ! Row 3 horizontal walls
    write ( *, '(A)', advance='no' ) '+'
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '-'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) '+'
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '-'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) '+'
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '-'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)' ) '+'
    
    ! Row 3 vertical walls and cells
    write ( *, '(A)', advance='no' ) '|'
    write ( *, '(A)', advance='no' ) ' '
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '|'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) ' '
    if ( wallList (currentInteriorWall) .eqv. .true. ) then
        write ( *, '(A)', advance='no' ) '|'
    else
        write ( *, '(A)', advance='no' ) ' '
    end if
    currentInteriorWall = currentInteriorWall + 1
    write ( *, '(A)', advance='no' ) ' '
    write ( *, '(A)' ) '|'
    
    ! Bottom border
    write ( *, '(A)', advance='no' ) '+'
    write ( *, '(A)', advance='no' ) '-'
    write ( *, '(A)', advance='no' ) '+'
    write ( *, '(A)', advance='no' ) '-'
    write ( *, '(A)', advance='no' ) '+'
    write ( *, '(A)', advance='no' ) '-'
    write ( *, '(A)' ) '+'
    
end program Main
