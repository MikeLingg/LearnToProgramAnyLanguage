program maze_generator
    implicit none
    
    ! Define directions
    ! integer, parameter :: LEFT = 0
    ! integer, parameter :: UP = 1
    ! integer, parameter :: RIGHT = 2
    ! integer, parameter :: DOWN = 3

    integer, parameter :: FIRST_CELL = 1
    integer, parameter :: SECOND_CELL = 2
    
    integer, parameter :: NO_CELL = -1
    
    integer, parameter :: INTERIOR_WALL_COUNT = 12

    ! For each cell 1-9, indicate if a wall is exterior and cannot be removed ( -1 ) or its interior index for each of
    ! the four directions, LEFT, UP, RIGHT, DOWN.
    ! integer, dimension ( 9, 4 ) :: cellToWallLUT
    ! cellToWallLUT = reshape ( [ &
    !    -1, -1,  1,  3,   1, -1,  2,  4,   2, -1, -1,  5, &
    !    -1,  3,  6,  8,   6,  4,  7,  9,   7,  5, -1, 10, &
    !    -1,  8, 11, -1,  11,  9, 12, -1,  12, 10, -1, -1 ], [ 9, 4 ] )

    ! 12 interior walls in a 3x3 maze. Start with all the walls up.
    logical, dimension ( 12 ) :: wallsUp = .true.

    ! Identify the cells each wall connects.
    integer, dimension ( 12, 2 ) :: wallConnections
    
    ! Identify which group each cell is a part of.
    integer, dimension ( 9 ) :: cellToGroup = [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ]

    ! Identify which cells are a part of each group
    integer, dimension ( 9, 9 ) :: groupCells

    ! Print maze variables
    integer :: currentInteriorWall = 1
    
    ! Create and randomize wall removal list
    integer, dimension ( 12 ) :: wallRemoveList = [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 ]
    
    logical :: mazeComplete = .false.
    
    ! Variables for algorithm
    integer :: nextWallToCheck
    integer :: firstCell
    integer :: firstCellGroupIndex
    integer :: secondCell
    integer :: secondCellGroupIndex
    integer :: nextEmptyFirstGroupIndex
    integer :: cellToMove
    
    ! Loop variables
    integer :: cellIndex, rowIndex, removeWallIndex
    integer :: shuffleIndex, otherIndex, temp
    integer :: groupCellIndex
    
    ! Random number variables
    real :: randValue
    integer :: seedSize
    integer, allocatable :: seed ( : )
    
    ! Initialize wallConnections
    wallConnections ( 1, : ) = [ 1, 2 ]
    wallConnections ( 2, : ) = [ 2, 3 ]
    wallConnections ( 3, : ) = [ 1, 4 ]
    wallConnections ( 4, : ) = [ 2, 5 ]
    wallConnections ( 5, : ) = [ 3, 6 ]
    wallConnections ( 6, : ) = [ 4, 5 ]
    wallConnections ( 7, : ) = [ 5, 6 ]
    wallConnections ( 8, : ) = [ 4, 7 ]
    wallConnections ( 9, : ) = [ 5, 8 ]
    wallConnections ( 10, : ) = [ 6, 9 ]
    wallConnections ( 11, : ) = [ 7, 8 ]
    wallConnections ( 12, : ) = [ 8, 9 ]
    
    ! Initialize groupCells
    do cellIndex = 1, 9
        groupCells ( cellIndex, 1 ) = cellIndex
        do otherIndex = 2, 9
            groupCells ( cellIndex, otherIndex ) = NO_CELL
        end do
    end do

    ! Initialize random number generator
    call random_seed ( size = seedSize )
    allocate ( seed ( seedSize ) )
    call system_clock ( count = seed ( 1 ) )
    call random_seed ( put = seed )

    ! Print maze code:
    ! Print out the maze, this is a less painful copy/paste job without functions, but better with loops.

    ! Print the horizontal walls above row 1 - All are exterior walls, no conditions.
    ! +-+-+-+
    ! One initial cell with + followed by 3 cells with -+
    write ( *, '(A)', advance='no' ) '+'
    do cellIndex = 1, 3
        write ( *, '(A)', advance='no' ) '-'
        write ( *, '(A)', advance='no' ) '+'
    end do
    write ( *, * )

    do rowIndex = 1, 3
        ! Vertical walls and cells row 1.
        ! The left and right vertical walls are exterior, always up.
        ! | | | |
        ! Or print one |, followed by 3 cells of <space>| where the |
        ! may be down ( <space> ).
        write ( *, '(A)', advance='no' ) '|'
        do cellIndex = 1, 3
            write ( *, '(A)', advance='no' ) ' '

            ! Always print the right most vertical wall,
            ! if interior wall, print if the wall is up.
            if ( cellIndex == 3 .or. wallsUp ( currentInteriorWall ) ) then
                write ( *, '(A)', advance='no' ) '|'
            else
                write ( *, '(A)', advance='no' ) ' '
            end if
            if ( cellIndex < 3 ) then
                currentInteriorWall = currentInteriorWall + 1
            end if
        end do
        write ( *, * )

        ! One fewer horizontal wall than vertical
        if ( rowIndex < 3 ) then
            ! Horizontal walls above row rowIndex
            ! +-+-+-+
            write ( *, '(A)', advance='no' ) '+'
            do cellIndex = 1, 3
                if ( wallsUp ( currentInteriorWall ) ) then
                    write ( *, '(A)', advance='no' ) '-'
                else
                    write ( *, '(A)', advance='no' ) ' '
                end if
                write ( *, '(A)', advance='no' ) '+'
            end do
            write ( *, * )
        end if

        currentInteriorWall = currentInteriorWall + 1
    end do

    ! Horizontal walls below row 3 - All are exterior walls, no conditions.
    ! +-+-+-+
    write ( *, '(A)', advance='no' ) '+'
    do cellIndex = 1, 3
        write ( *, '(A)', advance='no' ) '-'
        write ( *, '(A)', advance='no' ) '+'
    end do
    write ( *, * )

    ! Fisher-Yates shuffle algorithm
    do shuffleIndex = 12, 2, -1
        ! Generate random index from 1 to shuffleIndex ( inclusive )
        call random_number ( randValue )
        otherIndex = int ( randValue * shuffleIndex ) + 1

        ! Swap wallRemoveList ( shuffleIndex ) with wallRemoveList ( otherIndex )
        temp = wallRemoveList ( shuffleIndex )
        wallRemoveList ( shuffleIndex ) = wallRemoveList ( otherIndex )
        wallRemoveList ( otherIndex ) = temp
    end do

    ! Remove wall code:
    ! Now that we have loops we can implement Kruskal's algorithm.
    ! The simple description of the algorithm is first place each 
    ! cell in its own group.  Then process all walls in random order,
    ! if the cells on either side of the wall are in separate groups, 
    ! remove the wall and merge the groups.  Repeat until all 
    ! cells are now in the same group.
    do removeWallIndex = 1, INTERIOR_WALL_COUNT
        nextWallToCheck = wallRemoveList ( removeWallIndex )

        ! If the two cells connected to this wall are not part 
        ! of the same group, remove the wall and merge the 
        ! groups.
        firstCell = wallConnections ( nextWallToCheck, FIRST_CELL )
        firstCellGroupIndex = cellToGroup ( firstCell )
        secondCell = wallConnections ( nextWallToCheck, SECOND_CELL )
        secondCellGroupIndex = cellToGroup ( secondCell )
        
        if ( firstCellGroupIndex /= secondCellGroupIndex ) then
            wallsUp ( nextWallToCheck ) = .false.

            ! Loop through the indices of all cells in the first 
            ! group until we find a NO_CELL indicating no cell here.
            nextEmptyFirstGroupIndex = 1
            do cellIndex = 1, 9
                if ( groupCells ( firstCellGroupIndex, cellIndex ) == NO_CELL ) then
                    nextEmptyFirstGroupIndex = cellIndex
                    exit
                end if
            end do

            ! Loop through the indices of all cells in the second group,
            ! move each cell to the first group, and set that cell's 
            ! group to the first group index.
            do groupCellIndex = 9, 1, -1
                ! Skip until we reach valid cells
                if ( groupCells ( secondCellGroupIndex, groupCellIndex ) /= NO_CELL ) then
                    ! Get the id number of the cell to move from 
                    ! the second group to the first group
                    cellToMove = groupCells ( secondCellGroupIndex, groupCellIndex )

                    ! Move the cell number from the second group 
                    ! to the first group
                    groupCells ( firstCellGroupIndex, nextEmptyFirstGroupIndex ) = cellToMove
                    ! Move our empty index to the next cell in this array.
                    nextEmptyFirstGroupIndex = nextEmptyFirstGroupIndex + 1
                    ! Mark this cell as part of the first group.
                    cellToGroup ( cellToMove ) = firstCellGroupIndex
                    ! Remove the cell from the second group ( set the
                    ! array entry to NO_CELL )
                    groupCells ( secondCellGroupIndex, groupCellIndex ) = NO_CELL
                    
                    if ( nextEmptyFirstGroupIndex >= 10 ) then
                        mazeComplete = .true.
                    end if
                end if
            end do

            ! Print maze code ( copied from above ):
            call sleep ( 1 ) ! Sleep for approximately 1 second
            currentInteriorWall = 1

            write ( *, * )
            write ( *, '(A)', advance='no' ) '+'
            do cellIndex = 1, 3
                write ( *, '(A)', advance='no' ) '-'
                write ( *, '(A)', advance='no' ) '+'
            end do
            write ( *, * )

            do rowIndex = 1, 3
                write ( *, '(A)', advance='no' ) '|'
                do cellIndex = 1, 3
                    write ( *, '(A)', advance='no' ) ' '

                    if ( cellIndex == 3 .or. wallsUp ( currentInteriorWall ) ) then
                        write ( *, '(A)', advance='no' ) '|'
                    else
                        write ( *, '(A)', advance='no' ) ' '
                    end if
                    if ( cellIndex < 3 ) then
                        currentInteriorWall = currentInteriorWall + 1
                    end if
                end do
                write ( *, * )

                if ( rowIndex < 3 ) then
                    write ( *, '(A)', advance='no' ) '+'
                    do cellIndex = 1, 3
                        if ( wallsUp ( currentInteriorWall ) ) then
                            write ( *, '(A)', advance='no' ) '-'
                        else
                            write ( *, '(A)', advance='no' ) ' '
                        end if
                        write ( *, '(A)', advance='no' ) '+'

                        currentInteriorWall = currentInteriorWall + 1
                    end do
                    write ( *, * )
                end if
            end do

            write ( *, '(A)', advance='no' ) '+'
            do cellIndex = 1, 3
                write ( *, '(A)', advance='no' ) '-'
                write ( *, '(A)', advance='no' ) '+'
            end do
            write ( *, * )
        end if

        if ( mazeComplete ) then
            exit
        end if
    end do

end program maze_generator
