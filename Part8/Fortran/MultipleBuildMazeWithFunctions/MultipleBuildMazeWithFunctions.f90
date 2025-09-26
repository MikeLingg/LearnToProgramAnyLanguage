program Maze_Generator
    implicit none

    ! Define directions
    integer, parameter :: LEFT = 0
    integer, parameter :: UP = 1
    integer, parameter :: RIGHT = 2
    integer, parameter :: DOWN = 3

    integer, parameter :: FIRST_CELL = 0
    integer, parameter :: SECOND_CELL = 1
    integer, parameter :: NO_CELL = -1

    ! Define algorithm types
    integer, parameter :: KRUSKAL_ALGORITHM = 1
    integer, parameter :: PRIM_ALGORITHM = 2
    integer, parameter :: DEPTH_FIRST_ALGORITHM = 3
    integer, parameter :: BINARY_TREE_ALGORITHM = 4
    integer, parameter :: RECURSIVE_DIVISION_ALGORITHM = 5

    ! Global variables
    logical, allocatable :: AllHorizontalWallsUp_Glob ( : )
    logical, allocatable :: WallsUp_Glob ( : )
    integer :: InteriorWallCount_Glob
    integer :: MazeRows_Glob
    integer :: MazeColumns_Glob

    ! Additional globals for algorithm state sharing
    logical, allocatable :: CellVisited_Glob ( : )
    integer, allocatable :: FrontierWalls_Glob ( : )
    integer :: FrontierWallCount_Glob
    logical, allocatable :: CellInMaze_Glob ( : )

    ! Lookup tables for wall/cell relationships
    integer, allocatable :: WallToCells_Glob ( :, : )
    integer, allocatable :: CellToWalls_Glob ( :, : )

    ! Main program variables
    character ( 100 ) :: userInput
    integer :: algorithmChoice
    integer :: ios
    integer :: columnIndex
    integer :: wallIndex
    integer :: cellIndex

    ! Initialize random seed
    call random_seed ()

    ! Prompt the user for maze size
    MazeColumns_Glob = 0
    do while ( MazeColumns_Glob <= 0 )
        write ( *, '(a)', advance='no' ) 'Please enter number of columns for maze, must be greater than 1: '
        read ( *, '(a)' ) userInput
        read ( userInput, *, iostat=ios ) MazeColumns_Glob
        if ( ios /= 0 ) then
            MazeColumns_Glob = 0
        end if
    end do

    MazeRows_Glob = 0
    do while ( MazeRows_Glob <= 0 )
        write ( *, '(a)', advance='no' ) 'Please enter number of rows for maze, must be greater than 1: '
        read ( *, '(a)' ) userInput
        read ( userInput, *, iostat=ios ) MazeRows_Glob
        if ( ios /= 0 ) then
            MazeRows_Glob = 0
        end if
    end do

    ! Prompt the user for algorithm choice
    algorithmChoice = 0
    do while ( algorithmChoice < 1 .or. algorithmChoice > 5 )
        write ( *, * ) 'Please choose maze generation algorithm:'
        write ( *, * ) '1 - Kruskal''s Algorithm'
        write ( *, * ) '2 - Prim''s Algorithm'
        write ( *, * ) '3 - Depth-First Search'
        write ( *, * ) '4 - Binary Tree Algorithm'
        write ( *, * ) '5 - Recursive Division Algorithm'
        read ( *, '(a)' ) userInput
        read ( userInput, *, iostat=ios ) algorithmChoice
        if ( ios /= 0 ) then
            algorithmChoice = 0
        end if
    end do

    ! Allocate arrays based on user input sizes
    allocate ( AllHorizontalWallsUp_Glob ( MazeColumns_Glob ) )
    
    InteriorWallCount_Glob = MazeRows_Glob * ( MazeColumns_Glob - 1 ) + ( MazeRows_Glob - 1 ) * MazeColumns_Glob
    
    allocate ( WallsUp_Glob ( InteriorWallCount_Glob ) )
    allocate ( CellVisited_Glob ( MazeRows_Glob * MazeColumns_Glob ) )
    allocate ( CellInMaze_Glob ( MazeRows_Glob * MazeColumns_Glob ) )
    allocate ( FrontierWalls_Glob ( InteriorWallCount_Glob ) )
    allocate ( WallToCells_Glob ( InteriorWallCount_Glob, 2 ) )
    allocate ( CellToWalls_Glob ( MazeRows_Glob * MazeColumns_Glob, 4 ) )

    ! Setup maze datastructures for the user entered size.
    do columnIndex = 1, MazeColumns_Glob
        AllHorizontalWallsUp_Glob ( columnIndex ) = .true.
    end do
    
    do wallIndex = 1, InteriorWallCount_Glob
        WallsUp_Glob ( wallIndex ) = .true.
    end do

    ! Initialize algorithm state globals
    do cellIndex = 1, MazeRows_Glob * MazeColumns_Glob
        CellVisited_Glob ( cellIndex ) = .false.
        CellInMaze_Glob ( cellIndex ) = .false.
    end do

    ! Initialize lookup tables based on chosen algorithm
    call initializeLookupTables ( algorithmChoice )

    ! Execute the chosen algorithm
    if ( algorithmChoice == KRUSKAL_ALGORITHM ) then
        call buildMazeKruskal ()
    else if ( algorithmChoice == PRIM_ALGORITHM ) then
        call buildMazePrim ()
    else if ( algorithmChoice == DEPTH_FIRST_ALGORITHM ) then
        call buildMazeDepthFirst ()
    else if ( algorithmChoice == BINARY_TREE_ALGORITHM ) then
        call buildMazeBinaryTree ()
    else if ( algorithmChoice == RECURSIVE_DIVISION_ALGORITHM ) then
        call buildMazeRecursiveDivision ()
    end if

    write ( *, * ) 'Press Enter to exit...'
    read ( *, '(a)' ) userInput

contains



    ! +-+-+-+
    ! Prints a horizontal wall, similar to the example above, 
    ! with walls down based on the provided parameters.
    ! HorizontalWallsUp_Par: Boolean array of size MazeColumns_Glob, 
    !     True indicates the wall should be printed as up.
    subroutine printHorizontalWalls ( HorizontalWallsUp_Par )
        implicit none
        logical, intent ( in ) :: HorizontalWallsUp_Par ( : )
        integer :: horizontalWallIndex

        write ( *, '(a)', advance='no' ) '+'
        do horizontalWallIndex = 1, MazeColumns_Glob
            if ( HorizontalWallsUp_Par ( horizontalWallIndex ) .eqv. .true. ) then
                write ( *, '(a)', advance='no' ) '-'
            else
                write ( *, '(a)', advance='no' ) ' '
            end if
            write ( *, '(a)', advance='no' ) '+'
        end do
        write ( *, * )
    end subroutine printHorizontalWalls



    ! +-+-+-+
    ! Prints a horizontal wall, similar to the example above, with all walls up.
    subroutine printAllHorizontalWalls ()
        implicit none
        call printHorizontalWalls ( AllHorizontalWallsUp_Glob )
    end subroutine printAllHorizontalWalls



    ! | | | |
    ! Prints a vertical wall, similar to the example above, 
    ! with walls down based on the provided parameters.
    ! VerticalWallsUp_Par: Boolean array of size MazeColumns_Glob - 1, 
    !     True indicates the wall should be printed as up.
    subroutine printVerticalWalls ( VerticalWallsUp_Par )
        implicit none
        logical, intent ( in ) :: VerticalWallsUp_Par ( : )
        integer :: verticalWallIndex

        ! First wall is an exterior wall, always up.
        write ( *, '(a)', advance='no' ) '|'
        do verticalWallIndex = 1, MazeColumns_Glob - 1
            write ( *, '(a)', advance='no' ) ' '
            if ( VerticalWallsUp_Par ( verticalWallIndex ) .eqv. .true. ) then
                write ( *, '(a)', advance='no' ) '|'
            else
                write ( *, '(a)', advance='no' ) ' '
            end if
        end do
        ! Last wall exterior, always up.
        write ( *, '(a)', advance='no' ) ' '
        write ( *, '(a)', advance='no' ) '|'
        write ( *, * )
    end subroutine printVerticalWalls



    ! Loop through the rows of the maze and print the maze 
    ! based on WallsUp_Glob
    subroutine printMaze ()
        implicit none
        integer :: interiorWallIndex
        logical :: verticalWallsUp ( MazeColumns_Glob - 1 )
        logical :: horizontalWallsUp ( MazeColumns_Glob )
        integer :: rowIndex
        integer :: columnIndex

        interiorWallIndex = 1

        ! First row is exterior walls
        call printAllHorizontalWalls ()
        do rowIndex = 1, MazeRows_Glob
            do columnIndex = 1, MazeColumns_Glob - 1
                verticalWallsUp ( columnIndex ) = WallsUp_Glob ( interiorWallIndex )
                interiorWallIndex = interiorWallIndex + 1
            end do

            call printVerticalWalls ( verticalWallsUp )

            if ( rowIndex == MazeRows_Glob ) then
                call printAllHorizontalWalls ()
            else
                do columnIndex = 1, MazeColumns_Glob
                    horizontalWallsUp ( columnIndex ) = WallsUp_Glob ( interiorWallIndex )
                    interiorWallIndex = interiorWallIndex + 1
                end do

                call printHorizontalWalls ( horizontalWallsUp )
            end if
        end do

        write ( *, * )
    end subroutine printMaze



    ! Simple sleep function
    subroutine sleepHalfSecond ()
        implicit none
        real :: startTime
        real :: currentTime
        
        call cpu_time ( startTime )
        do
            call cpu_time ( currentTime )
            if ( currentTime - startTime >= 0.5 ) exit
        end do
    end subroutine sleepHalfSecond



    ! Initialize lookup tables for wall/cell relationships.
    ! AlgorithmType_Par: Algorithm constant to determine which tables to build
    ! Must be called after maze dimensions are set.
    subroutine initializeLookupTables ( AlgorithmType_Par )
        implicit none
        integer, intent ( in ) :: AlgorithmType_Par
        integer :: wallIndex
        integer :: rowIndex
        integer :: verticalWallIndex
        integer :: horizontalWallIndex
        integer :: firstCellInRow
        integer :: leftCell
        integer :: rightCell
        integer :: upperCell
        integer :: lowerCell
        integer :: cellIndex
        integer :: cellRow
        integer :: cellCol
        integer :: firstCellIndex
        integer :: secondCellIndex
        integer :: secondRow
        integer :: secondCol
        integer :: firstRow
        integer :: firstCol

        ! Build WallToCells_Glob for algorithms that need wall-to-cell lookups
        if ( AlgorithmType_Par == KRUSKAL_ALGORITHM .or. AlgorithmType_Par == PRIM_ALGORITHM .or. &
             AlgorithmType_Par == DEPTH_FIRST_ALGORITHM .or. AlgorithmType_Par == BINARY_TREE_ALGORITHM .or. &
             AlgorithmType_Par == RECURSIVE_DIVISION_ALGORITHM ) then
            
            wallIndex = 1
            do rowIndex = 1, MazeRows_Glob
                ! Track the first cell in the current row
                firstCellInRow = ( rowIndex - 1 ) * MazeColumns_Glob + 1

                ! Note the 1..MazeColumns_Glob - 1, one less vertical wall
                ! than the number of columns.
                do verticalWallIndex = 1, MazeColumns_Glob - 1
                    leftCell = firstCellInRow + verticalWallIndex - 1
                    rightCell = leftCell + 1
                    WallToCells_Glob ( wallIndex, FIRST_CELL + 1 ) = leftCell
                    WallToCells_Glob ( wallIndex, SECOND_CELL + 1 ) = rightCell
                    wallIndex = wallIndex + 1
                end do

                ! The last row will have no interior horizontal walls below
                ! it, so will be skipped.
                if ( wallIndex <= InteriorWallCount_Glob ) then
                    do horizontalWallIndex = 1, MazeColumns_Glob
                        upperCell = firstCellInRow + horizontalWallIndex - 1
                        lowerCell = upperCell + MazeColumns_Glob
                        WallToCells_Glob ( wallIndex, FIRST_CELL + 1 ) = upperCell
                        WallToCells_Glob ( wallIndex, SECOND_CELL + 1 ) = lowerCell
                        wallIndex = wallIndex + 1
                    end do
                end if
            end do
        end if

        ! Build CellToWalls_Glob for algorithms that need cell-to-wall lookups
        if ( AlgorithmType_Par == PRIM_ALGORITHM .or. AlgorithmType_Par == DEPTH_FIRST_ALGORITHM .or. &
             AlgorithmType_Par == BINARY_TREE_ALGORITHM .or. AlgorithmType_Par == RECURSIVE_DIVISION_ALGORITHM ) then
            
            do cellIndex = 1, MazeRows_Glob * MazeColumns_Glob
                cellRow = ( cellIndex - 1 ) / MazeColumns_Glob + 1
                cellCol = mod ( cellIndex - 1, MazeColumns_Glob ) + 1
                
                ! Initialize all directions to NO_CELL (invalid wall)
                CellToWalls_Glob ( cellIndex, LEFT + 1 ) = NO_CELL
                CellToWalls_Glob ( cellIndex, UP + 1 ) = NO_CELL
                CellToWalls_Glob ( cellIndex, RIGHT + 1 ) = NO_CELL
                CellToWalls_Glob ( cellIndex, DOWN + 1 ) = NO_CELL
                
                ! Find walls by checking which walls connect to this cell
                do wallIndex = 1, InteriorWallCount_Glob
                    firstCellIndex = WallToCells_Glob ( wallIndex, FIRST_CELL + 1 )
                    secondCellIndex = WallToCells_Glob ( wallIndex, SECOND_CELL + 1 )
                    
                    if ( firstCellIndex == cellIndex ) then
                        ! This wall connects from our cell to secondCell
                        secondRow = ( secondCellIndex - 1 ) / MazeColumns_Glob + 1
                        secondCol = mod ( secondCellIndex - 1, MazeColumns_Glob ) + 1
                        
                        if ( secondRow == cellRow .and. secondCol == cellCol + 1 ) then
                            ! Wall goes RIGHT
                            CellToWalls_Glob ( cellIndex, RIGHT + 1 ) = wallIndex
                        else if ( secondRow == cellRow + 1 .and. secondCol == cellCol ) then
                            ! Wall goes DOWN
                            CellToWalls_Glob ( cellIndex, DOWN + 1 ) = wallIndex
                        end if
                    else if ( secondCellIndex == cellIndex ) then
                        ! This wall connects from firstCellIndex to our cell
                        firstRow = ( firstCellIndex - 1 ) / MazeColumns_Glob + 1
                        firstCol = mod ( firstCellIndex - 1, MazeColumns_Glob ) + 1
                        
                        if ( firstRow == cellRow .and. firstCol == cellCol - 1 ) then
                            ! Wall comes from LEFT
                            CellToWalls_Glob ( cellIndex, LEFT + 1 ) = wallIndex
                        else if ( firstRow == cellRow - 1 .and. firstCol == cellCol ) then
                            ! Wall comes from UP
                            CellToWalls_Glob ( cellIndex, UP + 1 ) = wallIndex
                        end if
                    end if
                end do
            end do
        end if
    end subroutine initializeLookupTables



    ! Add a wall to frontier if not already there.
    ! WallIndex_Par: Index of wall to add to frontier, must be 1 to InteriorWallCount_Glob
    subroutine addWallToFrontier ( WallIndex_Par )
        implicit none
        integer, intent ( in ) :: WallIndex_Par
        logical :: alreadyInFrontier
        integer :: wallIndex

        alreadyInFrontier = .false.
        ! Check if wall already in frontier
        do wallIndex = 1, FrontierWallCount_Glob
            if ( FrontierWalls_Glob ( wallIndex ) == WallIndex_Par ) then
                alreadyInFrontier = .true.
                exit
            end if
        end do

        if ( alreadyInFrontier .eqv. .false. ) then
            FrontierWallCount_Glob = FrontierWallCount_Glob + 1
            FrontierWalls_Glob ( FrontierWallCount_Glob ) = WallIndex_Par
        end if
    end subroutine addWallToFrontier



    ! Add all walls adjacent to a cell to the frontier list.
    ! CellIndex_Par: Index of the cell whose adjacent walls should be added to frontier: 1 .. ( MazeRows_Glob * MazeColumns_Glob )
    subroutine addCellWallsToFrontier ( CellIndex_Par )
        implicit none
        integer, intent ( in ) :: CellIndex_Par

        ! Check all four directions
        if ( CellToWalls_Glob ( CellIndex_Par, UP + 1 ) /= NO_CELL ) then
            call addWallToFrontier ( CellToWalls_Glob ( CellIndex_Par, UP + 1 ) )
        end if
        if ( CellToWalls_Glob ( CellIndex_Par, DOWN + 1 ) /= NO_CELL ) then
            call addWallToFrontier ( CellToWalls_Glob ( CellIndex_Par, DOWN + 1 ) )
        end if
        if ( CellToWalls_Glob ( CellIndex_Par, LEFT + 1 ) /= NO_CELL ) then
            call addWallToFrontier ( CellToWalls_Glob ( CellIndex_Par, LEFT + 1 ) )
        end if
        if ( CellToWalls_Glob ( CellIndex_Par, RIGHT + 1 ) /= NO_CELL ) then
            call addWallToFrontier ( CellToWalls_Glob ( CellIndex_Par, RIGHT + 1 ) )
        end if
    end subroutine addCellWallsToFrontier



    ! Recursively divide an area with walls.
    ! StartRow_Par: Starting row of area to divide, must be 1 to MazeRows_Glob
    ! EndRow_Par: Ending row of area to divide, must be StartRow_Par to MazeRows_Glob
    ! StartCol_Par: Starting column of area to divide, must be 1 to MazeColumns_Glob
    ! EndCol_Par: Ending column of area to divide, must be StartCol_Par to MazeColumns_Glob
    recursive subroutine divideArea ( StartRow_Par, EndRow_Par, StartCol_Par, EndCol_Par )
        implicit none
        integer, intent ( in ) :: StartRow_Par, EndRow_Par, StartCol_Par, EndCol_Par
        integer :: height
        integer :: width
        logical :: divideHorizontally
        integer :: divideRow
        integer :: divideCol
        integer :: cellIndex
        integer :: wallIndex
        integer :: gapCol
        integer :: gapRow
        integer :: gapCellIndex
        integer :: gapWallIndex
        integer :: wallCol
        integer :: cellRow
        real :: randomValue

        height = EndRow_Par - StartRow_Par + 1
        width = EndCol_Par - StartCol_Par + 1

        ! Base case - area too small to divide
        if ( height >= 2 .and. width >= 2 ) then
            ! Choose whether to divide horizontally or vertically
            divideHorizontally = .false.
            if ( height > width ) then
                divideHorizontally = .true.
            else if ( width > height ) then
                divideHorizontally = .false.
            else
                ! Square area - choose randomly
                call random_number ( randomValue )
                divideHorizontally = ( int ( randomValue * 2 ) == 0 )
            end if

            if ( divideHorizontally .eqv. .true. .and. height > 1 ) then
                ! Choose random row to divide on
                call random_number ( randomValue )
                divideRow = StartRow_Par + int ( randomValue * ( EndRow_Par - StartRow_Par ) )

                ! Add horizontal wall
                do wallCol = StartCol_Par, EndCol_Par
                    cellIndex = ( divideRow - 1 ) * MazeColumns_Glob + wallCol
                    if ( cellIndex <= MazeRows_Glob * MazeColumns_Glob ) then
                        wallIndex = CellToWalls_Glob ( cellIndex, DOWN + 1 )
                        if ( wallIndex /= NO_CELL ) then
                            WallsUp_Glob ( wallIndex ) = .true.
                        end if
                    end if
                end do

                ! Choose random gap in the wall
                call random_number ( randomValue )
                gapCol = StartCol_Par + int ( randomValue * ( EndCol_Par - StartCol_Par + 1 ) )
                gapCellIndex = ( divideRow - 1 ) * MazeColumns_Glob + gapCol
                if ( gapCellIndex <= MazeRows_Glob * MazeColumns_Glob ) then
                    gapWallIndex = CellToWalls_Glob ( gapCellIndex, DOWN + 1 )
                    if ( gapWallIndex /= NO_CELL ) then
                        WallsUp_Glob ( gapWallIndex ) = .false.
                    end if
                end if

                call sleepHalfSecond ()
                call printMaze ()

                ! Recursively divide the two areas
                call divideArea ( StartRow_Par, divideRow, StartCol_Par, EndCol_Par )
                call divideArea ( divideRow + 1, EndRow_Par, StartCol_Par, EndCol_Par )
            else if ( divideHorizontally .eqv. .false. .and. width > 1 ) then
                ! Choose random column to divide on
                call random_number ( randomValue )
                divideCol = StartCol_Par + int ( randomValue * ( EndCol_Par - StartCol_Par ) )

                ! Add vertical wall
                do cellRow = StartRow_Par, EndRow_Par
                    cellIndex = ( cellRow - 1 ) * MazeColumns_Glob + divideCol
                    if ( cellIndex <= MazeRows_Glob * MazeColumns_Glob ) then
                        wallIndex = CellToWalls_Glob ( cellIndex, RIGHT + 1 )
                        if ( wallIndex /= NO_CELL ) then
                            WallsUp_Glob ( wallIndex ) = .true.
                        end if
                    end if
                end do

                ! Choose random gap in the wall
                call random_number ( randomValue )
                gapRow = StartRow_Par + int ( randomValue * ( EndRow_Par - StartRow_Par + 1 ) )
                gapCellIndex = ( gapRow - 1 ) * MazeColumns_Glob + divideCol
                if ( gapCellIndex <= MazeRows_Glob * MazeColumns_Glob ) then
                    gapWallIndex = CellToWalls_Glob ( gapCellIndex, RIGHT + 1 )
                    if ( gapWallIndex /= NO_CELL ) then
                        WallsUp_Glob ( gapWallIndex ) = .false.
                    end if
                end if

                call sleepHalfSecond ()
                call printMaze ()

                ! Recursively divide the two areas
                call divideArea ( StartRow_Par, EndRow_Par, StartCol_Par, divideCol )
                call divideArea ( StartRow_Par, EndRow_Par, divideCol + 1, EndCol_Par )
            end if
        end if
    end subroutine divideArea



    ! Kruskal's algorithm.
    ! The simple description of the algorithm is first place each 
    ! cell in its own group.  Then process all walls in random order,
    ! if the cells on either side of the wall are in separate groups, 
    ! remove the wall and merge the groups.  Repeat until all 
    ! cells are now in the same group.
    subroutine buildMazeKruskal ()
        implicit none
        ! Identify which cells are a part of each group
        integer, allocatable :: cellToGroup ( : )
        integer, allocatable :: groupCells ( :, : )
        logical :: mazeComplete
        integer, allocatable :: wallRemoveList ( : )
        integer :: shuffleIndex
        integer :: randomIndex
        integer :: temp
        integer :: nextWallToCheck
        integer :: firstCellIndex
        integer :: firstCellGroupIndex
        integer :: secondCellIndex
        integer :: secondCellGroupIndex
        integer :: nextEmptyFirstGroupIndex
        integer :: cellToMove
        integer :: cellIndex
        integer :: groupCellIndex
        integer :: removeWallIndex
        integer :: wallIndex
        real :: randomValue

        allocate ( cellToGroup ( MazeRows_Glob * MazeColumns_Glob ) )
        allocate ( groupCells ( MazeRows_Glob * MazeColumns_Glob, MazeRows_Glob * MazeColumns_Glob ) )
        allocate ( wallRemoveList ( InteriorWallCount_Glob ) )

        mazeComplete = .false.

        do cellIndex = 1, MazeRows_Glob * MazeColumns_Glob
            cellToGroup ( cellIndex ) = cellIndex

            do groupCellIndex = 1, MazeRows_Glob * MazeColumns_Glob
                if ( groupCellIndex == 1 ) then
                    groupCells ( cellIndex, groupCellIndex ) = cellIndex
                else
                    groupCells ( cellIndex, groupCellIndex ) = NO_CELL
                end if
            end do
        end do

        do wallIndex = 1, InteriorWallCount_Glob
            wallRemoveList ( wallIndex ) = wallIndex
        end do
        
        ! Fisher-Yates shuffle
        do shuffleIndex = InteriorWallCount_Glob, 2, -1
            call random_number ( randomValue )
            randomIndex = 1 + int ( randomValue * shuffleIndex )
            temp = wallRemoveList ( shuffleIndex )
            wallRemoveList ( shuffleIndex ) = wallRemoveList ( randomIndex )
            wallRemoveList ( randomIndex ) = temp
        end do

        ! Perform Kruskal's algorithm.
        do removeWallIndex = 1, InteriorWallCount_Glob
            nextWallToCheck = wallRemoveList ( removeWallIndex )

            ! If the two cells connected to this wall are not part 
            ! of the same group, remove the wall and merge the 
            ! groups.
            firstCellIndex = WallToCells_Glob ( nextWallToCheck, FIRST_CELL + 1 )
            firstCellGroupIndex = cellToGroup ( firstCellIndex )
            secondCellIndex = WallToCells_Glob ( nextWallToCheck, SECOND_CELL + 1 )
            secondCellGroupIndex = cellToGroup ( secondCellIndex )
            if ( firstCellGroupIndex /= secondCellGroupIndex ) then
                WallsUp_Glob ( nextWallToCheck ) = .false.

                ! Loop through the indices of all cells in the first 
                ! group until we find a NO_CELL indicating no cell here.
                nextEmptyFirstGroupIndex = 1
                do while ( nextEmptyFirstGroupIndex <= MazeColumns_Glob * MazeRows_Glob .and. &
                           groupCells ( firstCellGroupIndex, nextEmptyFirstGroupIndex ) /= NO_CELL )
                    nextEmptyFirstGroupIndex = nextEmptyFirstGroupIndex + 1
                end do

                ! Loop through the indices of all cells in the second group,
                ! move each cell to the first group, and set that cell's 
                ! group to the first group index.
                do groupCellIndex = MazeColumns_Glob * MazeRows_Glob, 1, -1
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
                        ! Remove the cell from the second group (set the
                        ! array entry to NO_CELL)
                        groupCells ( secondCellGroupIndex, groupCellIndex ) = NO_CELL

                        if ( nextEmptyFirstGroupIndex >= MazeColumns_Glob * MazeRows_Glob ) then
                            mazeComplete = .true.
                        end if
                    end if
                end do

                call sleepHalfSecond ()
                call printMaze ()

                if ( mazeComplete .eqv. .true. ) then
                    exit
                end if
            end if
        end do

        deallocate ( cellToGroup )
        deallocate ( groupCells )
        deallocate ( wallRemoveList )
    end subroutine buildMazeKruskal



    ! Prim's algorithm for maze generation.
    ! Start with a random cell, mark it as part of the maze.
    ! Repeatedly pick a random wall from cells in the maze that 
    ! connects to a cell not in the maze, remove the wall and 
    ! add the new cell to the maze.
    subroutine buildMazePrim ()
        implicit none
        integer :: cellsInMaze
        integer :: startCell
        integer :: randomWallIndex
        integer :: wallToCheck
        integer :: firstCellIndex
        integer :: secondCellIndex
        integer :: outerCellIndex
        real :: randomValue

        ! Initialize algorithm state
        FrontierWallCount_Glob = 0
        cellsInMaze = 0

        ! Start with a random cell
        call random_number ( randomValue )
        startCell = 1 + int ( randomValue * ( MazeRows_Glob * MazeColumns_Glob ) )
        CellInMaze_Glob ( startCell ) = .true.
        cellsInMaze = cellsInMaze + 1

        ! Add all walls adjacent to the start cell to frontier
        call addCellWallsToFrontier ( startCell )

        ! Continue until all cells are in the maze
        do while ( cellsInMaze < MazeRows_Glob * MazeColumns_Glob )
            ! Pick a random wall from frontier
            call random_number ( randomValue )
            randomWallIndex = 1 + int ( randomValue * FrontierWallCount_Glob )
            wallToCheck = FrontierWalls_Glob ( randomWallIndex )

            ! Remove this wall from frontier list by replacing 
            ! it with the last wall in the list
            FrontierWalls_Glob ( randomWallIndex ) = FrontierWalls_Glob ( FrontierWallCount_Glob )
            FrontierWallCount_Glob = FrontierWallCount_Glob - 1

            ! Get the two cells this wall connects
            firstCellIndex = WallToCells_Glob ( wallToCheck, FIRST_CELL + 1 )
            secondCellIndex = WallToCells_Glob ( wallToCheck, SECOND_CELL + 1 )

            ! If one cell is already in the maze and the 
            ! other is not, remove the wall to connect the 
            ! outside cell to the maze
            if ( CellInMaze_Glob ( firstCellIndex ) .neqv. CellInMaze_Glob ( secondCellIndex ) ) then
                WallsUp_Glob ( wallToCheck ) = .false.

                outerCellIndex = -1

                ! Add the outside cell to the maze
                if ( CellInMaze_Glob ( firstCellIndex ) .eqv. .false. ) then
                    outerCellIndex = firstCellIndex
                else
                    outerCellIndex = secondCellIndex
                end if

                CellInMaze_Glob ( outerCellIndex ) = .true.
                cellsInMaze = cellsInMaze + 1
                call addCellWallsToFrontier ( outerCellIndex )

                call sleepHalfSecond ()
                call printMaze ()
            end if
        end do
    end subroutine buildMazePrim



    ! Depth-first search maze generation using recursive backtracking.
    ! Start at a random cell, randomly walk to neighbors outside 
    ! the maze,removing walls as you go. 
    ! When all neighbors are in the maze, backtrack to a cell 
    ! with neighbors outside the maze.
    subroutine buildMazeDepthFirst ()
        implicit none
        integer :: cellStack ( MazeRows_Glob * MazeColumns_Glob )
        integer :: stackSize
        integer :: currentCellIndex
        integer :: randomizedDirections ( 4 )
        integer :: shuffleIndex
        integer :: randomIndex
        integer :: temp
        logical :: foundNeighbor
        integer :: nextCellIndex
        integer :: wallIndex
        integer :: firstCellIndex
        integer :: secondCellIndex
        integer :: wallCandidate
        integer :: directionIndex
        integer :: direction
        real :: randomValue

        stackSize = 0

        ! Start with random cell
        call random_number ( randomValue )
        currentCellIndex = 1 + int ( randomValue * ( MazeRows_Glob * MazeColumns_Glob ) )
        CellVisited_Glob ( currentCellIndex ) = .true.

        ! Push starting cell onto stack
        stackSize = stackSize + 1
        cellStack ( stackSize ) = currentCellIndex

        do while ( stackSize > 0 )
            ! Create randomized direction list
            randomizedDirections ( 1 ) = LEFT
            randomizedDirections ( 2 ) = UP
            randomizedDirections ( 3 ) = RIGHT
            randomizedDirections ( 4 ) = DOWN
            
            ! Fisher-Yates shuffle the directions
            do shuffleIndex = 4, 2, -1
                call random_number ( randomValue )
                randomIndex = 1 + int ( randomValue * shuffleIndex )
                temp = randomizedDirections ( shuffleIndex )
                randomizedDirections ( shuffleIndex ) = randomizedDirections ( randomIndex )
                randomizedDirections ( randomIndex ) = temp
            end do

            foundNeighbor = .false.
            nextCellIndex = NO_CELL
            wallIndex = NO_CELL

            ! Check directions in random order until we find an unvisited neighbor
            do directionIndex = 1, 4
                wallIndex = CellToWalls_Glob ( currentCellIndex, randomizedDirections ( directionIndex ) + 1 )
                if ( wallIndex /= NO_CELL ) then
                    ! Find the cell on the other side of this wall
                    firstCellIndex = WallToCells_Glob ( wallIndex, FIRST_CELL + 1 )
                    secondCellIndex = WallToCells_Glob ( wallIndex, SECOND_CELL + 1 )
                    
                    if ( firstCellIndex == currentCellIndex ) then
                        nextCellIndex = secondCellIndex
                    else
                        nextCellIndex = firstCellIndex
                    end if
                    
                    ! Check if neighbor is unvisited
                    if ( CellVisited_Glob ( nextCellIndex ) .eqv. .false. ) then
                        foundNeighbor = .true.
                        exit
                    end if
                end if
            end do

            if ( foundNeighbor .eqv. .true. ) then
                ! Find wall between current and next cell using lookup tables
                wallIndex = NO_CELL
                do direction = 1, 4
                    wallCandidate = CellToWalls_Glob ( currentCellIndex, direction )
                    if ( wallCandidate /= NO_CELL ) then
                        firstCellIndex = WallToCells_Glob ( wallCandidate, FIRST_CELL + 1 )
                        secondCellIndex = WallToCells_Glob ( wallCandidate, SECOND_CELL + 1 )
                        if ( ( firstCellIndex == currentCellIndex .and. secondCellIndex == nextCellIndex ) .or. &
                             ( firstCellIndex == nextCellIndex .and. secondCellIndex == currentCellIndex ) ) then
                            wallIndex = wallCandidate
                            exit
                        end if
                    end if
                end do
                WallsUp_Glob ( wallIndex ) = .false.

                ! Mark next cell as visited
                CellVisited_Glob ( nextCellIndex ) = .true.

                ! Push next cell onto stack
                stackSize = stackSize + 1
                cellStack ( stackSize ) = nextCellIndex

                currentCellIndex = nextCellIndex

                call sleepHalfSecond ()
                call printMaze ()
            else
                ! Backtrack - pop from stack
                stackSize = stackSize - 1
                if ( stackSize > 0 ) then
                    currentCellIndex = cellStack ( stackSize )
                end if
            end if
        end do
    end subroutine buildMazeDepthFirst



    ! Binary Tree maze generation algorithm.
    ! For each cell, randomly choose to either remove the wall 
    ! to the north or the wall to the east (if they exist).
    ! This creates a maze with a distinctive bias.
    subroutine buildMazeBinaryTree ()
        implicit none
        integer :: cellRow
        integer :: cellCol
        integer :: currentCellIndex
        integer :: validWalls ( 2 )
        integer :: validWallCount
        integer :: wallIndex
        integer :: randomWallIndex
        integer :: wallToRemove
        real :: randomValue

        do cellRow = 1, MazeRows_Glob
            do cellCol = 1, MazeColumns_Glob
                currentCellIndex = ( cellRow - 1 ) * MazeColumns_Glob + cellCol
                validWallCount = 0

                ! Check if we can go north (UP) - only if not in top row
                if ( cellRow > 1 ) then
                    wallIndex = CellToWalls_Glob ( currentCellIndex, UP + 1 )
                    if ( wallIndex /= NO_CELL ) then
                        validWallCount = validWallCount + 1
                        validWalls ( validWallCount ) = wallIndex
                    end if
                end if

                ! Check if we can go east (RIGHT) - only if not in rightmost column
                if ( cellCol < MazeColumns_Glob ) then
                    wallIndex = CellToWalls_Glob ( currentCellIndex, RIGHT + 1 )
                    if ( wallIndex /= NO_CELL ) then
                        validWallCount = validWallCount + 1
                        validWalls ( validWallCount ) = wallIndex
                    end if
                end if

                ! If we have at least one valid wall, pick one randomly and remove it
                if ( validWallCount > 0 ) then
                    call random_number ( randomValue )
                    randomWallIndex = 1 + int ( randomValue * validWallCount )
                    wallToRemove = validWalls ( randomWallIndex )
                    WallsUp_Glob ( wallToRemove ) = .false.
                end if

                call sleepHalfSecond ()
                call printMaze ()
            end do
        end do
    end subroutine buildMazeBinaryTree



    ! Recursive Division maze generation algorithm.
    ! Start with an empty area (all walls down), then recursively
    ! divide the area with walls, leaving random gaps.
    subroutine buildMazeRecursiveDivision ()
        implicit none
        integer :: wallIndex

        ! Start with all interior walls down
        do wallIndex = 1, InteriorWallCount_Glob
            WallsUp_Glob ( wallIndex ) = .false.
        end do

        call printMaze ()
        call sleepHalfSecond ()

        ! Recursively divide the entire maze area
        call divideArea ( 1, MazeRows_Glob, 1, MazeColumns_Glob )
    end subroutine buildMazeRecursiveDivision

end program Maze_Generator
