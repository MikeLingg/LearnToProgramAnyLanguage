program mazeGenerator
    implicit none
    
    integer, parameter :: FIRST_CELL = 1
    integer, parameter :: SECOND_CELL = 2
    integer, parameter :: NO_CELL = -1
    integer, parameter :: MAX_SIZE = 1000
    
    logical :: wallsUp ( MAX_SIZE )
    integer :: wallConnections ( MAX_SIZE, 2 )
    integer :: cellToGroup ( MAX_SIZE )
    integer :: groupCells ( MAX_SIZE, MAX_SIZE )
    integer :: wallRemoveList ( MAX_SIZE )
    
    integer :: mazeColumns, mazeRows, interiorWallCount
    integer :: wallIndex, rowIndex, columnIndex
    integer :: firstCellInRow, leftCell, rightCell
    integer :: upperCell, lowerCell, cellIndex
    integer :: innerCellIndex, shuffleIndex, otherIndex
    integer :: removeWallIndex, nextWallToCheck
    integer :: firstCell, firstCellGroupIndex
    integer :: secondCell, secondCellGroupIndex
    integer :: nextEmptyFirstGroupIndex
    integer :: groupCellIndex, cellToMove
    integer :: temp, ios, seed
    logical :: mazeComplete
    character ( len=100 ) :: inputString
    
    ! Prompt for maze size
    mazeColumns = 0
    do while ( mazeColumns <= 0 )
        write ( *, '(A)', advance='no' ) 'Please enter number of columns for maze, must be greater than 1: '
        read ( *, '(A)', iostat=ios ) inputString
        if ( ios == 0 ) then
            read ( inputString, *, iostat=ios ) mazeColumns
            if ( ios /= 0 ) mazeColumns = 0
        end if
    end do
    
    mazeRows = 0
    do while ( mazeRows <= 0 )
        write ( *, '(A)', advance='no' ) 'Please enter number of rows for maze, must be greater than 1: '
        read ( *, '(A)', iostat=ios ) inputString
        if ( ios == 0 ) then
            read ( inputString, *, iostat=ios ) mazeRows
            if ( ios /= 0 ) mazeRows = 0
        end if
    end do
    
    interiorWallCount = mazeRows * ( mazeColumns - 1 ) + ( mazeRows - 1 ) * mazeColumns
    
    ! Initialize walls (all up)
    do wallIndex = 1, interiorWallCount
        wallsUp ( wallIndex ) = .true.
    end do
    
    ! Build wall connections
    wallIndex = 1
    do rowIndex = 0, mazeRows - 1
        firstCellInRow = rowIndex * mazeColumns + 1
        
        ! Vertical walls
        do columnIndex = 0, mazeColumns - 2
            leftCell = firstCellInRow + columnIndex
            rightCell = leftCell + 1
            wallConnections ( wallIndex, FIRST_CELL ) = leftCell
            wallConnections ( wallIndex, SECOND_CELL ) = rightCell
            wallIndex = wallIndex + 1
        end do
        
        ! Horizontal walls
        if ( wallIndex <= interiorWallCount ) then
            do columnIndex = 0, mazeColumns - 1
                upperCell = firstCellInRow + columnIndex
                lowerCell = upperCell + mazeColumns
                wallConnections ( wallIndex, FIRST_CELL ) = upperCell
                wallConnections ( wallIndex, SECOND_CELL ) = lowerCell
                wallIndex = wallIndex + 1
            end do
        end if
    end do
    
    ! Initialize cell groups
    do cellIndex = 1, mazeRows * mazeColumns
        cellToGroup ( cellIndex ) = cellIndex
    end do
    
    ! Initialize group cells
    do cellIndex = 1, mazeRows * mazeColumns
        do innerCellIndex = 1, mazeRows * mazeColumns
            if ( innerCellIndex == 1 ) then
                groupCells ( cellIndex, innerCellIndex ) = cellIndex
            else
                groupCells ( cellIndex, innerCellIndex ) = NO_CELL
            end if
        end do
    end do
    
    ! Print initial maze
    call printMaze ( )
    
    ! Initialize wall removal list
    do wallIndex = 1, interiorWallCount
        wallRemoveList ( wallIndex ) = wallIndex
    end do
    
    ! Initialize random seed
    call random_seed ( )
    seed = 12345
    
    ! Fisher-Yates shuffle algorithm
    do shuffleIndex = interiorWallCount, 2, -1
        ! Simple LCG for random number generation
        seed = mod ( seed * 1664525 + 1013904223, 2147483647 )
        otherIndex = mod ( abs ( seed ), shuffleIndex ) + 1
        temp = wallRemoveList ( shuffleIndex )
        wallRemoveList ( shuffleIndex ) = wallRemoveList ( otherIndex )
        wallRemoveList ( otherIndex ) = temp
    end do
    
    mazeComplete = .false.
    
    ! Kruskal's algorithm
    do removeWallIndex = 1, interiorWallCount
        nextWallToCheck = wallRemoveList ( removeWallIndex )
        
        firstCell = wallConnections ( nextWallToCheck, FIRST_CELL )
        firstCellGroupIndex = cellToGroup ( firstCell )
        secondCell = wallConnections ( nextWallToCheck, SECOND_CELL )
        secondCellGroupIndex = cellToGroup ( secondCell )
        
        if ( firstCellGroupIndex /= secondCellGroupIndex ) then
            wallsUp ( nextWallToCheck ) = .false.
            
            ! Find next empty position in first group
            nextEmptyFirstGroupIndex = 1
            do cellIndex = 1, mazeColumns * mazeRows
                if ( groupCells ( firstCellGroupIndex, cellIndex ) == NO_CELL ) then
                    nextEmptyFirstGroupIndex = cellIndex
                    exit
                end if
            end do
            
            ! Move all cells from second group to first group
            do groupCellIndex = mazeColumns * mazeRows, 1, -1
                if ( groupCells ( secondCellGroupIndex, groupCellIndex ) /= NO_CELL ) then
                    cellToMove = groupCells ( secondCellGroupIndex, groupCellIndex )
                    
                    groupCells ( firstCellGroupIndex, nextEmptyFirstGroupIndex ) = cellToMove
                    nextEmptyFirstGroupIndex = nextEmptyFirstGroupIndex + 1
                    cellToGroup ( cellToMove ) = firstCellGroupIndex
                    groupCells ( secondCellGroupIndex, groupCellIndex ) = NO_CELL
                    
                    if ( nextEmptyFirstGroupIndex > mazeColumns * mazeRows ) then
                        mazeComplete = .true.
                    end if
                end if
            end do
            
            ! Sleep and print maze
            call sleep ( 1 )
            write ( *, * )
            call printMaze ( )
            
            if ( mazeComplete ) exit
        end if
    end do
    
contains
    
    subroutine printMaze ( )
        integer :: currentInteriorWall, rowIdx, colIdx
        
        currentInteriorWall = 1
        
        ! Print top border
        write ( *, '(A)', advance='no' ) '+'
        do colIdx = 1, mazeColumns
            write ( *, '(A)', advance='no' ) '-+'
        end do
        write ( *, * )
        
        do rowIdx = 1, mazeRows
            ! Print vertical walls and cells
            write ( *, '(A)', advance='no' ) '|'
            do colIdx = 1, mazeColumns
                write ( *, '(A)', advance='no' ) ' '
                
                if ( colIdx == mazeColumns .or. wallsUp ( currentInteriorWall ) ) then
                    write ( *, '(A)', advance='no' ) '|'
                else
                    write ( *, '(A)', advance='no' ) ' '
                end if
                
                if ( colIdx < mazeColumns ) then
                    currentInteriorWall = currentInteriorWall + 1
                end if
            end do
            write ( *, * )
            
            ! Print horizontal walls
            if ( rowIdx < mazeRows ) then
                write ( *, '(A)', advance='no' ) '+'
                do colIdx = 1, mazeColumns
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
        
        ! Print bottom border
        write ( *, '(A)', advance='no' ) '+'
        do colIdx = 1, mazeColumns
            write ( *, '(A)', advance='no' ) '-+'
        end do
        write ( *, * )
    end subroutine printMaze
    
end program mazeGenerator
