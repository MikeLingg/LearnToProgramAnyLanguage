import random
import time

FIRST_CELL = 0
SECOND_CELL = 1
NO_CELL = -1

# Prompt the user for maze size
mazeColumns = 0
while ( mazeColumns <= 0 ):
    userInput = input ( "Please enter number of columns for maze, must be greater than 1: " )
    # Check if string contains only digits
    isValid = True
    if ( len ( userInput ) == 0 ):
        isValid = False
    else:
        i = 0
        while ( i < len ( userInput ) ):
            if ( userInput[i] < '0' or userInput[i] > '9' ):
                isValid = False
                break
            i = i + 1
    
    if ( isValid ):
        mazeColumns = int ( userInput )

mazeRows = 0
while ( mazeRows <= 0 ):
    userInput = input ( "Please enter number of rows for maze, must be greater than 1: " )
    # Check if string contains only digits
    isValid = True
    if ( len ( userInput ) == 0 ):
        isValid = False
    else:
        i = 0
        while ( i < len ( userInput ) ):
            if ( userInput[i] < '0' or userInput[i] > '9' ):
                isValid = False
                break
            i = i + 1
    
    if ( isValid ):
        mazeRows = int ( userInput )

interiorWallCount = mazeRows * ( mazeColumns - 1 ) + ( mazeRows - 1 ) * mazeColumns

# Start with all the walls up
wallsUp = [False] * interiorWallCount
i = 0
while ( i < interiorWallCount ):
    wallsUp[i] = True
    i = i + 1

# Identify the cells each wall connects
wallConnections = [[0, 0] for _ in range ( interiorWallCount )]

wallIndex = 0
rowIndex = 0
while ( rowIndex < mazeRows ):
    # Track the first cell in the current row
    firstCellInRow = rowIndex * mazeColumns
    
    # Vertical walls
    verticalWallIndex = 0
    while ( verticalWallIndex < mazeColumns - 1 ):
        leftCell = firstCellInRow + verticalWallIndex
        rightCell = leftCell + 1
        wallConnections[wallIndex][FIRST_CELL] = leftCell
        wallConnections[wallIndex][SECOND_CELL] = rightCell
        wallIndex = wallIndex + 1
        verticalWallIndex = verticalWallIndex + 1
    
    # Horizontal walls
    if ( wallIndex < interiorWallCount ):
        horizontalWallIndex = 0
        while ( horizontalWallIndex < mazeColumns ):
            upperCell = firstCellInRow + horizontalWallIndex
            lowerCell = upperCell + mazeColumns
            wallConnections[wallIndex][FIRST_CELL] = upperCell
            wallConnections[wallIndex][SECOND_CELL] = lowerCell
            wallIndex = wallIndex + 1
            horizontalWallIndex = horizontalWallIndex + 1
    
    rowIndex = rowIndex + 1

# Identify which group each cell is a part of
cellToGroup = [0] * ( mazeRows * mazeColumns )
cellIndex = 0
while ( cellIndex < mazeRows * mazeColumns ):
    cellToGroup[cellIndex] = cellIndex
    cellIndex = cellIndex + 1

# Identify which cells are a part of each group
groupCells = [[NO_CELL for _ in range ( mazeRows * mazeColumns )] for _ in range ( mazeRows * mazeColumns )]

cellIndex = 0
while ( cellIndex < mazeRows * mazeColumns ):
    innerCellIndex = 0
    while ( innerCellIndex < mazeRows * mazeColumns ):
        if ( innerCellIndex == 0 ):
            groupCells[cellIndex][innerCellIndex] = cellIndex
        else:
            groupCells[cellIndex][innerCellIndex] = NO_CELL
        innerCellIndex = innerCellIndex + 1
    cellIndex = cellIndex + 1

# Print initial maze
currentInteriorWall = 0

# Print top border
print ( "+", end="" )
cellIndex = 0
while ( cellIndex < mazeColumns ):
    print ( "-+", end="" )
    cellIndex = cellIndex + 1
print ( )

rowIndex = 0
while ( rowIndex < mazeRows ):
    # Print vertical walls and cells
    print ( "|", end="" )
    columnIndex = 0
    while ( columnIndex < mazeColumns ):
        print ( " ", end="" )
        
        if ( columnIndex == mazeColumns - 1 or wallsUp[currentInteriorWall] ):
            print ( "|", end="" )
        else:
            print ( " ", end="" )
        
        if ( columnIndex < mazeColumns - 1 ):
            currentInteriorWall = currentInteriorWall + 1
        columnIndex = columnIndex + 1
    print ( )
    
    # Print horizontal walls
    if ( rowIndex < mazeRows - 1 ):
        print ( "+", end="" )
        columnIndex = 0
        while ( columnIndex < mazeColumns ):
            if ( wallsUp[currentInteriorWall] ):
                print ( "-", end="" )
            else:
                print ( " ", end="" )
            print ( "+", end="" )
            currentInteriorWall = currentInteriorWall + 1
            columnIndex = columnIndex + 1
        print ( )
    rowIndex = rowIndex + 1

# Print bottom border
print ( "+", end="" )
columnIndex = 0
while ( columnIndex < mazeColumns ):
    print ( "-+", end="" )
    columnIndex = columnIndex + 1
print ( )

# Create wall removal list
wallRemoveList = [0] * interiorWallCount
wallIndex = 0
while ( wallIndex < interiorWallCount ):
    wallRemoveList[wallIndex] = wallIndex
    wallIndex = wallIndex + 1

# Fisher-Yates shuffle algorithm
shuffleIndex = interiorWallCount - 1
while ( shuffleIndex > 0 ):
    otherIndex = random.randint ( 0, shuffleIndex )
    temp = wallRemoveList[shuffleIndex]
    wallRemoveList[shuffleIndex] = wallRemoveList[otherIndex]
    wallRemoveList[otherIndex] = temp
    shuffleIndex = shuffleIndex - 1

mazeComplete = False

# Kruskal's algorithm
removeWallIndex = 0
while ( removeWallIndex < interiorWallCount ):
    nextWallToCheck = wallRemoveList[removeWallIndex]
    
    firstCellValue = wallConnections[nextWallToCheck][FIRST_CELL]
    firstCellGroupIndex = cellToGroup[firstCellValue]
    secondCellValue = wallConnections[nextWallToCheck][SECOND_CELL]
    secondCellGroupIndex = cellToGroup[secondCellValue]
    
    if ( firstCellGroupIndex != secondCellGroupIndex ):
        wallsUp[nextWallToCheck] = False
        
        # Find next empty position in first group
        nextEmptyFirstGroupIndex = 0
        cellIndex = 0
        while ( cellIndex < mazeColumns * mazeRows ):
            if ( groupCells[firstCellGroupIndex][cellIndex] == NO_CELL ):
                nextEmptyFirstGroupIndex = cellIndex
                break
            cellIndex = cellIndex + 1
        
        # Move all cells from second group to first group
        groupCellIndex = mazeColumns * mazeRows - 1
        while ( groupCellIndex >= 0 ):
            if ( groupCells[secondCellGroupIndex][groupCellIndex] != NO_CELL ):
                cellToMove = groupCells[secondCellGroupIndex][groupCellIndex]
                
                groupCells[firstCellGroupIndex][nextEmptyFirstGroupIndex] = cellToMove
                nextEmptyFirstGroupIndex = nextEmptyFirstGroupIndex + 1
                cellToGroup[cellToMove] = firstCellGroupIndex
                groupCells[secondCellGroupIndex][groupCellIndex] = NO_CELL
                
                if ( nextEmptyFirstGroupIndex >= mazeColumns * mazeRows ):
                    mazeComplete = True
            groupCellIndex = groupCellIndex - 1
        
        time.sleep ( 0.5 )
        
        # Copy in Print maze code from above again here:
        currentInteriorWall = 0
        
        print ( )
        print ( "+", end="" )
        cellIndex = 0
        while ( cellIndex < mazeColumns ):
            print ( "-+", end="" )
            cellIndex = cellIndex + 1
        print ( )
        
        rowIndex = 0
        while ( rowIndex < mazeRows ):
            print ( "|", end="" )
            columnIndex = 0
            while ( columnIndex < mazeColumns ):
                print ( " ", end="" )
                
                if ( columnIndex == mazeColumns - 1 or wallsUp[currentInteriorWall] ):
                    print ( "|", end="" )
                else:
                    print ( " ", end="" )
                
                if ( columnIndex < mazeColumns - 1 ):
                    currentInteriorWall = currentInteriorWall + 1
                columnIndex = columnIndex + 1
            print ( )
            
            if ( rowIndex < mazeRows - 1 ):
                print ( "+", end="" )
                columnIndex = 0
                while ( columnIndex < mazeColumns ):
                    if ( wallsUp[currentInteriorWall] ):
                        print ( "-", end="" )
                    else:
                        print ( " ", end="" )
                    print ( "+", end="" )
                    currentInteriorWall = currentInteriorWall + 1
                    columnIndex = columnIndex + 1
                print ( )
            rowIndex = rowIndex + 1
        
        print ( "+", end="" )
        columnIndex = 0
        while ( columnIndex < mazeColumns ):
            print ( "-+", end="" )
            columnIndex = columnIndex + 1
        print ( )
        
        if ( mazeComplete ):
            break
    
    removeWallIndex = removeWallIndex + 1
