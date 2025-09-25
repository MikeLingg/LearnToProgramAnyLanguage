import random
import time



# Define directions
LEFT = 0
UP = 1
RIGHT = 2
DOWN = 3

FIRST_CELL = 0
SECOND_CELL = 1

NO_CELL = -1

# Define algorithm types
KRUSKAL_ALGORITHM = 1
PRIM_ALGORITHM = 2
DEPTH_FIRST_ALGORITHM = 3
BINARY_TREE_ALGORITHM = 4
RECURSIVE_DIVISION_ALGORITHM = 5



# Global variables
AllHorizontalWallsUp_Glob = None
WallsUp_Glob = None
InteriorWallCount_Glob = 0
MazeRows_Glob = 0
MazeColumns_Glob = 0

# Additional globals for algorithm state sharing
CellVisited_Glob = None
FrontierWalls_Glob = None
FrontierWallCount_Glob = 0
CellInMaze_Glob = None

# Lookup tables for wall/cell relationships
WallToCells_Glob = None
CellToWalls_Glob = None



# +-+-+-+
# Prints a horizontal wall, similar to the example above, 
# with walls down based on the provided parameters.
# HorizontalWallsUp_Par: Boolean list of size MazeColumns_Glob, 
#     True indicates the wall should be printed as up.
def printHorizontalWalls ( HorizontalWallsUp_Par ):
    print ( "+", end="" )
    for horizontalWallIndex in range ( 0, MazeColumns_Glob ):
        if ( HorizontalWallsUp_Par[horizontalWallIndex] == True ):
            print ( "-", end="" )
        else:
            print ( " ", end="" )
        print ( "+", end="" )
    print ()



# +-+-+-+
# Prints a horizontal wall, similar to the example above, with all walls up.
def printAllHorizontalWalls ():
    printHorizontalWalls ( AllHorizontalWallsUp_Glob )



# | | | |
# Prints a vertical wall, similar to the example above, 
# with walls down based on the provided parameters.
# VerticalWallsUp_Par: Boolean list of size MazeColumns_Glob - 1, 
#     True indicates the wall should be printed as up.
def printVerticalWalls ( VerticalWallsUp_Par ):
    # First wall is an exterior wall, always up.
    print ( "|", end="" )
    for verticalWallIndex in range ( 0, MazeColumns_Glob - 1 ):
        print ( " ", end="" )
        if ( VerticalWallsUp_Par[verticalWallIndex] == True ):
            print ( "|", end="" )
        else:
            print ( " ", end="" )
    # Last wall exterior, always up.
    print ( " ", end="" )
    print ( "|", end="" )
    print ()



# Loop through the rows of the maze and print the maze 
# based on WallsUp_Glob
def printMaze ():
    interiorWallIndex = 0

    # First row is exterior walls
    printAllHorizontalWalls ()
    for rowIndex in range ( 0, MazeRows_Glob ):
        verticalWallsUp = [False] * ( MazeColumns_Glob - 1 )
        for columnIndex in range ( 0, MazeColumns_Glob - 1 ):
            verticalWallsUp[columnIndex] = WallsUp_Glob[interiorWallIndex]
            interiorWallIndex = interiorWallIndex + 1

        printVerticalWalls ( verticalWallsUp )

        if ( rowIndex == MazeRows_Glob - 1 ):
            printAllHorizontalWalls ()
        else:
            horizontalWallsUp = [False] * MazeColumns_Glob
            for columnIndex in range ( 0, MazeColumns_Glob ):
                horizontalWallsUp[columnIndex] = WallsUp_Glob[interiorWallIndex]
                interiorWallIndex = interiorWallIndex + 1

            printHorizontalWalls ( horizontalWallsUp )

    print ()



# Simple sleep function
def sleepHalfSecond ():
    time.sleep ( 0.5 )



# Initialize lookup tables for wall/cell relationships.
# AlgorithmType_Par: Algorithm constant to determine which tables to build
# Must be called after maze dimensions are set.
def initializeLookupTables ( AlgorithmType_Par ):
    global WallToCells_Glob, CellToWalls_Glob
    
    # Build WallToCells_Glob for algorithms that need wall-to-cell lookups
    if ( AlgorithmType_Par == KRUSKAL_ALGORITHM or AlgorithmType_Par == PRIM_ALGORITHM or 
         AlgorithmType_Par == DEPTH_FIRST_ALGORITHM or AlgorithmType_Par == BINARY_TREE_ALGORITHM or 
         AlgorithmType_Par == RECURSIVE_DIVISION_ALGORITHM ):
        
        WallToCells_Glob = [[0, 0] for wallIndex in range ( InteriorWallCount_Glob )]
        
        wallIndex = 0
        for rowIndex in range ( 0, MazeRows_Glob ):
            # Track the first cell in the current row
            firstCellInRow = rowIndex * MazeColumns_Glob

            # Note the 0..MazeColumns_Glob minus 2, one less vertical wall
            # than the number of columns.
            for verticalWallIndex in range ( 0, MazeColumns_Glob - 1 ):
                leftCell = firstCellInRow + verticalWallIndex
                rightCell = leftCell + 1
                WallToCells_Glob[wallIndex][FIRST_CELL] = leftCell
                WallToCells_Glob[wallIndex][SECOND_CELL] = rightCell
                wallIndex = wallIndex + 1

            # The last row will have no interior horizontal walls below
            # it, so will be skipped.
            if ( wallIndex < InteriorWallCount_Glob ):
                for horizontalWallIndex in range ( 0, MazeColumns_Glob ):
                    upperCell = firstCellInRow + horizontalWallIndex
                    lowerCell = upperCell + MazeColumns_Glob
                    WallToCells_Glob[wallIndex][FIRST_CELL] = upperCell
                    WallToCells_Glob[wallIndex][SECOND_CELL] = lowerCell
                    wallIndex = wallIndex + 1

    # Build CellToWalls_Glob for algorithms that need cell-to-wall lookups
    if ( AlgorithmType_Par == PRIM_ALGORITHM or AlgorithmType_Par == DEPTH_FIRST_ALGORITHM or 
         AlgorithmType_Par == BINARY_TREE_ALGORITHM or AlgorithmType_Par == RECURSIVE_DIVISION_ALGORITHM ):
        
        CellToWalls_Glob = [[NO_CELL, NO_CELL, NO_CELL, NO_CELL] for cellIndex in range ( MazeRows_Glob * MazeColumns_Glob )]
        
        for cellIndex in range ( 0, MazeRows_Glob * MazeColumns_Glob ):
            cellRow = cellIndex // MazeColumns_Glob
            cellCol = cellIndex % MazeColumns_Glob
            
            # Initialize all directions to NO_CELL (invalid wall)
            CellToWalls_Glob[cellIndex][LEFT] = NO_CELL
            CellToWalls_Glob[cellIndex][UP] = NO_CELL
            CellToWalls_Glob[cellIndex][RIGHT] = NO_CELL
            CellToWalls_Glob[cellIndex][DOWN] = NO_CELL
            
            # Find walls by checking which walls connect to this cell
            for wallIndex in range ( 0, InteriorWallCount_Glob ):
                firstCellIndex = WallToCells_Glob[wallIndex][FIRST_CELL]
                secondCellIndex = WallToCells_Glob[wallIndex][SECOND_CELL]
                
                if ( firstCellIndex == cellIndex ):
                    # This wall connects from our cell to secondCell
                    secondRow = secondCellIndex // MazeColumns_Glob
                    secondCol = secondCellIndex % MazeColumns_Glob
                    
                    if ( secondRow == cellRow and secondCol == cellCol + 1 ):
                        # Wall goes RIGHT
                        CellToWalls_Glob[cellIndex][RIGHT] = wallIndex
                    elif ( secondRow == cellRow + 1 and secondCol == cellCol ):
                        # Wall goes DOWN
                        CellToWalls_Glob[cellIndex][DOWN] = wallIndex
                elif ( secondCellIndex == cellIndex ):
                    # This wall connects from firstCellIndex to our cell
                    firstRow = firstCellIndex // MazeColumns_Glob
                    firstCol = firstCellIndex % MazeColumns_Glob
                    
                    if ( firstRow == cellRow and firstCol == cellCol - 1 ):
                        # Wall comes from LEFT
                        CellToWalls_Glob[cellIndex][LEFT] = wallIndex
                    elif ( firstRow == cellRow - 1 and firstCol == cellCol ):
                        # Wall comes from UP
                        CellToWalls_Glob[cellIndex][UP] = wallIndex



# Add a wall to frontier if not already there.
# WallIndex_Par: Index of wall to add to frontier, must be 0 to InteriorWallCount_Glob-1
def addWallToFrontier ( WallIndex_Par ):
    global FrontierWallCount_Glob
    
    # Check if wall already in frontier
    alreadyInFrontier = False
    for wallIndex in range ( 0, FrontierWallCount_Glob ):
        if ( FrontierWalls_Glob[wallIndex] == WallIndex_Par ):
            alreadyInFrontier = True
            break

    if ( alreadyInFrontier == False ):
        FrontierWalls_Glob[FrontierWallCount_Glob] = WallIndex_Par
        FrontierWallCount_Glob = FrontierWallCount_Glob + 1



# Add all walls adjacent to a cell to the frontier list.
# CellIndex_Par: Index of the cell whose adjacent walls should be added to frontier: 0 .. ( MazeRows_Glob * MazeColumns_Glob - 1 )
def addCellWallsToFrontier ( CellIndex_Par ):
    # Check all four directions
    if ( CellToWalls_Glob[CellIndex_Par][UP] != NO_CELL ):
        addWallToFrontier ( CellToWalls_Glob[CellIndex_Par][UP] )
    if ( CellToWalls_Glob[CellIndex_Par][DOWN] != NO_CELL ):
        addWallToFrontier ( CellToWalls_Glob[CellIndex_Par][DOWN] )
    if ( CellToWalls_Glob[CellIndex_Par][LEFT] != NO_CELL ):
        addWallToFrontier ( CellToWalls_Glob[CellIndex_Par][LEFT] )
    if ( CellToWalls_Glob[CellIndex_Par][RIGHT] != NO_CELL ):
        addWallToFrontier ( CellToWalls_Glob[CellIndex_Par][RIGHT] )



# Recursively divide an area with walls.
# StartRow_Par: Starting row of area to divide, must be 0 to MazeRows_Glob-1
# EndRow_Par: Ending row of area to divide, must be StartRow_Par to MazeRows_Glob-1
# StartCol_Par: Starting column of area to divide, must be 0 to MazeColumns_Glob-1
# EndCol_Par: Ending column of area to divide, must be StartCol_Par to MazeColumns_Glob-1
def divideArea ( StartRow_Par, EndRow_Par, StartCol_Par, EndCol_Par ):
    height = EndRow_Par - StartRow_Par + 1
    width = EndCol_Par - StartCol_Par + 1

    # Base case - area too small to divide
    if ( height >= 2 or width >= 2 ):
        # Choose whether to divide horizontally or vertically
        divideHorizontally = False
        if ( height > width ):
            divideHorizontally = True
        elif ( width > height ):
            divideHorizontally = False
        else:
            # Square area - choose randomly
            divideHorizontally = ( random.randint ( 0, 1 ) == 0 )

        if ( divideHorizontally == True and height > 1 ):
            # Choose random row to divide on
            divideRow = StartRow_Par + random.randint ( 0, EndRow_Par - StartRow_Par - 1 )

            # Add horizontal wall
            for wallCol in range ( StartCol_Par, EndCol_Par + 1 ):
                cellIndex = divideRow * MazeColumns_Glob + wallCol
                if ( cellIndex < MazeRows_Glob * MazeColumns_Glob ):
                    wallIndex = CellToWalls_Glob[cellIndex][DOWN]
                    if ( wallIndex != NO_CELL ):
                        WallsUp_Glob[wallIndex] = True

            # Choose random gap in the wall
            gapCol = StartCol_Par + random.randint ( 0, EndCol_Par - StartCol_Par )
            gapCellIndex = divideRow * MazeColumns_Glob + gapCol
            if ( gapCellIndex < MazeRows_Glob * MazeColumns_Glob ):
                gapWallIndex = CellToWalls_Glob[gapCellIndex][DOWN]
                if ( gapWallIndex != NO_CELL ):
                    WallsUp_Glob[gapWallIndex] = False

            sleepHalfSecond ()
            printMaze ()

            # Recursively divide the two areas
            divideArea ( StartRow_Par, divideRow, StartCol_Par, EndCol_Par )
            divideArea ( divideRow + 1, EndRow_Par, StartCol_Par, EndCol_Par )
        elif ( divideHorizontally == False and width > 1 ):
            # Choose random column to divide on
            divideCol = StartCol_Par + random.randint ( 0, EndCol_Par - StartCol_Par - 1 )

            # Add vertical wall
            for cellRow in range ( StartRow_Par, EndRow_Par + 1 ):
                cellIndex = cellRow * MazeColumns_Glob + divideCol
                if ( cellIndex < MazeRows_Glob * MazeColumns_Glob ):
                    wallIndex = CellToWalls_Glob[cellIndex][RIGHT]
                    if ( wallIndex != NO_CELL ):
                        WallsUp_Glob[wallIndex] = True

            # Choose random gap in the wall
            gapRow = StartRow_Par + random.randint ( 0, EndRow_Par - StartRow_Par )
            gapCellIndex = gapRow * MazeColumns_Glob + divideCol
            if ( gapCellIndex < MazeRows_Glob * MazeColumns_Glob ):
                gapWallIndex = CellToWalls_Glob[gapCellIndex][RIGHT]
                if ( gapWallIndex != NO_CELL ):
                    WallsUp_Glob[gapWallIndex] = False

            sleepHalfSecond ()
            printMaze ()

            # Recursively divide the two areas
            divideArea ( StartRow_Par, EndRow_Par, StartCol_Par, divideCol )
            divideArea ( StartRow_Par, EndRow_Par, divideCol + 1, EndCol_Par )



# Kruskal's algorithm.
# The simple description of the algorithm is first place each 
# cell in its own group.  Then process all walls in random order,
# if the cells on either side of the wall are in separate groups, 
# remove the wall and merge the groups.  Repeat until all 
# cells are now in the same group.
def buildMazeKruskal ():
    # Identify which cells are a part of each group
    # Note each list is large enough to fit all cells,
    # NO_CELL indicates no cell is assigned to this index.
    # Languages like python that make adding/removing 
    # list items easy will not need this, for C arrays
    # we will discuss reallocation or linked list in 
    # later videos for covering changing array sizes.
    cellToGroup = [0] * ( MazeRows_Glob * MazeColumns_Glob )
    groupCells = [[NO_CELL] * ( MazeRows_Glob * MazeColumns_Glob ) for groupIndex in range ( MazeRows_Glob * MazeColumns_Glob )]

    for cellIndex in range ( 0, MazeRows_Glob * MazeColumns_Glob ):
        cellToGroup[cellIndex] = cellIndex

        for groupCellIndex in range ( 0, MazeRows_Glob * MazeColumns_Glob ):
            if ( groupCellIndex == 0 ):
                groupCells[cellIndex][groupCellIndex] = cellIndex
            else:
                groupCells[cellIndex][groupCellIndex] = NO_CELL

    mazeComplete = False

    wallRemoveList = [0] * InteriorWallCount_Glob
    for wallIndex in range ( 0, InteriorWallCount_Glob ):
        wallRemoveList[wallIndex] = wallIndex
    
    # Fisher-Yates shuffle
    for shuffleIndex in range ( InteriorWallCount_Glob - 1, 0, -1 ):
        randomIndex = random.randint ( 0, shuffleIndex )
        temp = wallRemoveList[shuffleIndex]
        wallRemoveList[shuffleIndex] = wallRemoveList[randomIndex]
        wallRemoveList[randomIndex] = temp

    # Perform Kruskal's algorithm.
    for removeWallIndex in range ( 0, InteriorWallCount_Glob ):
        nextWallToCheck = wallRemoveList[removeWallIndex]

        # If the two cells connected to this wall are not part 
        # of the same group, remove the wall and merge the 
        # groups.
        firstCellIndex = WallToCells_Glob[nextWallToCheck][FIRST_CELL]
        firstCellGroupIndex = cellToGroup[firstCellIndex]
        secondCellIndex = WallToCells_Glob[nextWallToCheck][SECOND_CELL]
        secondCellGroupIndex = cellToGroup[secondCellIndex]
        if ( firstCellGroupIndex != secondCellGroupIndex ):
            WallsUp_Glob[nextWallToCheck] = False

            # Loop through the indices of all cells in the first 
            # group until we find a NO_CELL indicating no cell here.
            nextEmptyFirstGroupIndex = 0
            for cellIndex in range ( 0, MazeColumns_Glob * MazeRows_Glob ):
                if ( groupCells[firstCellGroupIndex][cellIndex] == NO_CELL ):
                    nextEmptyFirstGroupIndex = cellIndex
                    break

            # Loop through the indices of all cells in the second group,
            # move each cell to the first group, and set that cell's 
            # group to the first group index.
            for groupCellIndex in range ( MazeColumns_Glob * MazeRows_Glob - 1, -1, -1 ):
                # Skip until we reach valid cells
                if ( groupCells[secondCellGroupIndex][groupCellIndex] != NO_CELL ):
                    # Get the id number of the cell to move from 
                    # the second group to the first group
                    cellToMove = groupCells[secondCellGroupIndex][groupCellIndex]

                    # Move the cell number from the second group 
                    # to the first group
                    groupCells[firstCellGroupIndex][nextEmptyFirstGroupIndex] = cellToMove
                    # Move our empty index to the next cell in this list.
                    nextEmptyFirstGroupIndex = nextEmptyFirstGroupIndex + 1
                    # Mark this cell as part of the first group.
                    cellToGroup[cellToMove] = firstCellGroupIndex
                    # Remove the cell from the second group (set the
                    # list entry to NO_CELL)
                    groupCells[secondCellGroupIndex][groupCellIndex] = NO_CELL

                    if ( nextEmptyFirstGroupIndex >= MazeColumns_Glob * MazeRows_Glob ):
                        mazeComplete = True

            sleepHalfSecond ()
            printMaze ()

            if ( mazeComplete == True ):
                break



# Prim's algorithm for maze generation.
# Start with a random cell, mark it as part of the maze.
# Repeatedly pick a random wall from cells in the maze that 
# connects to a cell not in the maze, remove the wall and 
# add the new cell to the maze.
def buildMazePrim ():
    global FrontierWallCount_Glob
    
    # Initialize algorithm state
    FrontierWallCount_Glob = 0

    # Track how many cells have been added to the maze
    cellsInMaze = 0

    # Start with a random cell
    startCell = random.randint ( 0, MazeRows_Glob * MazeColumns_Glob - 1 )
    CellInMaze_Glob[startCell] = True
    cellsInMaze = cellsInMaze + 1

    # Add all walls adjacent to the start cell to frontier
    addCellWallsToFrontier ( startCell )

    # Continue until all cells are in the maze
    while ( cellsInMaze < MazeRows_Glob * MazeColumns_Glob ):
        # Pick a random wall from frontier
        randomWallIndex = random.randint ( 0, FrontierWallCount_Glob - 1 )
        wallToCheck = FrontierWalls_Glob[randomWallIndex]

        # Remove this wall from frontier list by replacing 
        # it with the last wall in the list
        FrontierWalls_Glob[randomWallIndex] = FrontierWalls_Glob[FrontierWallCount_Glob - 1]
        FrontierWallCount_Glob = FrontierWallCount_Glob - 1

        # Get the two cells this wall connects
        firstCellIndex = WallToCells_Glob[wallToCheck][FIRST_CELL]
        secondCellIndex = WallToCells_Glob[wallToCheck][SECOND_CELL]

        # If one cell is already in the maze and the 
        # other is not, remove the wall to connect the 
        # outside cell to the maze
        if ( CellInMaze_Glob[firstCellIndex] != CellInMaze_Glob[secondCellIndex] ):
            WallsUp_Glob[wallToCheck] = False

            outerCellIndex = -1

            # Add the outside cell to the maze
            if ( CellInMaze_Glob[firstCellIndex] == False ):
                outerCellIndex = firstCellIndex
            else:
                outerCellIndex = secondCellIndex

            CellInMaze_Glob[outerCellIndex] = True
            cellsInMaze = cellsInMaze + 1
            addCellWallsToFrontier ( outerCellIndex )

            sleepHalfSecond ()
            printMaze ()



# Depth-first search maze generation using recursive backtracking.
# Start at a random cell, randomly walk to neighbors outside 
# the maze,removing walls as you go. 
# When all neighbors are in the maze, backtrack to a cell 
# with neighbors outside the maze.
def buildMazeDepthFirst ():
    # Stack for backtracking - store cell indices
    cellStack = [0] * ( MazeRows_Glob * MazeColumns_Glob )
    stackSize = 0

    # Start with random cell
    currentCellIndex = random.randint ( 0, MazeRows_Glob * MazeColumns_Glob - 1 )
    CellVisited_Glob[currentCellIndex] = True

    # Push starting cell onto stack
    cellStack[stackSize] = currentCellIndex
    stackSize = stackSize + 1

    while ( stackSize > 0 ):
        # Create randomized direction list
        randomizedDirections = [LEFT, UP, RIGHT, DOWN]
        
        # Fisher-Yates shuffle the directions
        for shuffleIndex in range ( 3, 0, -1 ):
            randomIndex = random.randint ( 0, shuffleIndex )
            temp = randomizedDirections[shuffleIndex]
            randomizedDirections[shuffleIndex] = randomizedDirections[randomIndex]
            randomizedDirections[randomIndex] = temp

        foundNeighbor = False
        nextCellIndex = NO_CELL
        wallIndex = NO_CELL

        # Check directions in random order until we find an unvisited neighbor
        for directionIndex in range ( 0, 4 ):
            direction = randomizedDirections[directionIndex]
            wallIndex = CellToWalls_Glob[currentCellIndex][direction]
            if ( wallIndex != NO_CELL ):
                # Find the cell on the other side of this wall
                firstCellIndex = WallToCells_Glob[wallIndex][FIRST_CELL]
                secondCellIndex = WallToCells_Glob[wallIndex][SECOND_CELL]
                
                if ( firstCellIndex == currentCellIndex ):
                    nextCellIndex = secondCellIndex
                else:
                    nextCellIndex = firstCellIndex
                
                # Check if neighbor is unvisited
                if ( CellVisited_Glob[nextCellIndex] == False ):
                    foundNeighbor = True
                    break

        if ( foundNeighbor == True ):
            # Find wall between current and next cell using lookup tables
            wallIndex = NO_CELL
            for direction in range ( 0, 4 ):
                wallCandidate = CellToWalls_Glob[currentCellIndex][direction]
                if ( wallCandidate != NO_CELL ):
                    firstCellIndex = WallToCells_Glob[wallCandidate][FIRST_CELL]
                    secondCellIndex = WallToCells_Glob[wallCandidate][SECOND_CELL]
                    if ( ( firstCellIndex == currentCellIndex and secondCellIndex == nextCellIndex ) or 
                         ( firstCellIndex == nextCellIndex and secondCellIndex == currentCellIndex ) ):
                        wallIndex = wallCandidate
                        break
            WallsUp_Glob[wallIndex] = False

            # Mark next cell as visited
            CellVisited_Glob[nextCellIndex] = True

            # Push next cell onto stack
            cellStack[stackSize] = nextCellIndex
            stackSize = stackSize + 1

            currentCellIndex = nextCellIndex

            sleepHalfSecond ()
            printMaze ()
        else:
            # Backtrack - pop from stack
            stackSize = stackSize - 1
            if ( stackSize > 0 ):
                currentCellIndex = cellStack[stackSize - 1]



# Binary Tree maze generation algorithm.
# For each cell, randomly choose to either remove the wall 
# to the north or the wall to the east (if they exist).
# This creates a maze with a distinctive bias.
def buildMazeBinaryTree ():
    for cellRow in range ( 0, MazeRows_Glob ):
        for cellCol in range ( 0, MazeColumns_Glob ):
            currentCellIndex = cellRow * MazeColumns_Glob + cellCol
            validWalls = [0, 0]
            validWallCount = 0

            # Check if we can go north (UP) - only if not in top row
            if ( cellRow > 0 ):
                wallIndex = CellToWalls_Glob[currentCellIndex][UP]
                if ( wallIndex != NO_CELL ):
                    validWalls[validWallCount] = wallIndex
                    validWallCount = validWallCount + 1

            # Check if we can go east (RIGHT) - only if not in rightmost column
            if ( cellCol < MazeColumns_Glob - 1 ):
                wallIndex = CellToWalls_Glob[currentCellIndex][RIGHT]
                if ( wallIndex != NO_CELL ):
                    validWalls[validWallCount] = wallIndex
                    validWallCount = validWallCount + 1

            # If we have at least one valid wall, pick one randomly and remove it
            if ( validWallCount > 0 ):
                randomWallIndex = random.randint ( 0, validWallCount - 1 )
                wallToRemove = validWalls[randomWallIndex]
                WallsUp_Glob[wallToRemove] = False

            sleepHalfSecond ()
            printMaze ()



# Recursive Division maze generation algorithm.
# Start with an empty area (all walls down), then recursively
# divide the area with walls, leaving random gaps.
def buildMazeRecursiveDivision ():
    # Start with all interior walls down
    for wallIndex in range ( 0, InteriorWallCount_Glob ):
        WallsUp_Glob[wallIndex] = False

    printMaze ()
    sleepHalfSecond ()

    # Recursively divide the entire maze area
    divideArea ( 0, MazeRows_Glob - 1, 0, MazeColumns_Glob - 1 )



# Note we could use command line arguments instead of user 
# prompts to get the maze size and algorithm choice.
def main ():
    global MazeColumns_Glob, MazeRows_Glob
    global AllHorizontalWallsUp_Glob, WallsUp_Glob, InteriorWallCount_Glob
    global CellVisited_Glob, FrontierWalls_Glob, CellInMaze_Glob
    
    # Prompt the user for maze size
    MazeColumns_Glob = 0
    while ( MazeColumns_Glob <= 0 ):
        userInput = input ( "Please enter number of columns for maze, must be greater than 1: " )
        
        # Check if string is a valid number
        isValid = True
        if ( len ( userInput ) == 0 ):
            isValid = False
        else:
            for characterIndex in range ( 0, len ( userInput ) ):
                if ( userInput[characterIndex] < '0' or userInput[characterIndex] > '9' ):
                    isValid = False
                    break
        
        if ( isValid == True ):
            MazeColumns_Glob = int ( userInput )

    MazeRows_Glob = 0
    while ( MazeRows_Glob <= 0 ):
        userInput = input ( "Please enter number of rows for maze, must be greater than 1: " )
        
        # Check if string is a valid number
        isValid = True
        if ( len ( userInput ) == 0 ):
            isValid = False
        else:
            for characterIndex in range ( 0, len ( userInput ) ):
                if ( userInput[characterIndex] < '0' or userInput[characterIndex] > '9' ):
                    isValid = False
                    break
        
        if ( isValid == True ):
            MazeRows_Glob = int ( userInput )

    # Prompt the user for algorithm choice
    algorithmChoice = 0
    while ( algorithmChoice < 1 or algorithmChoice > 5 ):
        print ( "Please choose maze generation algorithm:" )
        print ( "1 - Kruskal's Algorithm" )
        print ( "2 - Prim's Algorithm" )
        print ( "3 - Depth-First Search" )
        print ( "4 - Binary Tree Algorithm" )
        print ( "5 - Recursive Division Algorithm" )
        userInput = input ()
        
        # Check if string is a valid number
        isValid = True
        if ( len ( userInput ) == 0 ):
            isValid = False
        else:
            for characterIndex in range ( 0, len ( userInput ) ):
                if ( userInput[characterIndex] < '0' or userInput[characterIndex] > '9' ):
                    isValid = False
                    break
        
        if ( isValid == True ):
            algorithmChoice = int ( userInput )

    # Setup maze datastructures for the user entered size.
    AllHorizontalWallsUp_Glob = [True] * MazeColumns_Glob
    
    InteriorWallCount_Glob = MazeRows_Glob * ( MazeColumns_Glob - 1 ) + ( MazeRows_Glob - 1 ) * MazeColumns_Glob
    
    WallsUp_Glob = [True] * InteriorWallCount_Glob

    # Initialize algorithm state globals
    CellVisited_Glob = [False] * ( MazeRows_Glob * MazeColumns_Glob )
    
    FrontierWalls_Glob = [0] * InteriorWallCount_Glob
    
    CellInMaze_Glob = [False] * ( MazeRows_Glob * MazeColumns_Glob )

    # Initialize lookup tables based on chosen algorithm
    initializeLookupTables ( algorithmChoice )

    # Execute the chosen algorithm
    if ( algorithmChoice == KRUSKAL_ALGORITHM ):
        buildMazeKruskal ()
    elif ( algorithmChoice == PRIM_ALGORITHM ):
        buildMazePrim ()
    elif ( algorithmChoice == DEPTH_FIRST_ALGORITHM ):
        buildMazeDepthFirst ()
    elif ( algorithmChoice == BINARY_TREE_ALGORITHM ):
        buildMazeBinaryTree ()
    elif ( algorithmChoice == RECURSIVE_DIVISION_ALGORITHM ):
        buildMazeRecursiveDivision ()

    input ( "Press Enter to exit..." )



if __name__ == "__main__":
    main ()
