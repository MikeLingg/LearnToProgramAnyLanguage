import time
import random



# Define directions
LEFT = 0
UP = 1
RIGHT = 2
DOWN = 3

FIRST_CELL = 0
SECOND_CELL = 1

NO_CELL = -1



# Global variables
AllHorizontalWallsUp_Glob = []
WallsUp_Glob = []
InteriorWallCount_Glob = 0
MazeRows_Glob = 0
MazeColumns_Glob = 0



# +-+-+-+
# Prints a horizontal wall, similar to the example above, 
# with walls down based on the provided parameters.
# HorizontalWallsUp_Par: Boolean list of size mazeColumns, 
#     True indicates the wall should be printed as up.
def printHorizontalWalls ( HorizontalWallsUp_Par ):
    print ( "+", end="" )
    for horizontalWallIndex in range ( 0, MazeColumns_Glob ):
        if ( HorizontalWallsUp_Par[horizontalWallIndex] == True ):
            print ( "-", end="" )
        else:
            print ( " ", end="" )
        print ( "+", end="" )
    print()



# +-+-+-+
# Prints a horizontal wall, similar to the example above, with all walls up.
def printHorizontalWallsAll():
    printHorizontalWalls ( AllHorizontalWallsUp_Glob )



# | | | |
# Prints a vertical wall, similar to the example above, 
# with walls down based on the provided parameters.
# VerticalWallsUp_Par: Boolean list of size mazeColumns - 1, 
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
    print()



# Loop through the rows of the maze and print the maze 
# based on WallsUp_Glob
def printMaze():
    interiorWallIndex = 0

    # First row is exterior walls
    printHorizontalWallsAll()
    for rowIndex in range ( 0, MazeRows_Glob ):
        verticalWallsUp = [False] * ( MazeColumns_Glob - 1 )
        for columnIndex in range ( 0, MazeColumns_Glob - 1 ):
            verticalWallsUp[columnIndex] = WallsUp_Glob[interiorWallIndex]
            interiorWallIndex = interiorWallIndex + 1

        printVerticalWalls ( verticalWallsUp )

        if ( rowIndex == MazeRows_Glob - 1 ):
            printHorizontalWallsAll()
        else:
            horizontalWallsUp = [False] * MazeColumns_Glob
            for columnIndex in range ( 0, MazeColumns_Glob ):
                horizontalWallsUp[columnIndex] = WallsUp_Glob[interiorWallIndex]
                interiorWallIndex = interiorWallIndex + 1

            printHorizontalWalls ( horizontalWallsUp )

    print()



# Simple sleep function
def sleepHalfSecond():
    time.sleep ( 0.5 )



# Kruskal's algorithm.
# The simple description of the algorithm is first place each 
# cell in its own group.  Then process all walls in random order,
# if the cells on either side of the wall are in separate groups, 
# remove the wall and merge the groups.  Repeat until all 
# cells are now in the same group.
def buildMazeKruskal():
    # Identify the cells each wall connects.
    wallConnections = [[0, 0] for _ in range ( InteriorWallCount_Glob )]
    wallRemoveList = [0] * InteriorWallCount_Glob
    cellToGroup = [0] * ( MazeRows_Glob * MazeColumns_Glob )
    groupCells = [[NO_CELL for _ in range ( MazeRows_Glob * MazeColumns_Glob )] for _ in range ( MazeRows_Glob * MazeColumns_Glob )]

    wallIndex = 0
    for rowIndex in range ( 0, MazeRows_Glob ):
        # Track the first cell in the current row
        firstCellInRow = rowIndex * MazeColumns_Glob

        # Note the 0..mazeColumns minus 2, one less vertical wall
        # than the number of columns.
        for verticalWallIndex in range ( 0, MazeColumns_Glob - 1 ):
            leftCell = firstCellInRow + verticalWallIndex
            rightCell = leftCell + 1
            wallConnections[wallIndex][FIRST_CELL] = leftCell
            wallConnections[wallIndex][SECOND_CELL] = rightCell
            wallIndex = wallIndex + 1

        # The last row will have no interior horizontal walls below
        # it, so will be skipped.
        if ( wallIndex < InteriorWallCount_Glob ):
            for horizontalWallIndex in range ( 0, MazeColumns_Glob ):
                upperCell = firstCellInRow + horizontalWallIndex
                lowerCell = upperCell + MazeColumns_Glob
                wallConnections[wallIndex][FIRST_CELL] = upperCell
                wallConnections[wallIndex][SECOND_CELL] = lowerCell
                wallIndex = wallIndex + 1

    for cellIndex in range ( 0, MazeRows_Glob * MazeColumns_Glob ):
        cellToGroup[cellIndex] = cellIndex

        for innerCellIndex in range ( 0, MazeRows_Glob * MazeColumns_Glob ):
            if ( innerCellIndex == 0 ):
                groupCells[cellIndex][innerCellIndex] = cellIndex
            else:
                groupCells[cellIndex][innerCellIndex] = NO_CELL

    mazeComplete = False

    for wallIndexLoop in range ( 0, InteriorWallCount_Glob ):
        wallRemoveList[wallIndexLoop] = wallIndexLoop

    # Fisher-Yates shuffle
    for i in range ( InteriorWallCount_Glob - 1, 0, -1 ):
        j = random.randint ( 0, i )
        temp = wallRemoveList[i]
        wallRemoveList[i] = wallRemoveList[j]
        wallRemoveList[j] = temp

    # Perform Kruskal's algorithm.
    for removeWallIndex in range ( 0, InteriorWallCount_Glob ):
        nextWallToCheck = wallRemoveList[removeWallIndex]

        # If the two cells connected to this wall are not part 
        # of the same group, remove the wall and merge the 
        # groups.
        firstCell = wallConnections[nextWallToCheck][FIRST_CELL]
        firstCellGroupIndex = cellToGroup[firstCell]
        secondCell = wallConnections[nextWallToCheck][SECOND_CELL]
        secondCellGroupIndex = cellToGroup[secondCell]
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
                    # Move our empty index to the next cell in this array.
                    nextEmptyFirstGroupIndex = nextEmptyFirstGroupIndex + 1
                    # Mark this cell as part of the first group.
                    cellToGroup[cellToMove] = firstCellGroupIndex
                    # Remove the cell from the second group (set the
                    # array entry to NO_CELL)
                    groupCells[secondCellGroupIndex][groupCellIndex] = NO_CELL

                    if ( nextEmptyFirstGroupIndex >= MazeColumns_Glob * MazeRows_Glob ):
                        mazeComplete = True

            sleepHalfSecond()

            printMaze()

            if ( mazeComplete == True ):
                break



# Note we could use command line arguments instead of user 
# prompts to get the maze size.
def main():
    global MazeColumns_Glob, MazeRows_Glob, AllHorizontalWallsUp_Glob, InteriorWallCount_Glob, WallsUp_Glob

    # Prompt the user for maze size
    MazeColumns_Glob = 0
    while ( MazeColumns_Glob <= 0 ):
        userInput = input ( "Please enter number of columns for maze, must be greater than 1: " )
        try:
            tempValue = int ( userInput )
            if ( tempValue > 1 ):
                MazeColumns_Glob = tempValue
        except ValueError:
            pass

    MazeRows_Glob = 0
    while ( MazeRows_Glob <= 0 ):
        userInput = input ( "Please enter number of rows for maze, must be greater than 1: " )
        try:
            tempValue = int ( userInput )
            if ( tempValue > 1 ):
                MazeRows_Glob = tempValue
        except ValueError:
            pass

    # Setup maze datastructures for the user entered size.
    AllHorizontalWallsUp_Glob = [True] * MazeColumns_Glob

    InteriorWallCount_Glob = MazeRows_Glob * ( MazeColumns_Glob - 1 ) + ( MazeRows_Glob - 1 ) * MazeColumns_Glob

    WallsUp_Glob = [True] * InteriorWallCount_Glob

    buildMazeKruskal()



if __name__ == "__main__":
    main()
