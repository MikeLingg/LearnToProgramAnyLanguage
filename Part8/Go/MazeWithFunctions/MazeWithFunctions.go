package main

import (
    "fmt"
    "math/rand"
    "strconv"
    "strings"
    "time"
)



// Define directions
const LEFT = 0
const UP = 1
const RIGHT = 2
const DOWN = 3

const FIRST_CELL = 0
const SECOND_CELL = 1

const NO_CELL = -1



// Global variables
var AllHorizontalWallsUp_Glob []bool
var WallsUp_Glob []bool
var InteriorWallCount_Glob int
var MazeRows_Glob int
var MazeColumns_Glob int



// +-+-+-+
// Prints a horizontal wall, similar to the example above, 
// with walls down based on the provided parameters.
// HorizontalWallsUp_Par: Boolean slice of size MazeColumns_Glob, 
//     True indicates the wall should be printed as up.
func printHorizontalWalls ( HorizontalWallsUp_Par []bool ) {
    fmt.Print ( "+" )
    for horizontalWallIndex := 0; horizontalWallIndex < MazeColumns_Glob; horizontalWallIndex++ {
        if ( HorizontalWallsUp_Par[horizontalWallIndex] == true ) {
            fmt.Print ( "-" )
        } else {
            fmt.Print ( " " )
        }
        fmt.Print ( "+" )
    }
    fmt.Println()
}



// +-+-+-+
// Prints a horizontal wall, similar to the example above, with all walls up.
func printHorizontalWallsAll() {
    printHorizontalWalls ( AllHorizontalWallsUp_Glob )
}



// | | | |
// Prints a vertical wall, similar to the example above, 
// with walls down based on the provided parameters.
// VerticalWallsUp_Par: Boolean slice of size MazeColumns_Glob - 1, 
//     True indicates the wall should be printed as up.
func printVerticalWalls ( VerticalWallsUp_Par []bool ) {
    // First wall is an exterior wall, always up.
    fmt.Print ( "|" )
    for verticalWallIndex := 0; verticalWallIndex < MazeColumns_Glob - 1; verticalWallIndex++ {
        fmt.Print ( " " )
        if ( VerticalWallsUp_Par[verticalWallIndex] == true ) {
            fmt.Print ( "|" )
        } else {
            fmt.Print ( " " )
        }
    }
    // Last wall exterior, always up.
    fmt.Print ( " " )
    fmt.Print ( "|" )
    fmt.Println()
}



// Loop through the rows of the maze and print the maze 
// based on WallsUp_Glob
func printMaze() {
    interiorWallIndex := 0

    // First row is exterior walls
    printHorizontalWallsAll()
    for rowIndex := 0; rowIndex < MazeRows_Glob; rowIndex++ {
        verticalWallsUp := make ( []bool, MazeColumns_Glob - 1 )
        for columnIndex := 0; columnIndex < MazeColumns_Glob - 1; columnIndex++ {
            verticalWallsUp[columnIndex] = WallsUp_Glob[interiorWallIndex]
            interiorWallIndex = interiorWallIndex + 1
        }

        printVerticalWalls ( verticalWallsUp )

        if ( rowIndex == MazeRows_Glob - 1 ) {
            printHorizontalWallsAll()
        } else {
            horizontalWallsUp := make ( []bool, MazeColumns_Glob )
            for columnIndex := 0; columnIndex < MazeColumns_Glob; columnIndex++ {
                horizontalWallsUp[columnIndex] = WallsUp_Glob[interiorWallIndex]
                interiorWallIndex = interiorWallIndex + 1
            }

            printHorizontalWalls ( horizontalWallsUp )
        }
    }

    fmt.Println()
}



// Simple sleep function
func sleepHalfSecond() {
    time.Sleep ( 500 * time.Millisecond )
}



// Kruskal's algorithm.
// The simple description of the algorithm is first place each 
// cell in its own group.  Then process all walls in random order,
// if the cells on either side of the wall are in separate groups, 
// remove the wall and merge the groups.  Repeat until all 
// cells are now in the same group.
func buildMazeKruskal() {
    // Identify the cells each wall connects.
    wallConnections := make ( [][]int, InteriorWallCount_Glob )
    for i := range wallConnections {
        wallConnections[i] = make ( []int, 2 )
    }
    
    wallRemoveList := make ( []int, InteriorWallCount_Glob )
    cellToGroup := make ( []int, MazeRows_Glob * MazeColumns_Glob )
    
    groupCells := make ( [][]int, MazeRows_Glob * MazeColumns_Glob )
    for i := range groupCells {
        groupCells[i] = make ( []int, MazeRows_Glob * MazeColumns_Glob )
    }

    wallIndex := 0
    for rowIndex := 0; rowIndex < MazeRows_Glob; rowIndex++ {
        // Track the first cell in the current row
        firstCellInRow := rowIndex * MazeColumns_Glob

        // Note the 0..MazeColumns_Glob minus 2, one less vertical wall
        // than the number of columns.
        for verticalWallIndex := 0; verticalWallIndex < MazeColumns_Glob - 1; verticalWallIndex++ {
            leftCell := firstCellInRow + verticalWallIndex
            rightCell := leftCell + 1
            wallConnections[wallIndex][FIRST_CELL] = leftCell
            wallConnections[wallIndex][SECOND_CELL] = rightCell
            wallIndex = wallIndex + 1
        }

        // The last row will have no interior horizontal walls below
        // it, so will be skipped.
        if ( wallIndex < InteriorWallCount_Glob ) {
            for horizontalWallIndex := 0; horizontalWallIndex < MazeColumns_Glob; horizontalWallIndex++ {
                upperCell := firstCellInRow + horizontalWallIndex
                lowerCell := upperCell + MazeColumns_Glob
                wallConnections[wallIndex][FIRST_CELL] = upperCell
                wallConnections[wallIndex][SECOND_CELL] = lowerCell
                wallIndex = wallIndex + 1
            }
        }
    }

    for cellIndex := 0; cellIndex < MazeRows_Glob * MazeColumns_Glob; cellIndex++ {
        cellToGroup[cellIndex] = cellIndex

        for innerCellIndex := 0; innerCellIndex < MazeRows_Glob * MazeColumns_Glob; innerCellIndex++ {
            if ( innerCellIndex == 0 ) {
                groupCells[cellIndex][innerCellIndex] = cellIndex
            } else {
                groupCells[cellIndex][innerCellIndex] = NO_CELL
            }
        }
    }

    mazeComplete := false

    for wallIndexLoop := 0; wallIndexLoop < InteriorWallCount_Glob; wallIndexLoop++ {
        wallRemoveList[wallIndexLoop] = wallIndexLoop
    }

    // Fisher-Yates shuffle
    for i := InteriorWallCount_Glob - 1; i > 0; i-- {
        j := rand.Intn ( i + 1 )
        temp := wallRemoveList[i]
        wallRemoveList[i] = wallRemoveList[j]
        wallRemoveList[j] = temp
    }

    // Perform Kruskal's algorithm.
    for removeWallIndex := 0; removeWallIndex < InteriorWallCount_Glob; removeWallIndex++ {
        nextWallToCheck := wallRemoveList[removeWallIndex]

        // If the two cells connected to this wall are not part 
        // of the same group, remove the wall and merge the 
        // groups.
        firstCell := wallConnections[nextWallToCheck][FIRST_CELL]
        firstCellGroupIndex := cellToGroup[firstCell]
        secondCell := wallConnections[nextWallToCheck][SECOND_CELL]
        secondCellGroupIndex := cellToGroup[secondCell]
        if ( firstCellGroupIndex != secondCellGroupIndex ) {
            WallsUp_Glob[nextWallToCheck] = false

            // Loop through the indices of all cells in the first 
            // group until we find a NO_CELL indicating no cell here.
            nextEmptyFirstGroupIndex := 0
            for cellIndex := 0; cellIndex < MazeColumns_Glob * MazeRows_Glob; cellIndex++ {
                if ( groupCells[firstCellGroupIndex][cellIndex] == NO_CELL ) {
                    nextEmptyFirstGroupIndex = cellIndex
                    break
                }
            }

            // Loop through the indices of all cells in the second group,
            // move each cell to the first group, and set that cell's 
            // group to the first group index.
            for groupCellIndex := MazeColumns_Glob * MazeRows_Glob - 1; groupCellIndex >= 0; groupCellIndex-- {
                // Skip until we reach valid cells
                if ( groupCells[secondCellGroupIndex][groupCellIndex] != NO_CELL ) {
                    // Get the id number of the cell to move from 
                    // the second group to the first group
                    cellToMove := groupCells[secondCellGroupIndex][groupCellIndex]

                    // Move the cell number from the second group 
                    // to the first group
                    groupCells[firstCellGroupIndex][nextEmptyFirstGroupIndex] = cellToMove
                    // Move our empty index to the next cell in this array.
                    nextEmptyFirstGroupIndex = nextEmptyFirstGroupIndex + 1
                    // Mark this cell as part of the first group.
                    cellToGroup[cellToMove] = firstCellGroupIndex
                    // Remove the cell from the second group (set the
                    // array entry to NO_CELL)
                    groupCells[secondCellGroupIndex][groupCellIndex] = NO_CELL

                    if ( nextEmptyFirstGroupIndex >= MazeColumns_Glob * MazeRows_Glob ) {
                        mazeComplete = true
                    }
                }
            }

            sleepHalfSecond()

            printMaze()

            if ( mazeComplete == true ) {
                break
            }
        }
    }
}



// Note we could use command line arguments instead of user 
// prompts to get the maze size.
func main() {
    // Seed random number generator
    rand.Seed ( time.Now().UnixNano() )

    // Prompt the user for maze size
    MazeColumns_Glob = 0
    for MazeColumns_Glob <= 0 {
        fmt.Print ( "Please enter number of columns for maze, must be greater than 1: " )
        var userInput string
        fmt.Scanln ( &userInput )
        
        userInput = strings.TrimSpace ( userInput )
        if tempValue, err := strconv.Atoi ( userInput ); err == nil {
            if ( tempValue > 1 ) {
                MazeColumns_Glob = tempValue
            }
        }
    }

    MazeRows_Glob = 0
    for MazeRows_Glob <= 0 {
        fmt.Print ( "Please enter number of rows for maze, must be greater than 1: " )
        var userInput string
        fmt.Scanln ( &userInput )
        
        userInput = strings.TrimSpace ( userInput )
        if tempValue, err := strconv.Atoi ( userInput ); err == nil {
            if ( tempValue > 1 ) {
                MazeRows_Glob = tempValue
            }
        }
    }

    // Setup maze datastructures for the user entered size.
    AllHorizontalWallsUp_Glob = make ( []bool, MazeColumns_Glob )
    for i := 0; i < MazeColumns_Glob; i++ {
        AllHorizontalWallsUp_Glob[i] = true
    }

    InteriorWallCount_Glob = MazeRows_Glob * ( MazeColumns_Glob - 1 ) + ( MazeRows_Glob - 1 ) * MazeColumns_Glob

    WallsUp_Glob = make ( []bool, InteriorWallCount_Glob )
    for i := 0; i < InteriorWallCount_Glob; i++ {
        WallsUp_Glob[i] = true
    }

    buildMazeKruskal()
}
