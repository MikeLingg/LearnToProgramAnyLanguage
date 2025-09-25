package main

import (
    "bufio"
    "fmt"
    "math/rand"
    "os"
    "strconv"
    "strings"
    "time"
)



// Define directions
const LEFT int = 0
const UP int = 1
const RIGHT int = 2
const DOWN int = 3

const FIRST_CELL int = 0
const SECOND_CELL int = 1

const NO_CELL int = -1

// Define algorithm types
const KRUSKAL_ALGORITHM int = 1
const PRIM_ALGORITHM int = 2
const DEPTH_FIRST_ALGORITHM int = 3
const BINARY_TREE_ALGORITHM int = 4
const RECURSIVE_DIVISION_ALGORITHM int = 5



// Global variables
var AllHorizontalWallsUp_Glob []bool
var WallsUp_Glob []bool
var InteriorWallCount_Glob int
var MazeRows_Glob int
var MazeColumns_Glob int

// Additional globals for algorithm state sharing
var CellVisited_Glob []bool
var FrontierWalls_Glob []int
var FrontierWallCount_Glob int
var CellInMaze_Glob []bool

// Lookup tables for wall/cell relationships
var WallToCells_Glob [][]int
var CellToWalls_Glob [][]int



// +-+-+-+
// Prints a horizontal wall, similar to the example above, 
// with walls down based on the provided parameters.
// HorizontalWallsUp_Par: Boolean array of size MazeColumns_Glob, 
//     True indicates the wall should be printed as up.
func printHorizontalWalls ( HorizontalWallsUp_Par []bool ) {
    fmt.Print ( "+" )
    for horizontalWallIndex := 0; ( horizontalWallIndex < MazeColumns_Glob ); horizontalWallIndex = horizontalWallIndex + 1 {
        if ( HorizontalWallsUp_Par[horizontalWallIndex] == true ) {
            fmt.Print ( "-" )
        } else {
            fmt.Print ( " " )
        }
        fmt.Print ( "+" )
    }
    fmt.Println ()
}



// +-+-+-+
// Prints a horizontal wall, similar to the example above, with all walls up.
func printAllHorizontalWalls () {
    printHorizontalWalls ( AllHorizontalWallsUp_Glob )
}



// | | | |
// Prints a vertical wall, similar to the example above, 
// with walls down based on the provided parameters.
// VerticalWallsUp_Par: Boolean array of size MazeColumns_Glob - 1, 
//     True indicates the wall should be printed as up.
func printVerticalWalls ( VerticalWallsUp_Par []bool ) {
    // First wall is an exterior wall, always up.
    fmt.Print ( "|" )
    for verticalWallIndex := 0; ( verticalWallIndex < MazeColumns_Glob - 1 ); verticalWallIndex = verticalWallIndex + 1 {
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
    fmt.Println ()
}



// Loop through the rows of the maze and print the maze 
// based on WallsUp_Glob
func printMaze () {
    interiorWallIndex := 0

    // First row is exterior walls
    printAllHorizontalWalls ()
    for rowIndex := 0; ( rowIndex < MazeRows_Glob ); rowIndex = rowIndex + 1 {
        verticalWallsUp := make ( []bool, MazeColumns_Glob - 1 )
        for columnIndex := 0; ( columnIndex < MazeColumns_Glob - 1 ); columnIndex = columnIndex + 1 {
            verticalWallsUp[columnIndex] = WallsUp_Glob[interiorWallIndex]
            interiorWallIndex = interiorWallIndex + 1
        }

        printVerticalWalls ( verticalWallsUp )

        if ( rowIndex == MazeRows_Glob - 1 ) {
            printAllHorizontalWalls ()
        } else {
            horizontalWallsUp := make ( []bool, MazeColumns_Glob )
            for columnIndex := 0; ( columnIndex < MazeColumns_Glob ); columnIndex = columnIndex + 1 {
                horizontalWallsUp[columnIndex] = WallsUp_Glob[interiorWallIndex]
                interiorWallIndex = interiorWallIndex + 1
            }

            printHorizontalWalls ( horizontalWallsUp )
        }
    }

    fmt.Println ()
}



// Simple sleep function
func sleepHalfSecond () {
    time.Sleep ( 500 * time.Millisecond )
}



// Initialize lookup tables for wall/cell relationships.
// AlgorithmType_Par: Algorithm constant to determine which tables to build
// Must be called after maze dimensions are set.
func initializeLookupTables ( AlgorithmType_Par int ) {
    // Build WallToCells_Glob for algorithms that need wall-to-cell lookups
    if ( AlgorithmType_Par == KRUSKAL_ALGORITHM || AlgorithmType_Par == PRIM_ALGORITHM || 
         AlgorithmType_Par == DEPTH_FIRST_ALGORITHM || AlgorithmType_Par == BINARY_TREE_ALGORITHM || 
         AlgorithmType_Par == RECURSIVE_DIVISION_ALGORITHM ) {
        WallToCells_Glob = make ( [][]int, InteriorWallCount_Glob )
        for wallIndex := 0; ( wallIndex < InteriorWallCount_Glob ); wallIndex = wallIndex + 1 {
            WallToCells_Glob[wallIndex] = make ( []int, 2 )
        }
        
        wallIndex := 0
        for rowIndex := 0; ( rowIndex < MazeRows_Glob ); rowIndex = rowIndex + 1 {
            // Track the first cell in the current row
            firstCellInRow := rowIndex * MazeColumns_Glob

            // Note the 0..MazeColumns_Glob minus 2, one less vertical wall
            // than the number of columns.
            for verticalWallIndex := 0; ( verticalWallIndex < MazeColumns_Glob - 1 ); verticalWallIndex = verticalWallIndex + 1 {
                leftCell := firstCellInRow + verticalWallIndex
                rightCell := leftCell + 1
                WallToCells_Glob[wallIndex][FIRST_CELL] = leftCell
                WallToCells_Glob[wallIndex][SECOND_CELL] = rightCell
                wallIndex = wallIndex + 1
            }

            // The last row will have no interior horizontal walls below
            // it, so will be skipped.
            if ( wallIndex < InteriorWallCount_Glob ) {
                for horizontalWallIndex := 0; ( horizontalWallIndex < MazeColumns_Glob ); horizontalWallIndex = horizontalWallIndex + 1 {
                    upperCell := firstCellInRow + horizontalWallIndex
                    lowerCell := upperCell + MazeColumns_Glob
                    WallToCells_Glob[wallIndex][FIRST_CELL] = upperCell
                    WallToCells_Glob[wallIndex][SECOND_CELL] = lowerCell
                    wallIndex = wallIndex + 1
                }
            }
        }
    }

    // Build CellToWalls_Glob for algorithms that need cell-to-wall lookups
    if ( AlgorithmType_Par == PRIM_ALGORITHM || AlgorithmType_Par == DEPTH_FIRST_ALGORITHM || 
         AlgorithmType_Par == BINARY_TREE_ALGORITHM || AlgorithmType_Par == RECURSIVE_DIVISION_ALGORITHM ) {
        CellToWalls_Glob = make ( [][]int, MazeRows_Glob * MazeColumns_Glob )
        for cellIndex := 0; ( cellIndex < MazeRows_Glob * MazeColumns_Glob ); cellIndex = cellIndex + 1 {
            CellToWalls_Glob[cellIndex] = make ( []int, 4 )
        }
        
        for cellIndex := 0; ( cellIndex < MazeRows_Glob * MazeColumns_Glob ); cellIndex = cellIndex + 1 {
            cellRow := cellIndex / MazeColumns_Glob
            cellCol := cellIndex % MazeColumns_Glob
            
            // Initialize all directions to NO_CELL (invalid wall)
            CellToWalls_Glob[cellIndex][LEFT] = NO_CELL
            CellToWalls_Glob[cellIndex][UP] = NO_CELL
            CellToWalls_Glob[cellIndex][RIGHT] = NO_CELL
            CellToWalls_Glob[cellIndex][DOWN] = NO_CELL
            
            // Find walls by checking which walls connect to this cell
            for wallIndex := 0; ( wallIndex < InteriorWallCount_Glob ); wallIndex = wallIndex + 1 {
                firstCellIndex := WallToCells_Glob[wallIndex][FIRST_CELL]
                secondCellIndex := WallToCells_Glob[wallIndex][SECOND_CELL]
                
                if ( firstCellIndex == cellIndex ) {
                    // This wall connects from our cell to secondCell
                    secondRow := secondCellIndex / MazeColumns_Glob
                    secondCol := secondCellIndex % MazeColumns_Glob
                    
                    if ( secondRow == cellRow && secondCol == cellCol + 1 ) {
                        // Wall goes RIGHT
                        CellToWalls_Glob[cellIndex][RIGHT] = wallIndex
                    } else if ( secondRow == cellRow + 1 && secondCol == cellCol ) {
                        // Wall goes DOWN
                        CellToWalls_Glob[cellIndex][DOWN] = wallIndex
                    }
                } else if ( secondCellIndex == cellIndex ) {
                    // This wall connects from firstCellIndex to our cell
                    firstRow := firstCellIndex / MazeColumns_Glob
                    firstCol := firstCellIndex % MazeColumns_Glob
                    
                    if ( firstRow == cellRow && firstCol == cellCol - 1 ) {
                        // Wall comes from LEFT
                        CellToWalls_Glob[cellIndex][LEFT] = wallIndex
                    } else if ( firstRow == cellRow - 1 && firstCol == cellCol ) {
                        // Wall comes from UP
                        CellToWalls_Glob[cellIndex][UP] = wallIndex
                    }
                }
            }
        }
    }
}



// Add a wall to frontier if not already there.
// WallIndex_Par: Index of wall to add to frontier, must be 0 to InteriorWallCount_Glob-1
func addWallToFrontier ( WallIndex_Par int ) {
    // Check if wall already in frontier
    alreadyInFrontier := false
    for wallIndex := 0; ( wallIndex < FrontierWallCount_Glob ); wallIndex = wallIndex + 1 {
        if ( FrontierWalls_Glob[wallIndex] == WallIndex_Par ) {
            alreadyInFrontier = true
            break
        }
    }

    if ( alreadyInFrontier == false ) {
        FrontierWalls_Glob[FrontierWallCount_Glob] = WallIndex_Par
        FrontierWallCount_Glob = FrontierWallCount_Glob + 1
    }
}



// Add all walls adjacent to a cell to the frontier list.
// CellIndex_Par: Index of the cell whose adjacent walls should be added to frontier: 0 .. ( MazeRows_Glob * MazeColumns_Glob - 1 )
func addCellWallsToFrontier ( CellIndex_Par int ) {
    // Check all four directions
    if ( CellToWalls_Glob[CellIndex_Par][UP] != NO_CELL ) {
        addWallToFrontier ( CellToWalls_Glob[CellIndex_Par][UP] )
    }
    if ( CellToWalls_Glob[CellIndex_Par][DOWN] != NO_CELL ) {
        addWallToFrontier ( CellToWalls_Glob[CellIndex_Par][DOWN] )
    }
    if ( CellToWalls_Glob[CellIndex_Par][LEFT] != NO_CELL ) {
        addWallToFrontier ( CellToWalls_Glob[CellIndex_Par][LEFT] )
    }
    if ( CellToWalls_Glob[CellIndex_Par][RIGHT] != NO_CELL ) {
        addWallToFrontier ( CellToWalls_Glob[CellIndex_Par][RIGHT] )
    }
}



// Recursively divide an area with walls.
// StartRow_Par: Starting row of area to divide, must be 0 to MazeRows_Glob-1
// EndRow_Par: Ending row of area to divide, must be StartRow_Par to MazeRows_Glob-1
// StartCol_Par: Starting column of area to divide, must be 0 to MazeColumns_Glob-1
// EndCol_Par: Ending column of area to divide, must be StartCol_Par to MazeColumns_Glob-1
func divideArea ( StartRow_Par int, EndRow_Par int, StartCol_Par int, EndCol_Par int ) {
    height := EndRow_Par - StartRow_Par + 1
    width := EndCol_Par - StartCol_Par + 1

    // Base case - area too small to divide
    if ( height >= 2 || width >= 2 ) {
        // Choose whether to divide horizontally or vertically
        divideHorizontally := false
        if ( height > width ) {
            divideHorizontally = true
        } else if ( width > height ) {
            divideHorizontally = false
        } else {
            // Square area - choose randomly
            divideHorizontally = ( rand.Intn ( 2 ) == 0 )
        }

        if ( divideHorizontally == true && height > 1 ) {
            // Choose random row to divide on
            divideRow := StartRow_Par + rand.Intn ( EndRow_Par - StartRow_Par )

            // Add horizontal wall
            for wallCol := StartCol_Par; ( wallCol <= EndCol_Par ); wallCol = wallCol + 1 {
                cellIndex := divideRow * MazeColumns_Glob + wallCol
                if ( cellIndex < MazeRows_Glob * MazeColumns_Glob ) {
                    wallIndex := CellToWalls_Glob[cellIndex][DOWN]
                    if ( wallIndex != NO_CELL ) {
                        WallsUp_Glob[wallIndex] = true
                    }
                }
            }

            // Choose random gap in the wall
            gapCol := StartCol_Par + rand.Intn ( EndCol_Par - StartCol_Par + 1 )
            gapCellIndex := divideRow * MazeColumns_Glob + gapCol
            if ( gapCellIndex < MazeRows_Glob * MazeColumns_Glob ) {
                gapWallIndex := CellToWalls_Glob[gapCellIndex][DOWN]
                if ( gapWallIndex != NO_CELL ) {
                    WallsUp_Glob[gapWallIndex] = false
                }
            }

            sleepHalfSecond ()
            printMaze ()

            // Recursively divide the two areas
            divideArea ( StartRow_Par, divideRow, StartCol_Par, EndCol_Par )
            divideArea ( divideRow + 1, EndRow_Par, StartCol_Par, EndCol_Par )
        } else if ( divideHorizontally == false && width > 1 ) {
            // Choose random column to divide on
            divideCol := StartCol_Par + rand.Intn ( EndCol_Par - StartCol_Par )

            // Add vertical wall
            for cellRow := StartRow_Par; ( cellRow <= EndRow_Par ); cellRow = cellRow + 1 {
                cellIndex := cellRow * MazeColumns_Glob + divideCol
                if ( cellIndex < MazeRows_Glob * MazeColumns_Glob ) {
                    wallIndex := CellToWalls_Glob[cellIndex][RIGHT]
                    if ( wallIndex != NO_CELL ) {
                        WallsUp_Glob[wallIndex] = true
                    }
                }
            }

            // Choose random gap in the wall
            gapRow := StartRow_Par + rand.Intn ( EndRow_Par - StartRow_Par + 1 )
            gapCellIndex := gapRow * MazeColumns_Glob + divideCol
            if ( gapCellIndex < MazeRows_Glob * MazeColumns_Glob ) {
                gapWallIndex := CellToWalls_Glob[gapCellIndex][RIGHT]
                if ( gapWallIndex != NO_CELL ) {
                    WallsUp_Glob[gapWallIndex] = false
                }
            }

            sleepHalfSecond ()
            printMaze ()

            // Recursively divide the two areas
            divideArea ( StartRow_Par, EndRow_Par, StartCol_Par, divideCol )
            divideArea ( StartRow_Par, EndRow_Par, divideCol + 1, EndCol_Par )
        }
    }
}



// Kruskal's algorithm.
// The simple description of the algorithm is first place each 
// cell in its own group.  Then process all walls in random order,
// if the cells on either side of the wall are in separate groups, 
// remove the wall and merge the groups.  Repeat until all 
// cells are now in the same group.
func buildMazeKruskal () {
    // Identify which cells are a part of each group
    // Note each array is large enough to fit all cells,
    // NO_CELL indicates no cell is assigned to this index.
    // Languages like python that make adding/removing 
    // array items easy will not need this, for C arrays
    // we will discuss reallocation or linked list in 
    // later videos for covering changing array sizes.
    cellToGroup := make ( []int, MazeRows_Glob * MazeColumns_Glob )
    groupCells := make ( [][]int, MazeRows_Glob * MazeColumns_Glob )

    for cellIndex := 0; ( cellIndex < MazeRows_Glob * MazeColumns_Glob ); cellIndex = cellIndex + 1 {
        cellToGroup[cellIndex] = cellIndex
        groupCells[cellIndex] = make ( []int, MazeRows_Glob * MazeColumns_Glob )

        for groupCellIndex := 0; ( groupCellIndex < MazeRows_Glob * MazeColumns_Glob ); groupCellIndex = groupCellIndex + 1 {
            if ( groupCellIndex == 0 ) {
                groupCells[cellIndex][groupCellIndex] = cellIndex
            } else {
                groupCells[cellIndex][groupCellIndex] = NO_CELL
            }
        }
    }

    mazeComplete := false

    wallRemoveList := make ( []int, InteriorWallCount_Glob )
    for wallIndex := 0; ( wallIndex < InteriorWallCount_Glob ); wallIndex = wallIndex + 1 {
        wallRemoveList[wallIndex] = wallIndex
    }
    
    // Fisher-Yates shuffle
    for shuffleIndex := InteriorWallCount_Glob - 1; ( shuffleIndex > 0 ); shuffleIndex = shuffleIndex - 1 {
        randomIndex := rand.Intn ( shuffleIndex + 1 )
        temp := wallRemoveList[shuffleIndex]
        wallRemoveList[shuffleIndex] = wallRemoveList[randomIndex]
        wallRemoveList[randomIndex] = temp
    }

    // Perform Kruskal's algorithm.
    for removeWallIndex := 0; ( removeWallIndex < InteriorWallCount_Glob ); removeWallIndex = removeWallIndex + 1 {
        nextWallToCheck := wallRemoveList[removeWallIndex]

        // If the two cells connected to this wall are not part 
        // of the same group, remove the wall and merge the 
        // groups.
        firstCellIndex := WallToCells_Glob[nextWallToCheck][FIRST_CELL]
        firstCellGroupIndex := cellToGroup[firstCellIndex]
        secondCellIndex := WallToCells_Glob[nextWallToCheck][SECOND_CELL]
        secondCellGroupIndex := cellToGroup[secondCellIndex]
        if ( firstCellGroupIndex != secondCellGroupIndex ) {
            WallsUp_Glob[nextWallToCheck] = false

            // Loop through the indices of all cells in the first 
            // group until we find a NO_CELL indicating no cell here.
            nextEmptyFirstGroupIndex := 0
            for cellIndex := 0; ( cellIndex < MazeColumns_Glob * MazeRows_Glob ); cellIndex = cellIndex + 1 {
                if ( groupCells[firstCellGroupIndex][cellIndex] == NO_CELL ) {
                    nextEmptyFirstGroupIndex = cellIndex
                    break
                }
            }

            // Loop through the indices of all cells in the second group,
            // move each cell to the first group, and set that cell's 
            // group to the first group index.
            for groupCellIndex := MazeColumns_Glob * MazeRows_Glob - 1; ( groupCellIndex >= 0 ); groupCellIndex = groupCellIndex - 1 {
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

            sleepHalfSecond ()
            printMaze ()

            if ( mazeComplete == true ) {
                break
            }
        }
    }
}



// Prim's algorithm for maze generation.
// Start with a random cell, mark it as part of the maze.
// Repeatedly pick a random wall from cells in the maze that 
// connects to a cell not in the maze, remove the wall and 
// add the new cell to the maze.
func buildMazePrim () {
    // Initialize algorithm state
    FrontierWallCount_Glob = 0

    // Track how many cells have been added to the maze
    cellsInMaze := 0

    // Start with a random cell
    startCell := rand.Intn ( MazeRows_Glob * MazeColumns_Glob )
    CellInMaze_Glob[startCell] = true
    cellsInMaze = cellsInMaze + 1

    // Add all walls adjacent to the start cell to frontier
    addCellWallsToFrontier ( startCell )

    // Continue until all cells are in the maze
    for ( cellsInMaze < MazeRows_Glob * MazeColumns_Glob ) {
        // Pick a random wall from frontier
        randomWallIndex := rand.Intn ( FrontierWallCount_Glob )
        wallToCheck := FrontierWalls_Glob[randomWallIndex]

        // Remove this wall from frontier list by replacing 
        // it with the last wall in the list
        FrontierWalls_Glob[randomWallIndex] = FrontierWalls_Glob[FrontierWallCount_Glob - 1]
        FrontierWallCount_Glob = FrontierWallCount_Glob - 1

        // Get the two cells this wall connects
        firstCellIndex := WallToCells_Glob[wallToCheck][FIRST_CELL]
        secondCellIndex := WallToCells_Glob[wallToCheck][SECOND_CELL]

        // If one cell is already in the maze and the 
        // other is not, remove the wall to connect the 
        // outside cell to the maze
        if ( CellInMaze_Glob[firstCellIndex] != CellInMaze_Glob[secondCellIndex] ) {
            WallsUp_Glob[wallToCheck] = false

            outerCellIndex := -1

            // Add the outside cell to the maze
            if ( CellInMaze_Glob[firstCellIndex] == false ) {
                outerCellIndex = firstCellIndex
            } else {
                outerCellIndex = secondCellIndex
            }

            CellInMaze_Glob[outerCellIndex] = true
            cellsInMaze = cellsInMaze + 1
            addCellWallsToFrontier ( outerCellIndex )

            sleepHalfSecond ()
            printMaze ()
        }
    }
}



// Depth-first search maze generation using recursive backtracking.
// Start at a random cell, randomly walk to neighbors outside 
// the maze,removing walls as you go. 
// When all neighbors are in the maze, backtrack to a cell 
// with neighbors outside the maze.
func buildMazeDepthFirst () {
    // Stack for backtracking - store cell indices
    cellStack := make ( []int, MazeRows_Glob * MazeColumns_Glob )
    stackSize := 0

    // Start with random cell
    currentCellIndex := rand.Intn ( MazeRows_Glob * MazeColumns_Glob )
    CellVisited_Glob[currentCellIndex] = true

    // Push starting cell onto stack
    cellStack[stackSize] = currentCellIndex
    stackSize = stackSize + 1

    for ( stackSize > 0 ) {
        // Create randomized direction list
        randomizedDirections := []int{LEFT, UP, RIGHT, DOWN}
        
        // Fisher-Yates shuffle the directions
        for shuffleIndex := 3; ( shuffleIndex >= 1 ); shuffleIndex = shuffleIndex - 1 {
            randomIndex := rand.Intn ( shuffleIndex + 1 )
            temp := randomizedDirections[shuffleIndex]
            randomizedDirections[shuffleIndex] = randomizedDirections[randomIndex]
            randomizedDirections[randomIndex] = temp
        }

        foundNeighbor := false
        nextCellIndex := NO_CELL
        wallIndex := NO_CELL

        // Check directions in random order until we find an unvisited neighbor
        for directionIndex := 0; ( directionIndex < 4 ); directionIndex = directionIndex + 1 {
            direction := randomizedDirections[directionIndex]
            wallIndex = CellToWalls_Glob[currentCellIndex][direction]
            if ( wallIndex != NO_CELL ) {
                // Find the cell on the other side of this wall
                firstCellIndex := WallToCells_Glob[wallIndex][FIRST_CELL]
                secondCellIndex := WallToCells_Glob[wallIndex][SECOND_CELL]
                
                if ( firstCellIndex == currentCellIndex ) {
                    nextCellIndex = secondCellIndex
                } else {
                    nextCellIndex = firstCellIndex
                }
                
                // Check if neighbor is unvisited
                if ( CellVisited_Glob[nextCellIndex] == false ) {
                    foundNeighbor = true
                    break
                }
            }
        }

        if ( foundNeighbor == true ) {
            // Find wall between current and next cell using lookup tables
            wallIndex = NO_CELL
            for direction := 0; ( direction < 4 ); direction = direction + 1 {
                wallCandidate := CellToWalls_Glob[currentCellIndex][direction]
                if ( wallCandidate != NO_CELL ) {
                    firstCellIndex := WallToCells_Glob[wallCandidate][FIRST_CELL]
                    secondCellIndex := WallToCells_Glob[wallCandidate][SECOND_CELL]
                    if ( ( firstCellIndex == currentCellIndex && secondCellIndex == nextCellIndex ) || 
                         ( firstCellIndex == nextCellIndex && secondCellIndex == currentCellIndex ) ) {
                        wallIndex = wallCandidate
                        break
                    }
                }
            }
            WallsUp_Glob[wallIndex] = false

            // Mark next cell as visited
            CellVisited_Glob[nextCellIndex] = true

            // Push next cell onto stack
            cellStack[stackSize] = nextCellIndex
            stackSize = stackSize + 1

            currentCellIndex = nextCellIndex

            sleepHalfSecond ()
            printMaze ()
        } else {
            // Backtrack - pop from stack
            stackSize = stackSize - 1
            if ( stackSize > 0 ) {
                currentCellIndex = cellStack[stackSize - 1]
            }
        }
    }
}



// Binary Tree maze generation algorithm.
// For each cell, randomly choose to either remove the wall 
// to the north or the wall to the east (if they exist).
// This creates a maze with a distinctive bias.
func buildMazeBinaryTree () {
    for cellRow := 0; ( cellRow < MazeRows_Glob ); cellRow = cellRow + 1 {
        for cellCol := 0; ( cellCol < MazeColumns_Glob ); cellCol = cellCol + 1 {
            currentCellIndex := cellRow * MazeColumns_Glob + cellCol
            validWalls := make ( []int, 2 )
            validWallCount := 0

            // Check if we can go north (UP) - only if not in top row
            if ( cellRow > 0 ) {
                wallIndex := CellToWalls_Glob[currentCellIndex][UP]
                if ( wallIndex != NO_CELL ) {
                    validWalls[validWallCount] = wallIndex
                    validWallCount = validWallCount + 1
                }
            }

            // Check if we can go east (RIGHT) - only if not in rightmost column
            if ( cellCol < MazeColumns_Glob - 1 ) {
                wallIndex := CellToWalls_Glob[currentCellIndex][RIGHT]
                if ( wallIndex != NO_CELL ) {
                    validWalls[validWallCount] = wallIndex
                    validWallCount = validWallCount + 1
                }
            }

            // If we have at least one valid wall, pick one randomly and remove it
            if ( validWallCount > 0 ) {
                randomWallIndex := rand.Intn ( validWallCount )
                wallToRemove := validWalls[randomWallIndex]
                WallsUp_Glob[wallToRemove] = false
            }

            sleepHalfSecond ()
            printMaze ()
        }
    }
}



// Recursive Division maze generation algorithm.
// Start with an empty area (all walls down), then recursively
// divide the area with walls, leaving random gaps.
func buildMazeRecursiveDivision () {
    // Start with all interior walls down
    for wallIndex := 0; ( wallIndex < InteriorWallCount_Glob ); wallIndex = wallIndex + 1 {
        WallsUp_Glob[wallIndex] = false
    }

    printMaze ()
    sleepHalfSecond ()

    // Recursively divide the entire maze area
    divideArea ( 0, MazeRows_Glob - 1, 0, MazeColumns_Glob - 1 )
}



// Note we could use command line arguments instead of user 
// prompts to get the maze size and algorithm choice.
func main () {
    // Seed random number generator
    rand.Seed ( time.Now ().UnixNano () )

    // Prompt the user for maze size
    MazeColumns_Glob = 0
    for ( MazeColumns_Glob <= 0 ) {
        fmt.Print ( "Please enter number of columns for maze, must be greater than 1: " )
        scanner := bufio.NewScanner ( os.Stdin )
        if scanner.Scan () {
            userInput := strings.TrimSpace ( scanner.Text () )
            
            // Check if string is a valid number
            isValid := true
            if ( len ( userInput ) == 0 ) {
                isValid = false
            } else {
                for characterIndex := 0; ( characterIndex < len ( userInput ) ); characterIndex = characterIndex + 1 {
                    character := userInput[characterIndex]
                    if ( character < '0' || character > '9' ) {
                        isValid = false
                        break
                    }
                }
            }
            
            if ( isValid == true ) {
                if value, err := strconv.Atoi ( userInput ); err == nil {
                    MazeColumns_Glob = value
                }
            }
        }
    }

    MazeRows_Glob = 0
    for ( MazeRows_Glob <= 0 ) {
        fmt.Print ( "Please enter number of rows for maze, must be greater than 1: " )
        scanner := bufio.NewScanner ( os.Stdin )
        if scanner.Scan () {
            userInput := strings.TrimSpace ( scanner.Text () )
            
            // Check if string is a valid number
            isValid := true
            if ( len ( userInput ) == 0 ) {
                isValid = false
            } else {
                for characterIndex := 0; ( characterIndex < len ( userInput ) ); characterIndex = characterIndex + 1 {
                    character := userInput[characterIndex]
                    if ( character < '0' || character > '9' ) {
                        isValid = false
                        break
                    }
                }
            }
            
            if ( isValid == true ) {
                if value, err := strconv.Atoi ( userInput ); err == nil {
                    MazeRows_Glob = value
                }
            }
        }
    }

    // Prompt the user for algorithm choice
    algorithmChoice := 0
    for ( algorithmChoice < 1 || algorithmChoice > 5 ) {
        fmt.Println ( "Please choose maze generation algorithm:" )
        fmt.Println ( "1 - Kruskal's Algorithm" )
        fmt.Println ( "2 - Prim's Algorithm" )
        fmt.Println ( "3 - Depth-First Search" )
        fmt.Println ( "4 - Binary Tree Algorithm" )
        fmt.Println ( "5 - Recursive Division Algorithm" )
        
        scanner := bufio.NewScanner ( os.Stdin )
        if scanner.Scan () {
            userInput := strings.TrimSpace ( scanner.Text () )
            
            // Check if string is a valid number
            isValid := true
            if ( len ( userInput ) == 0 ) {
                isValid = false
            } else {
                for characterIndex := 0; ( characterIndex < len ( userInput ) ); characterIndex = characterIndex + 1 {
                    character := userInput[characterIndex]
                    if ( character < '0' || character > '9' ) {
                        isValid = false
                        break
                    }
                }
            }
            
            if ( isValid == true ) {
                if value, err := strconv.Atoi ( userInput ); err == nil {
                    algorithmChoice = value
                }
            }
        }
    }

    // Setup maze datastructures for the user entered size.
    AllHorizontalWallsUp_Glob = make ( []bool, MazeColumns_Glob )
    for columnIndex := 0; ( columnIndex < MazeColumns_Glob ); columnIndex = columnIndex + 1 {
        AllHorizontalWallsUp_Glob[columnIndex] = true
    }
    
    InteriorWallCount_Glob = MazeRows_Glob * ( MazeColumns_Glob - 1 ) + ( MazeRows_Glob - 1 ) * MazeColumns_Glob
    
    WallsUp_Glob = make ( []bool, InteriorWallCount_Glob )
    for wallIndex := 0; ( wallIndex < InteriorWallCount_Glob ); wallIndex = wallIndex + 1 {
        WallsUp_Glob[wallIndex] = true
    }

    // Initialize algorithm state globals
    CellVisited_Glob = make ( []bool, MazeRows_Glob * MazeColumns_Glob )
    for cellIndex := 0; ( cellIndex < MazeRows_Glob * MazeColumns_Glob ); cellIndex = cellIndex + 1 {
        CellVisited_Glob[cellIndex] = false
    }
    
    FrontierWalls_Glob = make ( []int, InteriorWallCount_Glob )
    
    CellInMaze_Glob = make ( []bool, MazeRows_Glob * MazeColumns_Glob )
    for cellIndex := 0; ( cellIndex < MazeRows_Glob * MazeColumns_Glob ); cellIndex = cellIndex + 1 {
        CellInMaze_Glob[cellIndex] = false
    }

    // Initialize lookup tables based on chosen algorithm
    initializeLookupTables ( algorithmChoice )

    // Execute the chosen algorithm
    if ( algorithmChoice == KRUSKAL_ALGORITHM ) {
        buildMazeKruskal ()
    } else if ( algorithmChoice == PRIM_ALGORITHM ) {
        buildMazePrim ()
    } else if ( algorithmChoice == DEPTH_FIRST_ALGORITHM ) {
        buildMazeDepthFirst ()
    } else if ( algorithmChoice == BINARY_TREE_ALGORITHM ) {
        buildMazeBinaryTree ()
    } else if ( algorithmChoice == RECURSIVE_DIVISION_ALGORITHM ) {
        buildMazeRecursiveDivision ()
    }

    fmt.Println ( "Press Enter to exit..." )
    scanner := bufio.NewScanner ( os.Stdin )
    scanner.Scan ()
}
