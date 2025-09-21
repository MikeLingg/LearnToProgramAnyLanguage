package main

import (
    "bufio"
    "fmt"
    "os"
    "strconv"
    "strings"
    "time"
)

func main ( ) {
    const FIRST_CELL = 0
    const SECOND_CELL = 1
    const NO_CELL = -1
    
    scanner := bufio.NewScanner ( os.Stdin )
    
    // Prompt the user for maze size
    var mazeColumns int
    for ( mazeColumns <= 0 ) {
        fmt.Print ( "Please enter number of columns for maze, must be greater than 1: " )
        scanner.Scan ( )
        userInput := strings.TrimSpace ( scanner.Text ( ) )
        
        if num, err := strconv.Atoi ( userInput ); err == nil {
            mazeColumns = num
        }
    }
    
    var mazeRows int
    for ( mazeRows <= 0 ) {
        fmt.Print ( "Please enter number of rows for maze, must be greater than 1: " )
        scanner.Scan ( )
        userInput := strings.TrimSpace ( scanner.Text ( ) )
        
        if num, err := strconv.Atoi ( userInput ); err == nil {
            mazeRows = num
        }
    }
    
    interiorWallCount := mazeRows * ( mazeColumns - 1 ) + ( mazeRows - 1 ) * mazeColumns
    
    // Start with all the walls up
    wallsUp := make ( []bool, interiorWallCount )
    for i := range wallsUp {
        wallsUp[i] = true
    }
    
    // Identify the cells each wall connects
    wallConnections := make ( [][2]int, interiorWallCount )
    
    wallIndex := 0
    for rowIndex := 0; rowIndex < mazeRows; rowIndex++ {
        firstCellInRow := rowIndex * mazeColumns
        
        // Vertical walls
        for verticalWallIndex := 0; verticalWallIndex < mazeColumns - 1; verticalWallIndex++ {
            leftCell := firstCellInRow + verticalWallIndex
            rightCell := leftCell + 1
            wallConnections[wallIndex][FIRST_CELL] = leftCell
            wallConnections[wallIndex][SECOND_CELL] = rightCell
            wallIndex++
        }
        
        // Horizontal walls
        if ( wallIndex < interiorWallCount ) {
            for horizontalWallIndex := 0; horizontalWallIndex < mazeColumns; horizontalWallIndex++ {
                upperCell := firstCellInRow + horizontalWallIndex
                lowerCell := upperCell + mazeColumns
                wallConnections[wallIndex][FIRST_CELL] = upperCell
                wallConnections[wallIndex][SECOND_CELL] = lowerCell
                wallIndex++
            }
        }
    }
    
    // Identify which group each cell is a part of
    cellToGroup := make ( []int, mazeRows * mazeColumns )
    for i := range cellToGroup {
        cellToGroup[i] = i
    }
    
    // Identify which cells are a part of each group
    groupCells := make ( [][]int, mazeColumns * mazeRows )
    for i := range groupCells {
        groupCells[i] = make ( []int, mazeColumns * mazeRows )
        for j := range groupCells[i] {
            if ( j == 0 ) {
                groupCells[i][j] = i
            } else {
                groupCells[i][j] = NO_CELL
            }
        }
    }
    
    // Print initial maze
    currentInteriorWall := 0
    
    // Print top border
    fmt.Print ( "+" )
    for i := 0; i < mazeColumns; i++ {
        fmt.Print ( "-+" )
    }
    fmt.Println ( )
    
    for rowIndex := 0; rowIndex < mazeRows; rowIndex++ {
        // Print vertical walls and cells
        fmt.Print ( "|" )
        for columnIndex := 0; columnIndex < mazeColumns; columnIndex++ {
            fmt.Print ( " " )
            
            if ( columnIndex == mazeColumns - 1 || wallsUp[currentInteriorWall] ) {
                fmt.Print ( "|" )
            } else {
                fmt.Print ( " " )
            }
            
            if ( columnIndex < mazeColumns - 1 ) {
                currentInteriorWall++
            }
        }
        fmt.Println ( )
        
        // Print horizontal walls
        if ( rowIndex < mazeRows - 1 ) {
            fmt.Print ( "+" )
            for columnIndex := 0; columnIndex < mazeColumns; columnIndex++ {
                if ( wallsUp[currentInteriorWall] ) {
                    fmt.Print ( "-" )
                } else {
                    fmt.Print ( " " )
                }
                fmt.Print ( "+" )
                currentInteriorWall++
            }
            fmt.Println ( )
        }
    }
    
    // Print bottom border
    fmt.Print ( "+" )
    for i := 0; i < mazeColumns; i++ {
        fmt.Print ( "-+" )
    }
    fmt.Println ( )
    
    // Create wall removal list
    wallRemoveList := make ( []int, interiorWallCount )
    for i := 0; i < interiorWallCount; i++ {
        wallRemoveList[i] = i
    }
    
    // Seed the random number generator
    seed := time.Now ( ).UnixNano ( )
    
    // Fisher-Yates shuffle algorithm
    for shuffleIndex := interiorWallCount - 1; shuffleIndex > 0; shuffleIndex-- {
        // Simple LCG for random number generation
        seed = seed * 1664525 + 1013904223
        otherIndex := int ( seed ) % ( shuffleIndex + 1 )
        if ( otherIndex < 0 ) {
            otherIndex = -otherIndex
        }
        
        temp := wallRemoveList[shuffleIndex]
        wallRemoveList[shuffleIndex] = wallRemoveList[otherIndex]
        wallRemoveList[otherIndex] = temp
    }
    
    mazeComplete := false
    
    // Kruskal's algorithm
    for removeWallIndex := 0; removeWallIndex < interiorWallCount; removeWallIndex++ {
        nextWallToCheck := wallRemoveList[removeWallIndex]
        
        firstCell := wallConnections[nextWallToCheck][FIRST_CELL]
        firstCellGroupIndex := cellToGroup[firstCell]
        secondCell := wallConnections[nextWallToCheck][SECOND_CELL]
        secondCellGroupIndex := cellToGroup[secondCell]
        
        if ( firstCellGroupIndex != secondCellGroupIndex ) {
            wallsUp[nextWallToCheck] = false
            
            // Find next empty position in first group
            nextEmptyFirstGroupIndex := 0
            for cellIndex := 0; cellIndex < mazeColumns * mazeRows; cellIndex++ {
                if ( groupCells[firstCellGroupIndex][cellIndex] == NO_CELL ) {
                    nextEmptyFirstGroupIndex = cellIndex
                    break
                }
            }
            
            // Move all cells from second group to first group
            for groupCellIndex := mazeColumns * mazeRows - 1; groupCellIndex >= 0; groupCellIndex-- {
                if ( groupCells[secondCellGroupIndex][groupCellIndex] != NO_CELL ) {
                    cellToMove := groupCells[secondCellGroupIndex][groupCellIndex]
                    
                    groupCells[firstCellGroupIndex][nextEmptyFirstGroupIndex] = cellToMove
                    nextEmptyFirstGroupIndex++
                    cellToGroup[cellToMove] = firstCellGroupIndex
                    groupCells[secondCellGroupIndex][groupCellIndex] = NO_CELL
                    
                    if ( nextEmptyFirstGroupIndex >= mazeColumns * mazeRows ) {
                        mazeComplete = true
                    }
                }
            }
            
            time.Sleep ( 500 * time.Millisecond )
            
            // Copy in Print maze code from above again here:
            currentInteriorWall = 0
            
            fmt.Println ( )
            fmt.Print ( "+" )
            for i := 0; i < mazeColumns; i++ {
                fmt.Print ( "-+" )
            }
            fmt.Println ( )
            
            for rowIndex := 0; rowIndex < mazeRows; rowIndex++ {
                fmt.Print ( "|" )
                for columnIndex := 0; columnIndex < mazeColumns; columnIndex++ {
                    fmt.Print ( " " )
                    
                    if ( columnIndex == mazeColumns - 1 || wallsUp[currentInteriorWall] ) {
                        fmt.Print ( "|" )
                    } else {
                        fmt.Print ( " " )
                    }
                    
                    if ( columnIndex < mazeColumns - 1 ) {
                        currentInteriorWall++
                    }
                }
                fmt.Println ( )
                
                if ( rowIndex < mazeRows - 1 ) {
                    fmt.Print ( "+" )
                    for columnIndex := 0; columnIndex < mazeColumns; columnIndex++ {
                        if ( wallsUp[currentInteriorWall] ) {
                            fmt.Print ( "-" )
                        } else {
                            fmt.Print ( " " )
                        }
                        fmt.Print ( "+" )
                        currentInteriorWall++
                    }
                    fmt.Println ( )
                }
            }
            
            fmt.Print ( "+" )
            for i := 0; i < mazeColumns; i++ {
                fmt.Print ( "-+" )
            }
            fmt.Println ( )
            
            if ( mazeComplete ) {
                break
            }
        }
    }
}