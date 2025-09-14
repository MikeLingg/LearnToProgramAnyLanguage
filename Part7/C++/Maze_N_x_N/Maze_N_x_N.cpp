#include <chrono>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <thread>

int main ( )
{
    // Define directions
    //const int LEFT = 0;
    //const int UP = 1;
    //const int RIGHT = 2;
    //const int DOWN = 3;

    const int FIRST_CELL = 0;
    const int SECOND_CELL = 1;
    
    const int NO_CELL = -1;
    
    // Prompt the user for maze size
    int mazeColumns = 0;
    while ( mazeColumns <= 0 )
    {
        printf ( "Please enter number of columns for maze, must be greater than 1: " );
        char userInput[100];
        if ( fgets ( userInput, sizeof ( userInput ), stdin ) != nullptr )
        {
            // Remove newline if present
            userInput[strcspn ( userInput, "\n" )] = '\0';
            
            // Check if string contains only digits
            bool isValid = true;
            if ( strlen ( userInput ) == 0 )
            {
                isValid = false;
            }
            else
            {
                for ( int i = 0; userInput[i] != '\0'; i++ )
                {
                    if ( userInput[i] < '0' || userInput[i] > '9' )
                    {
                        isValid = false;
                        break;
                    }
                }
            }
            
            if ( isValid )
            {
                mazeColumns = atoi ( userInput );
            }
        }
    }

    int mazeRows = 0;
    while ( mazeRows <= 0 )
    {
        printf ( "Please enter number of rows for maze, must be greater than 1: " );
        char userInput[100];
        if ( fgets ( userInput, sizeof ( userInput ), stdin ) != nullptr )
        {
            // Remove newline if present
            userInput[strcspn ( userInput, "\n" )] = '\0';
            
            // Check if string contains only digits
            bool isValid = true;
            if ( strlen ( userInput ) == 0 )
            {
                isValid = false;
            }
            else
            {
                for ( int i = 0; userInput[i] != '\0'; i++ )
                {
                    if ( userInput[i] < '0' || userInput[i] > '9' )
                    {
                        isValid = false;
                        break;
                    }
                }
            }
            
            if ( isValid )
            {
                mazeRows = atoi ( userInput );
            }
        }
    }

    int interiorWallCount = mazeRows * ( mazeColumns - 1 ) + ( mazeRows - 1 ) * mazeColumns;

    // Start with all the walls up.
    bool wallsUp[interiorWallCount];
    for ( int i = 0; i < interiorWallCount; i++ )
    {
        wallsUp[i] = true;
    }

    // Identify the cells each wall connects.
    int wallConnections[interiorWallCount][2];

    int wallIndex = 0;
    for ( int rowIndex = 0; rowIndex <= mazeRows - 1; rowIndex++ )
    {
        // Track the first cell in the current row
        int firstCellInRow = rowIndex * mazeColumns;

        // Note the 0..mazeColumns - 2, one less vertical wall
        // than the number of columns.
        for ( int verticalWallIndex = 0; verticalWallIndex <= mazeColumns - 2; verticalWallIndex++ )
        {
            int leftCell = firstCellInRow + verticalWallIndex;
            int rightCell = leftCell + 1;
            wallConnections[wallIndex][FIRST_CELL] = leftCell;
            wallConnections[wallIndex][SECOND_CELL] = rightCell;
            wallIndex = wallIndex + 1;
        }

        // The last row will have no interior horizontal walls below
        // it, so will be skipped.
        if ( wallIndex < interiorWallCount )
        {
            for ( int horizontalWallIndex = 0; horizontalWallIndex <= mazeColumns - 1; horizontalWallIndex++ )
            {
                int upperCell = firstCellInRow + horizontalWallIndex;
                int lowerCell = upperCell + mazeColumns;
                wallConnections[wallIndex][FIRST_CELL] = upperCell;
                wallConnections[wallIndex][SECOND_CELL] = lowerCell;
                wallIndex = wallIndex + 1;
            }
        }
    }

    // Identify which group each cell is a part of.
    int cellToGroup[mazeRows * mazeColumns];

    for ( int cellIndex = 0; cellIndex <= mazeRows * mazeColumns - 1; cellIndex++ )
    {
        cellToGroup[cellIndex] = cellIndex;
    }

    // Identify which cells are a part of each group
    // Note each array is large enough to fit all cells,
    // NO_CELL indicates no cell is assigned to this index.
    int groupCells[mazeColumns * mazeRows][mazeColumns * mazeRows];
    
    for ( int cellIndex = 0; cellIndex <= mazeRows * mazeColumns - 1; cellIndex++ )
    {
        groupCells[cellIndex][0] = cellIndex;
    }

    // Print maze code:
    // Print out the maze, this is a less painful copy/paste job without functions, but better with loops.
    int currentInteriorWall = 0;

    // Print the horizontal walls above row 1 - All are exterior walls, no conditions.
    // +-+-+-+
    // One initial cell with + followed by mazeColumns cells with -+
    printf ( "+" );
    for ( int cellIndex = 0; cellIndex <= mazeColumns - 1; cellIndex++ )
    {
        printf ( "-" );
        printf ( "+" );
    }
    printf ( "\n" );

    for ( int rowIndex = 0; rowIndex <= mazeRows - 1; rowIndex++ )
    {
        // Vertical walls and cells in each row row.
        // The left and right vertical walls are exterior, always up.
        // | | | |
        // Or print one |, followed by mazeColumn cells of <space>| where the |
        // may be down (<space>).
        printf ( "|" );
        for ( int columnIndex = 0; columnIndex <= mazeColumns - 1; columnIndex++ )
        {
            printf ( " " );

            // Always print the right most vertical wall,
            // if interior wall, print if the wall is up.
            if ( columnIndex == mazeColumns - 1 || wallsUp[currentInteriorWall] == true )
            {
                printf ( "|" );
            }
            else
            {
                printf ( " " );
            }
            if ( columnIndex < mazeColumns - 1 )
            {
                currentInteriorWall = currentInteriorWall + 1;
            }
        }
        printf ( "\n" );

        // One fewer horizontal wall than vertical
        if ( rowIndex < mazeRows - 1 )
        {
            // Horizontal walls above row rowIndex
            // +-+-+-+
            printf ( "+" );
            for ( int columnIndex = 0; columnIndex <= mazeColumns - 1; columnIndex++ )
            {
                if ( wallsUp[currentInteriorWall] == true )
                {
                    printf ( "-" );
                }
                else
                {
                    printf ( " " );
                }
                printf ( "+" );
                currentInteriorWall = currentInteriorWall + 1;
            }
            printf ( "\n" );
        }
    }

    // Horizontal walls below row the final row - All are exterior walls, no conditions.
    // +-+-+-+
    printf ( "+" );
    for ( int columnIndex = 0; columnIndex <= mazeColumns - 1; columnIndex++ )
    {
        printf ( "-" );
        printf ( "+" );
    }
    printf ( "\n" );

    int wallRemoveList[interiorWallCount];
    for ( int wallIndex = 0; wallIndex <= interiorWallCount - 1; wallIndex++ )
    {
        wallRemoveList[wallIndex] = wallIndex;
    }

    srand ( time ( NULL ) );

    // Fisher-Yates shuffle algorithm
    for ( int shuffleIndex = interiorWallCount - 1; shuffleIndex > 0; shuffleIndex = shuffleIndex - 1 )
    {
        // Generate random index from 0 to shuffleIndex (inclusive)
        int otherIndex = rand ( ) % ( shuffleIndex + 1 );

        // Swap wallRemoveList[shuffleIndex] with wallRemoveList[otherIndex]
        int temp = wallRemoveList[shuffleIndex];
        wallRemoveList[shuffleIndex] = wallRemoveList[otherIndex];
        wallRemoveList[otherIndex] = temp;
    }

    bool mazeComplete = false;

    // Remove wall code:
    // Now that we have loops we can implement Kruskal's algorithm.
    // The simple description of the algorithm is first place each 
    // cell in its own group.  Then process all walls in random order,
    // if the cells on either side of the wall are in separate groups, 
    // remove the wall and merge the groups.  Repeat until all 
    // cells are now in the same group.
    for ( int removeWallIndex = 0; removeWallIndex <= interiorWallCount - 1; removeWallIndex++ )
    {
        int nextWallToCheck = wallRemoveList[removeWallIndex];

        // If the two cells connected to this wall are not part 
        // of the same group, remove the wall and merge the 
        // groups.
        int firstCell = wallConnections[nextWallToCheck][FIRST_CELL];
        int firstCellGroupIndex = cellToGroup[firstCell];
        int secondCell = wallConnections[nextWallToCheck][SECOND_CELL];
        int secondCellGroupIndex = cellToGroup[secondCell];
        
        if ( firstCellGroupIndex != secondCellGroupIndex )
        {
            wallsUp[nextWallToCheck] = false;

            // Loop through the indices of all cells in the first 
            // group until we find a NO_CELL indicating no cell here.
            int nextEmptyFirstGroupIndex = 0;
            for ( int cellIndex = 0; cellIndex <= mazeColumns * mazeRows - 1; cellIndex++ )
            {
                if ( groupCells[firstCellGroupIndex][cellIndex] == NO_CELL )
                {
                    nextEmptyFirstGroupIndex = cellIndex;
                    break;
                }
            }

            // Loop through the indices of all cells in the second group,
            // move each cell to the first group, and set that cell's 
            // group to the first group index.
            for ( int groupCellIndex = mazeColumns * mazeRows - 1; groupCellIndex >= 0; groupCellIndex-- )
            {
                // Skip until we reach valid cells
                if ( groupCells[secondCellGroupIndex][groupCellIndex] != NO_CELL )
                {
                    // Get the id number of the cell to move from 
                    // the second group to the first group
                    int cellToMove = groupCells[secondCellGroupIndex][groupCellIndex];

                    // Move the cell number from the second group 
                    // to the first group
                    groupCells[firstCellGroupIndex][nextEmptyFirstGroupIndex] = cellToMove;
                    // Move our empty index to the next cell in this array.
                    nextEmptyFirstGroupIndex = nextEmptyFirstGroupIndex + 1;
                    // Mark this cell as part of the first group.
                    cellToGroup[cellToMove] = firstCellGroupIndex;
                    // Remove the cell from the second group (set the
                    // array entry to NO_CELL)
                    groupCells[secondCellGroupIndex][groupCellIndex] = NO_CELL;
                    
                    if ( nextEmptyFirstGroupIndex >= mazeColumns * mazeRows )
                    {
                        mazeComplete = true;
                    }
                }
            }

            std::this_thread::sleep_for ( std::chrono::milliseconds ( 500 ) );
                    
            // Copy in Print maze code from above again here:
            currentInteriorWall = 0;

            printf ( "\n" );
            printf ( "+" );
            for ( int cellIndex = 0; cellIndex <= mazeColumns - 1; cellIndex++ )
            {
                printf ( "-" );
                printf ( "+" );
            }
            printf ( "\n" );

            for ( int rowIndex = 0; rowIndex <= mazeRows - 1; rowIndex++ )
            {
                printf ( "|" );
                for ( int columnIndex = 0; columnIndex <= mazeColumns - 1; columnIndex++ )
                {
                    printf ( " " );

                    if ( columnIndex == mazeColumns - 1 || wallsUp[currentInteriorWall] == true )
                    {
                        printf ( "|" );
                    }
                    else
                    {
                        printf ( " " );
                    }
                    if ( columnIndex < mazeColumns - 1 )
                    {
                        currentInteriorWall = currentInteriorWall + 1;
                    }
                }
                printf ( "\n" );

                if ( rowIndex < mazeRows - 1 )
                {
                    printf ( "+" );
                    for ( int columnIndex = 0; columnIndex <= mazeColumns - 1; columnIndex++ )
                    {
                        if ( wallsUp[currentInteriorWall] == true )
                        {
                            printf ( "-" );
                        }
                        else
                        {
                            printf ( " " );
                        }
                        printf ( "+" );
                        currentInteriorWall = currentInteriorWall + 1;
                    }
                    printf ( "\n" );
                }
            }

            printf ( "+" );
            for ( int columnIndex = 0; columnIndex <= mazeColumns - 1; columnIndex++ )
            {
                printf ( "-" );
                printf ( "+" );
            }
            printf ( "\n" );

            if ( mazeComplete == true )
            {
                break;
            }
        }
    }

    return 0;
}
