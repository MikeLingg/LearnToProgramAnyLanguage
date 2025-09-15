#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <time.h>

#ifdef _WIN32
#include <windows.h>
#define sleep_ms(ms) Sleep(ms)
#else
#include <unistd.h>
#define sleep_ms(ms) usleep((ms) * 1000)
#endif

#define FIRST_CELL 0
#define SECOND_CELL 1
#define NO_CELL -1

int main ( )
{
    // Prompt the user for maze size
    int mazeColumns = 0;
    while ( mazeColumns <= 0 )
    {
        printf ( "Please enter number of columns for maze, must be greater than 1: " );
        char userInput[100];
        if ( fgets ( userInput, sizeof ( userInput ), stdin ) != NULL )
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
        if ( fgets ( userInput, sizeof ( userInput ), stdin ) != NULL )
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

    // Start with all the walls up
    bool wallsUp[interiorWallCount];
    for ( int i = 0; i < interiorWallCount; i++ )
    {
        wallsUp[i] = true;
    }

    // Identify the cells each wall connects
    int wallConnections[interiorWallCount][2];

    int wallIndex = 0;
    for ( int rowIndex = 0; rowIndex < mazeRows; rowIndex++ )
    {
        // Track the first cell in the current row
        int firstCellInRow = rowIndex * mazeColumns;

        // Vertical walls
        for ( int verticalWallIndex = 0; verticalWallIndex < mazeColumns - 1; verticalWallIndex++ )
        {
            int leftCell = firstCellInRow + verticalWallIndex;
            int rightCell = leftCell + 1;
            wallConnections[wallIndex][FIRST_CELL] = leftCell;
            wallConnections[wallIndex][SECOND_CELL] = rightCell;
            wallIndex++;
        }

        // Horizontal walls
        if ( wallIndex < interiorWallCount )
        {
            for ( int horizontalWallIndex = 0; horizontalWallIndex < mazeColumns; horizontalWallIndex++ )
            {
                int upperCell = firstCellInRow + horizontalWallIndex;
                int lowerCell = upperCell + mazeColumns;
                wallConnections[wallIndex][FIRST_CELL] = upperCell;
                wallConnections[wallIndex][SECOND_CELL] = lowerCell;
                wallIndex++;
            }
        }
    }

    // Identify which group each cell is a part of
    int cellToGroup[mazeRows * mazeColumns];

    for ( int cellIndex = 0; cellIndex < mazeRows * mazeColumns; cellIndex++ )
    {
        cellToGroup[cellIndex] = cellIndex;
    }

    // Identify which cells are a part of each group
    int groupCells[mazeColumns * mazeRows][mazeColumns * mazeRows];
    
    for ( int cellIndex = 0; cellIndex < mazeRows * mazeColumns; cellIndex++ )
    {
        for ( int innerCellIndex = 0; innerCellIndex < mazeRows * mazeColumns; innerCellIndex++ )
        {
            if ( innerCellIndex == 0 )
            {
                groupCells[cellIndex][innerCellIndex] = cellIndex;
            }
            else
            {
                groupCells[cellIndex][innerCellIndex] = NO_CELL;
            }
        }
    }

    // Print initial maze
    int currentInteriorWall = 0;

    // Print top border
    printf ( "+" );
    for ( int cellIndex = 0; cellIndex < mazeColumns; cellIndex++ )
    {
        printf ( "-+" );
    }
    printf ( "\n" );

    for ( int rowIndex = 0; rowIndex < mazeRows; rowIndex++ )
    {
        // Print vertical walls and cells
        printf ( "|" );
        for ( int columnIndex = 0; columnIndex < mazeColumns; columnIndex++ )
        {
            printf ( " " );

            if ( columnIndex == mazeColumns - 1 || wallsUp[currentInteriorWall] )
            {
                printf ( "|" );
            }
            else
            {
                printf ( " " );
            }
            if ( columnIndex < mazeColumns - 1 )
            {
                currentInteriorWall++;
            }
        }
        printf ( "\n" );

        // Print horizontal walls
        if ( rowIndex < mazeRows - 1 )
        {
            printf ( "+" );
            for ( int columnIndex = 0; columnIndex < mazeColumns; columnIndex++ )
            {
                if ( wallsUp[currentInteriorWall] )
                {
                    printf ( "-" );
                }
                else
                {
                    printf ( " " );
                }
                printf ( "+" );
                currentInteriorWall++;
            }
            printf ( "\n" );
        }
    }

    // Print bottom border
    printf ( "+" );
    for ( int columnIndex = 0; columnIndex < mazeColumns; columnIndex++ )
    {
        printf ( "-+" );
    }
    printf ( "\n" );

    int wallRemoveList[interiorWallCount];
    for ( int wallIndex = 0; wallIndex < interiorWallCount; wallIndex++ )
    {
        wallRemoveList[wallIndex] = wallIndex;
    }

    srand ( time ( NULL ) );

    // Fisher-Yates shuffle algorithm
    for ( int shuffleIndex = interiorWallCount - 1; shuffleIndex > 0; shuffleIndex-- )
    {
        int otherIndex = rand ( ) % ( shuffleIndex + 1 );
        int temp = wallRemoveList[shuffleIndex];
        wallRemoveList[shuffleIndex] = wallRemoveList[otherIndex];
        wallRemoveList[otherIndex] = temp;
    }

    bool mazeComplete = false;

    // Kruskal's algorithm
    for ( int removeWallIndex = 0; removeWallIndex < interiorWallCount; removeWallIndex++ )
    {
        int nextWallToCheck = wallRemoveList[removeWallIndex];

        int firstCell = wallConnections[nextWallToCheck][FIRST_CELL];
        int firstCellGroupIndex = cellToGroup[firstCell];
        int secondCell = wallConnections[nextWallToCheck][SECOND_CELL];
        int secondCellGroupIndex = cellToGroup[secondCell];
        
        if ( firstCellGroupIndex != secondCellGroupIndex )
        {
            wallsUp[nextWallToCheck] = false;

            // Find next empty position in first group
            int nextEmptyFirstGroupIndex = 0;
            for ( int cellIndex = 0; cellIndex < mazeColumns * mazeRows; cellIndex++ )
            {
                if ( groupCells[firstCellGroupIndex][cellIndex] == NO_CELL )
                {
                    nextEmptyFirstGroupIndex = cellIndex;
                    break;
                }
            }

            // Move all cells from second group to first group
            for ( int groupCellIndex = mazeColumns * mazeRows - 1; groupCellIndex >= 0; groupCellIndex-- )
            {
                if ( groupCells[secondCellGroupIndex][groupCellIndex] != NO_CELL )
                {
                    int cellToMove = groupCells[secondCellGroupIndex][groupCellIndex];

                    groupCells[firstCellGroupIndex][nextEmptyFirstGroupIndex] = cellToMove;
                    nextEmptyFirstGroupIndex++;
                    cellToGroup[cellToMove] = firstCellGroupIndex;
                    groupCells[secondCellGroupIndex][groupCellIndex] = NO_CELL;
                    
                    if ( nextEmptyFirstGroupIndex >= mazeColumns * mazeRows )
                    {
                        mazeComplete = true;
                    }
                }
            }

            sleep_ms ( 500 );
            
            // Copy in Print maze code from above again here:
            currentInteriorWall = 0;

            printf ( "\n" );
            printf ( "+" );
            for ( int cellIndex = 0; cellIndex < mazeColumns; cellIndex++ )
            {
                printf ( "-+" );
            }
            printf ( "\n" );

            for ( int rowIndex = 0; rowIndex < mazeRows; rowIndex++ )
            {
                printf ( "|" );
                for ( int columnIndex = 0; columnIndex < mazeColumns; columnIndex++ )
                {
                    printf ( " " );

                    if ( columnIndex == mazeColumns - 1 || wallsUp[currentInteriorWall] )
                    {
                        printf ( "|" );
                    }
                    else
                    {
                        printf ( " " );
                    }
                    if ( columnIndex < mazeColumns - 1 )
                    {
                        currentInteriorWall++;
                    }
                }
                printf ( "\n" );

                if ( rowIndex < mazeRows - 1 )
                {
                    printf ( "+" );
                    for ( int columnIndex = 0; columnIndex < mazeColumns; columnIndex++ )
                    {
                        if ( wallsUp[currentInteriorWall] )
                        {
                            printf ( "-" );
                        }
                        else
                        {
                            printf ( " " );
                        }
                        printf ( "+" );
                        currentInteriorWall++;
                    }
                    printf ( "\n" );
                }
            }

            printf ( "+" );
            for ( int columnIndex = 0; columnIndex < mazeColumns; columnIndex++ )
            {
                printf ( "-+" );
            }
            printf ( "\n" );

            if ( mazeComplete )
            {
                break;
            }
        }
    }

    return 0;
}
