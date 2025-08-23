#include <stdio.h>
#include <stdbool.h>
#include <unistd.h>

int main()
{
    // Initialization code:
    // Cell indexes:
    // +-+-+-+
    // |0|1|2|
    // +-+-+-+
    // |3|4|5|
    // +-+-+-+
    // |6|7|8|
    // +-+-+-+
    
    // Interior wall indexes:
    // +-+-+-+
    // | 0 1 |
    // +2+3+4+
    // | 5 6 |
    // +7+8+9+
    // | 10 11 |
    // +-+-+-+
    
    // Define directions
    int LEFT = 0;
    //int UP = 1;
    //int RIGHT = 2;
    //int DOWN = 3;
    
    // For each cell 0-8, indicate if a wall is exterior and cannot be removed (-1) or its interior index for each of
    // the four directions, LEFT, UP, RIGHT, DOWN.
    int cellToWallLUT[9][4] = {
        {-1, -1, 0, 2},   {0, -1, 1, 3},   {1, -1, -1, 4},
        {-1, 2, 5, 7},    {5, 3, 6, 8},    {6, 4, -1, 9},
        {-1, 7, 10, -1},  {10, 8, 11, -1}, {11, 9, -1, -1}
    };
    
    // 12 interior walls in a 3x3 maze. Start with all the walls up.
    bool wallList[12] = {true, true, true, true, true, true, true, true, true, true, true, true};
    
    // Print initial maze
    printf( "Initial Maze:\n" );
    
    // Print out the maze, this is a rather painful copy/paste job without loops and functions.
    // Note printChar is meant to print a single character with no newlines.
    // Horizontal walls above row 1 - All are exterior walls, no conditions.
    int currentInteriorWall = 0;
    
    // +-+-+-+
    printf( "%c", '+' );
    printf( "%c", '-' );
    printf( "%c", '+' );
    printf( "%c", '-' );
    printf( "%c", '+' );
    printf( "%c", '-' );
    printf( "%c", '+' );
    printf( "\n" );
    
    // Vertical walls and cells row 1.
    // The left and right vertical walls are exterior, always up.
    // | | | |
    printf( "%c", '|' );
    printf( "%c", ' ' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '|' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", ' ' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '|' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", ' ' );
    printf( "%c", '|' );
    printf( "\n" );
    
    // Horizontal walls above row 2
    // +-+-+-+
    printf( "%c", '+' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '-' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", '+' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '-' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", '+' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '-' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", '+' );
    printf( "\n" );
    
    // Vertical walls and cells row 2.
    // The left and right vertical walls are exterior, always up.
    // | | | |
    printf( "%c", '|' );
    printf( "%c", ' ' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '|' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", ' ' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '|' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", ' ' );
    printf( "%c", '|' );
    printf( "\n" );
    
    // Horizontal walls above row 3
    // +-+-+-+
    printf( "%c", '+' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '-' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", '+' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '-' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", '+' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '-' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", '+' );
    printf( "\n" );
    
    // Vertical walls and cells row 3.
    // The left and right vertical walls are exterior, always up.
    // | | | |
    printf( "%c", '|' );
    printf( "%c", ' ' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '|' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", ' ' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '|' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", ' ' );
    printf( "%c", '|' );
    printf( "\n" );
    
    // Horizontal walls below row 3 - All are exterior walls, no conditions.
    // +-+-+-+
    printf( "%c", '+' );
    printf( "%c", '-' );
    printf( "%c", '+' );
    printf( "%c", '-' );
    printf( "%c", '+' );
    printf( "%c", '-' );
    printf( "%c", '+' );
    printf( "\n" );
    
    // Now process each cell 0-8
    // Cell 0
    printf( "\nRemoving wall from cell 0:\n" );
    
    // Remove wall code:
    // Remove a cell wall if possible for cell 0
    {
        int cellIndex = 0;
        int wallToRemove;
        bool wallRemoved = false;
        int direction = LEFT;
        
        // If the wall in this direction is NOT an exterior wall
        if ( cellToWallLUT[cellIndex][direction] >= 0 )
        {
            wallToRemove = cellToWallLUT[cellIndex][direction];
            // If this wall has not been already removed
            if ( wallList[wallToRemove] == true )
            {
                // Remove the wall and indicate a wall was successfully removed
                wallList[wallToRemove] = false;
                wallRemoved = true;
            }
        }
        
        // If we haven't removed a wall yet, try to remove one in the next direction
        // Check the guard, have we removed one wall
        if ( wallRemoved == false )
        {
            direction = direction + 1;
            // If the wall in this direction is NOT an exterior wall
            if ( cellToWallLUT[cellIndex][direction] >= 0 )
            {
                wallToRemove = cellToWallLUT[cellIndex][direction];
                // If this wall has not been already removed
                if ( wallList[wallToRemove] == true )
                {
                    // Remove the wall and indicate a wall was successfully removed
                    wallList[wallToRemove] = false;
                    wallRemoved = true;
                }
            }
        }
        
        // If we haven't removed a wall yet, try to remove one in the next direction
        // Check the guard, have we removed one wall
        if ( wallRemoved == false )
        {
            direction = direction + 1;
            // If the wall in this direction is NOT an exterior wall
            if ( cellToWallLUT[cellIndex][direction] >= 0 )
            {
                wallToRemove = cellToWallLUT[cellIndex][direction];
                // If this wall has not been already removed
                if ( wallList[wallToRemove] == true )
                {
                    // Remove the wall and indicate a wall was successfully removed
                    wallList[wallToRemove] = false;
                    wallRemoved = true;
                }
            }
        }
        
        // If we haven't removed a wall yet, try to remove one in the next direction
        // Check the guard, have we removed one wall
        if ( wallRemoved == false )
        {
            direction = direction + 1;
            // If the wall in this direction is NOT an exterior wall
            if ( cellToWallLUT[cellIndex][direction] >= 0 )
            {
                wallToRemove = cellToWallLUT[cellIndex][direction];
                // If this wall has not been already removed
                if ( wallList[wallToRemove] == true )
                {
                    // Remove the wall and indicate a wall was successfully removed
                    wallList[wallToRemove] = false;
                    wallRemoved = true;
                }
            }
        }
    }
    
    // Print maze after cell 0
    usleep( 500000 );
    
    // Print maze (duplicate of above print code)
    currentInteriorWall = 0;
    
    // +-+-+-+
    printf( "%c", '+' );
    printf( "%c", '-' );
    printf( "%c", '+' );
    printf( "%c", '-' );
    printf( "%c", '+' );
    printf( "%c", '-' );
    printf( "%c", '+' );
    printf( "\n" );
    
    // Row 1 vertical walls and cells
    printf( "%c", '|' );
    printf( "%c", ' ' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '|' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", ' ' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '|' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", ' ' );
    printf( "%c", '|' );
    printf( "\n" );
    
    // Row 2 horizontal walls
    printf( "%c", '+' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '-' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", '+' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '-' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", '+' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '-' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", '+' );
    printf( "\n" );
    
    // Row 2 vertical walls and cells
    printf( "%c", '|' );
    printf( "%c", ' ' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '|' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", ' ' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '|' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", ' ' );
    printf( "%c", '|' );
    printf( "\n" );
    
    // Row 3 horizontal walls
    printf( "%c", '+' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '-' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", '+' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '-' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", '+' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '-' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", '+' );
    printf( "\n" );
    
    // Row 3 vertical walls and cells
    printf( "%c", '|' );
    printf( "%c", ' ' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '|' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", ' ' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '|' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", ' ' );
    printf( "%c", '|' );
    printf( "\n" );
    
    // Bottom border
    printf( "%c", '+' );
    printf( "%c", '-' );
    printf( "%c", '+' );
    printf( "%c", '-' );
    printf( "%c", '+' );
    printf( "%c", '-' );
    printf( "%c", '+' );
    printf( "\n" );
    
    // Cell 1
    printf( "\nRemoving wall from cell 1:\n" );
    
    // Remove wall code:
    // Remove a cell wall if possible for cell 1
    {
        int cellIndex = 1;
        int wallToRemove;
        bool wallRemoved = false;
        int direction = LEFT;
        
        // If the wall in this direction is NOT an exterior wall
        if ( cellToWallLUT[cellIndex][direction] >= 0 )
        {
            wallToRemove = cellToWallLUT[cellIndex][direction];
            // If this wall has not been already removed
            if ( wallList[wallToRemove] == true )
            {
                // Remove the wall and indicate a wall was successfully removed
                wallList[wallToRemove] = false;
                wallRemoved = true;
            }
        }
        
        // If we haven't removed a wall yet, try to remove one in the next direction
        // Check the guard, have we removed one wall
        if ( wallRemoved == false )
        {
            direction = direction + 1;
            // If the wall in this direction is NOT an exterior wall
            if ( cellToWallLUT[cellIndex][direction] >= 0 )
            {
                wallToRemove = cellToWallLUT[cellIndex][direction];
                // If this wall has not been already removed
                if ( wallList[wallToRemove] == true )
                {
                    // Remove the wall and indicate a wall was successfully removed
                    wallList[wallToRemove] = false;
                    wallRemoved = true;
                }
            }
        }
        
        // If we haven't removed a wall yet, try to remove one in the next direction
        // Check the guard, have we removed one wall
        if ( wallRemoved == false )
        {
            direction = direction + 1;
            // If the wall in this direction is NOT an exterior wall
            if ( cellToWallLUT[cellIndex][direction] >= 0 )
            {
                wallToRemove = cellToWallLUT[cellIndex][direction];
                // If this wall has not been already removed
                if ( wallList[wallToRemove] == true )
                {
                    // Remove the wall and indicate a wall was successfully removed
                    wallList[wallToRemove] = false;
                    wallRemoved = true;
                }
            }
        }
        
        // If we haven't removed a wall yet, try to remove one in the next direction
        // Check the guard, have we removed one wall
        if ( wallRemoved == false )
        {
            direction = direction + 1;
            // If the wall in this direction is NOT an exterior wall
            if ( cellToWallLUT[cellIndex][direction] >= 0 )
            {
                wallToRemove = cellToWallLUT[cellIndex][direction];
                // If this wall has not been already removed
                if ( wallList[wallToRemove] == true )
                {
                    // Remove the wall and indicate a wall was successfully removed
                    wallList[wallToRemove] = false;
                    wallRemoved = true;
                }
            }
        }
    }
    
    // Print maze after cell 1
    usleep( 500000 );
    
    // Print maze (duplicate of above print code)
    currentInteriorWall = 0;
    
    // +-+-+-+
    printf( "%c", '+' );
    printf( "%c", '-' );
    printf( "%c", '+' );
    printf( "%c", '-' );
    printf( "%c", '+' );
    printf( "%c", '-' );
    printf( "%c", '+' );
    printf( "\n" );
    
    // Row 1 vertical walls and cells
    printf( "%c", '|' );
    printf( "%c", ' ' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '|' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", ' ' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '|' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", ' ' );
    printf( "%c", '|' );
    printf( "\n" );
    
    // Row 2 horizontal walls
    printf( "%c", '+' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '-' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", '+' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '-' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", '+' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '-' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", '+' );
    printf( "\n" );
    
    // Row 2 vertical walls and cells
    printf( "%c", '|' );
    printf( "%c", ' ' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '|' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", ' ' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '|' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", ' ' );
    printf( "%c", '|' );
    printf( "\n" );
    
    // Row 3 horizontal walls
    printf( "%c", '+' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '-' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", '+' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '-' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", '+' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '-' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", '+' );
    printf( "\n" );
    
    // Row 3 vertical walls and cells
    printf( "%c", '|' );
    printf( "%c", ' ' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '|' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", ' ' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '|' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", ' ' );
    printf( "%c", '|' );
    printf( "\n" );
    
    // Bottom border
    printf( "%c", '+' );
    printf( "%c", '-' );
    printf( "%c", '+' );
    printf( "%c", '-' );
    printf( "%c", '+' );
    printf( "%c", '-' );
    printf( "%c", '+' );
    printf( "\n" );
    
    // Cell 2
    printf( "\nRemoving wall from cell 2:\n" );
    
    // Remove wall code:
    // Remove a cell wall if possible for cell 2
    {
        int cellIndex = 2;
        int wallToRemove;
        bool wallRemoved = false;
        int direction = LEFT;
        
        // If the wall in this direction is NOT an exterior wall
        if ( cellToWallLUT[cellIndex][direction] >= 0 )
        {
            wallToRemove = cellToWallLUT[cellIndex][direction];
            // If this wall has not been already removed
            if ( wallList[wallToRemove] == true )
            {
                // Remove the wall and indicate a wall was successfully removed
                wallList[wallToRemove] = false;
                wallRemoved = true;
            }
        }
        
        // If we haven't removed a wall yet, try to remove one in the next direction
        // Check the guard, have we removed one wall
        if ( wallRemoved == false )
        {
            direction = direction + 1;
            // If the wall in this direction is NOT an exterior wall
            if ( cellToWallLUT[cellIndex][direction] >= 0 )
            {
                wallToRemove = cellToWallLUT[cellIndex][direction];
                // If this wall has not been already removed
                if ( wallList[wallToRemove] == true )
                {
                    // Remove the wall and indicate a wall was successfully removed
                    wallList[wallToRemove] = false;
                    wallRemoved = true;
                }
            }
        }
        
        // If we haven't removed a wall yet, try to remove one in the next direction
        // Check the guard, have we removed one wall
        if ( wallRemoved == false )
        {
            direction = direction + 1;
            // If the wall in this direction is NOT an exterior wall
            if ( cellToWallLUT[cellIndex][direction] >= 0 )
            {
                wallToRemove = cellToWallLUT[cellIndex][direction];
                // If this wall has not been already removed
                if ( wallList[wallToRemove] == true )
                {
                    // Remove the wall and indicate a wall was successfully removed
                    wallList[wallToRemove] = false;
                    wallRemoved = true;
                }
            }
        }
        
        // If we haven't removed a wall yet, try to remove one in the next direction
        // Check the guard, have we removed one wall
        if ( wallRemoved == false )
        {
            direction = direction + 1;
            // If the wall in this direction is NOT an exterior wall
            if ( cellToWallLUT[cellIndex][direction] >= 0 )
            {
                wallToRemove = cellToWallLUT[cellIndex][direction];
                // If this wall has not been already removed
                if ( wallList[wallToRemove] == true )
                {
                    // Remove the wall and indicate a wall was successfully removed
                    wallList[wallToRemove] = false;
                    wallRemoved = true;
                }
            }
        }
    }
    
    // Print maze after cell 2
    usleep( 500000 );
    
    // Print maze (duplicate of above print code)
    currentInteriorWall = 0;
    
    // +-+-+-+
    printf( "%c", '+' );
    printf( "%c", '-' );
    printf( "%c", '+' );
    printf( "%c", '-' );
    printf( "%c", '+' );
    printf( "%c", '-' );
    printf( "%c", '+' );
    printf( "\n" );
    
    // Row 1 vertical walls and cells
    printf( "%c", '|' );
    printf( "%c", ' ' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '|' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", ' ' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '|' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", ' ' );
    printf( "%c", '|' );
    printf( "\n" );
    
    // Row 2 horizontal walls
    printf( "%c", '+' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '-' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", '+' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '-' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", '+' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '-' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", '+' );
    printf( "\n" );
    
    // Row 2 vertical walls and cells
    printf( "%c", '|' );
    printf( "%c", ' ' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '|' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", ' ' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '|' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", ' ' );
    printf( "%c", '|' );
    printf( "\n" );
    
    // Row 3 horizontal walls
    printf( "%c", '+' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '-' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", '+' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '-' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", '+' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '-' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", '+' );
    printf( "\n" );
    
    // Row 3 vertical walls and cells
    printf( "%c", '|' );
    printf( "%c", ' ' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '|' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", ' ' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '|' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", ' ' );
    printf( "%c", '|' );
    printf( "\n" );
    
    // Bottom border
    printf( "%c", '+' );
    printf( "%c", '-' );
    printf( "%c", '+' );
    printf( "%c", '-' );
    printf( "%c", '+' );
    printf( "%c", '-' );
    printf( "%c", '+' );
    printf( "\n" );
    
    // Cell 3
    printf( "\nRemoving wall from cell 3:\n" );
    
    // Remove wall code:
    // Remove a cell wall if possible for cell 3
    {
        int cellIndex = 3;
        int wallToRemove;
        bool wallRemoved = false;
        int direction = LEFT;
        
        // If the wall in this direction is NOT an exterior wall
        if ( cellToWallLUT[cellIndex][direction] >= 0 )
        {
            wallToRemove = cellToWallLUT[cellIndex][direction];
            // If this wall has not been already removed
            if ( wallList[wallToRemove] == true )
            {
                // Remove the wall and indicate a wall was successfully removed
                wallList[wallToRemove] = false;
                wallRemoved = true;
            }
        }
        
        // If we haven't removed a wall yet, try to remove one in the next direction
        // Check the guard, have we removed one wall
        if ( wallRemoved == false )
        {
            direction = direction + 1;
            // If the wall in this direction is NOT an exterior wall
            if ( cellToWallLUT[cellIndex][direction] >= 0 )
            {
                wallToRemove = cellToWallLUT[cellIndex][direction];
                // If this wall has not been already removed
                if ( wallList[wallToRemove] == true )
                {
                    // Remove the wall and indicate a wall was successfully removed
                    wallList[wallToRemove] = false;
                    wallRemoved = true;
                }
            }
        }
        
        // If we haven't removed a wall yet, try to remove one in the next direction
        // Check the guard, have we removed one wall
        if ( wallRemoved == false )
        {
            direction = direction + 1;
            // If the wall in this direction is NOT an exterior wall
            if ( cellToWallLUT[cellIndex][direction] >= 0 )
            {
                wallToRemove = cellToWallLUT[cellIndex][direction];
                // If this wall has not been already removed
                if ( wallList[wallToRemove] == true )
                {
                    // Remove the wall and indicate a wall was successfully removed
                    wallList[wallToRemove] = false;
                    wallRemoved = true;
                }
            }
        }
        
        // If we haven't removed a wall yet, try to remove one in the next direction
        // Check the guard, have we removed one wall
        if ( wallRemoved == false )
        {
            direction = direction + 1;
            // If the wall in this direction is NOT an exterior wall
            if ( cellToWallLUT[cellIndex][direction] >= 0 )
            {
                wallToRemove = cellToWallLUT[cellIndex][direction];
                // If this wall has not been already removed
                if ( wallList[wallToRemove] == true )
                {
                    // Remove the wall and indicate a wall was successfully removed
                    wallList[wallToRemove] = false;
                    wallRemoved = true;
                }
            }
        }
    }
    
    // Print maze after cell 3
    usleep( 500000 );
    
    // Print maze (duplicate of above print code)
    currentInteriorWall = 0;
    
    // +-+-+-+
    printf( "%c", '+' );
    printf( "%c", '-' );
    printf( "%c", '+' );
    printf( "%c", '-' );
    printf( "%c", '+' );
    printf( "%c", '-' );
    printf( "%c", '+' );
    printf( "\n" );
    
    // Row 1 vertical walls and cells
    printf( "%c", '|' );
    printf( "%c", ' ' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '|' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", ' ' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '|' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", ' ' );
    printf( "%c", '|' );
    printf( "\n" );
    
    // Row 2 horizontal walls
    printf( "%c", '+' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '-' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", '+' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '-' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", '+' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '-' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", '+' );
    printf( "\n" );
    
    // Row 2 vertical walls and cells
    printf( "%c", '|' );
    printf( "%c", ' ' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '|' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", ' ' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '|' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", ' ' );
    printf( "%c", '|' );
    printf( "\n" );
    
    // Row 3 horizontal walls
    printf( "%c", '+' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '-' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", '+' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '-' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", '+' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '-' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", '+' );
    printf( "\n" );
    
    // Row 3 vertical walls and cells
    printf( "%c", '|' );
    printf( "%c", ' ' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '|' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", ' ' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '|' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", ' ' );
    printf( "%c", '|' );
    printf( "\n" );
    
    // Bottom border
    printf( "%c", '+' );
    printf( "%c", '-' );
    printf( "%c", '+' );
    printf( "%c", '-' );
    printf( "%c", '+' );
    printf( "%c", '-' );
    printf( "%c", '+' );
    printf( "\n" );
    
    // Cell 4
    printf( "\nRemoving wall from cell 4:\n" );
    
    // Remove wall code:
    // Remove a cell wall if possible for cell 4
    {
        int cellIndex = 4;
        int wallToRemove;
        bool wallRemoved = false;
        int direction = LEFT;
        
        // If the wall in this direction is NOT an exterior wall
        if ( cellToWallLUT[cellIndex][direction] >= 0 )
        {
            wallToRemove = cellToWallLUT[cellIndex][direction];
            // If this wall has not been already removed
            if ( wallList[wallToRemove] == true )
            {
                // Remove the wall and indicate a wall was successfully removed
                wallList[wallToRemove] = false;
                wallRemoved = true;
            }
        }
        
        // If we haven't removed a wall yet, try to remove one in the next direction
        // Check the guard, have we removed one wall
        if ( wallRemoved == false )
        {
            direction = direction + 1;
            // If the wall in this direction is NOT an exterior wall
            if ( cellToWallLUT[cellIndex][direction] >= 0 )
            {
                wallToRemove = cellToWallLUT[cellIndex][direction];
                // If this wall has not been already removed
                if ( wallList[wallToRemove] == true )
                {
                    // Remove the wall and indicate a wall was successfully removed
                    wallList[wallToRemove] = false;
                    wallRemoved = true;
                }
            }
        }
        
        // If we haven't removed a wall yet, try to remove one in the next direction
        // Check the guard, have we removed one wall
        if ( wallRemoved == false )
        {
            direction = direction + 1;
            // If the wall in this direction is NOT an exterior wall
            if ( cellToWallLUT[cellIndex][direction] >= 0 )
            {
                wallToRemove = cellToWallLUT[cellIndex][direction];
                // If this wall has not been already removed
                if ( wallList[wallToRemove] == true )
                {
                    // Remove the wall and indicate a wall was successfully removed
                    wallList[wallToRemove] = false;
                    wallRemoved = true;
                }
            }
        }
        
        // If we haven't removed a wall yet, try to remove one in the next direction
        // Check the guard, have we removed one wall
        if ( wallRemoved == false )
        {
            direction = direction + 1;
            // If the wall in this direction is NOT an exterior wall
            if ( cellToWallLUT[cellIndex][direction] >= 0 )
            {
                wallToRemove = cellToWallLUT[cellIndex][direction];
                // If this wall has not been already removed
                if ( wallList[wallToRemove] == true )
                {
                    // Remove the wall and indicate a wall was successfully removed
                    wallList[wallToRemove] = false;
                    wallRemoved = true;
                }
            }
        }
    }
    
    // Print maze after cell 4
    usleep( 500000 );
    
    // Print maze (duplicate of above print code)
    currentInteriorWall = 0;
    
    // +-+-+-+
    printf( "%c", '+' );
    printf( "%c", '-' );
    printf( "%c", '+' );
    printf( "%c", '-' );
    printf( "%c", '+' );
    printf( "%c", '-' );
    printf( "%c", '+' );
    printf( "\n" );
    
    // Row 1 vertical walls and cells
    printf( "%c", '|' );
    printf( "%c", ' ' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '|' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", ' ' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '|' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", ' ' );
    printf( "%c", '|' );
    printf( "\n" );
    
    // Row 2 horizontal walls
    printf( "%c", '+' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '-' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", '+' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '-' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", '+' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '-' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", '+' );
    printf( "\n" );
    
    // Row 2 vertical walls and cells
    printf( "%c", '|' );
    printf( "%c", ' ' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '|' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", ' ' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '|' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", ' ' );
    printf( "%c", '|' );
    printf( "\n" );
    
    // Row 3 horizontal walls
    printf( "%c", '+' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '-' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", '+' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '-' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", '+' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '-' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", '+' );
    printf( "\n" );
    
    // Row 3 vertical walls and cells
    printf( "%c", '|' );
    printf( "%c", ' ' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '|' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", ' ' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '|' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", ' ' );
    printf( "%c", '|' );
    printf( "\n" );
    
    // Bottom border
    printf( "%c", '+' );
    printf( "%c", '-' );
    printf( "%c", '+' );
    printf( "%c", '-' );
    printf( "%c", '+' );
    printf( "%c", '-' );
    printf( "%c", '+' );
    printf( "\n" );
    
    // Cell 5
    printf( "\nRemoving wall from cell 5:\n" );
    
    // Remove wall code:
    // Remove a cell wall if possible for cell 5
    {
        int cellIndex = 5;
        int wallToRemove;
        bool wallRemoved = false;
        int direction = LEFT;
        
        // If the wall in this direction is NOT an exterior wall
        if ( cellToWallLUT[cellIndex][direction] >= 0 )
        {
            wallToRemove = cellToWallLUT[cellIndex][direction];
            // If this wall has not been already removed
            if ( wallList[wallToRemove] == true )
            {
                // Remove the wall and indicate a wall was successfully removed
                wallList[wallToRemove] = false;
                wallRemoved = true;
            }
        }
        
        // If we haven't removed a wall yet, try to remove one in the next direction
        // Check the guard, have we removed one wall
        if ( wallRemoved == false )
        {
            direction = direction + 1;
            // If the wall in this direction is NOT an exterior wall
            if ( cellToWallLUT[cellIndex][direction] >= 0 )
            {
                wallToRemove = cellToWallLUT[cellIndex][direction];
                // If this wall has not been already removed
                if ( wallList[wallToRemove] == true )
                {
                    // Remove the wall and indicate a wall was successfully removed
                    wallList[wallToRemove] = false;
                    wallRemoved = true;
                }
            }
        }
        
        // If we haven't removed a wall yet, try to remove one in the next direction
        // Check the guard, have we removed one wall
        if ( wallRemoved == false )
        {
            direction = direction + 1;
            // If the wall in this direction is NOT an exterior wall
            if ( cellToWallLUT[cellIndex][direction] >= 0 )
            {
                wallToRemove = cellToWallLUT[cellIndex][direction];
                // If this wall has not been already removed
                if ( wallList[wallToRemove] == true )
                {
                    // Remove the wall and indicate a wall was successfully removed
                    wallList[wallToRemove] = false;
                    wallRemoved = true;
                }
            }
        }
        
        // If we haven't removed a wall yet, try to remove one in the next direction
        // Check the guard, have we removed one wall
        if ( wallRemoved == false )
        {
            direction = direction + 1;
            // If the wall in this direction is NOT an exterior wall
            if ( cellToWallLUT[cellIndex][direction] >= 0 )
            {
                wallToRemove = cellToWallLUT[cellIndex][direction];
                // If this wall has not been already removed
                if ( wallList[wallToRemove] == true )
                {
                    // Remove the wall and indicate a wall was successfully removed
                    wallList[wallToRemove] = false;
                    wallRemoved = true;
                }
            }
        }
    }
    
    // Print maze after cell 5
    usleep( 500000 );
    
    // Print maze (duplicate of above print code)
    currentInteriorWall = 0;
    
    // +-+-+-+
    printf( "%c", '+' );
    printf( "%c", '-' );
    printf( "%c", '+' );
    printf( "%c", '-' );
    printf( "%c", '+' );
    printf( "%c", '-' );
    printf( "%c", '+' );
    printf( "\n" );
    
    // Row 1 vertical walls and cells
    printf( "%c", '|' );
    printf( "%c", ' ' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '|' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", ' ' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '|' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", ' ' );
    printf( "%c", '|' );
    printf( "\n" );
    
    // Row 2 horizontal walls
    printf( "%c", '+' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '-' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", '+' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '-' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", '+' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '-' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", '+' );
    printf( "\n" );
    
    // Row 2 vertical walls and cells
    printf( "%c", '|' );
    printf( "%c", ' ' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '|' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", ' ' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '|' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", ' ' );
    printf( "%c", '|' );
    printf( "\n" );
    
    // Row 3 horizontal walls
    printf( "%c", '+' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '-' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", '+' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '-' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", '+' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '-' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", '+' );
    printf( "\n" );
    
    // Row 3 vertical walls and cells
    printf( "%c", '|' );
    printf( "%c", ' ' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '|' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", ' ' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '|' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", ' ' );
    printf( "%c", '|' );
    printf( "\n" );
    
    // Bottom border
    printf( "%c", '+' );
    printf( "%c", '-' );
    printf( "%c", '+' );
    printf( "%c", '-' );
    printf( "%c", '+' );
    printf( "%c", '-' );
    printf( "%c", '+' );
    printf( "\n" );
    
    // Cell 6
    printf( "\nRemoving wall from cell 6:\n" );
    
    // Remove wall code:
    // Remove a cell wall if possible for cell 6
    {
        int cellIndex = 6;
        int wallToRemove;
        bool wallRemoved = false;
        int direction = LEFT;
        
        // If the wall in this direction is NOT an exterior wall
        if ( cellToWallLUT[cellIndex][direction] >= 0 )
        {
            wallToRemove = cellToWallLUT[cellIndex][direction];
            // If this wall has not been already removed
            if ( wallList[wallToRemove] == true )
            {
                // Remove the wall and indicate a wall was successfully removed
                wallList[wallToRemove] = false;
                wallRemoved = true;
            }
        }
        
        // If we haven't removed a wall yet, try to remove one in the next direction
        // Check the guard, have we removed one wall
        if ( wallRemoved == false )
        {
            direction = direction + 1;
            // If the wall in this direction is NOT an exterior wall
            if ( cellToWallLUT[cellIndex][direction] >= 0 )
            {
                wallToRemove = cellToWallLUT[cellIndex][direction];
                // If this wall has not been already removed
                if ( wallList[wallToRemove] == true )
                {
                    // Remove the wall and indicate a wall was successfully removed
                    wallList[wallToRemove] = false;
                    wallRemoved = true;
                }
            }
        }
        
        // If we haven't removed a wall yet, try to remove one in the next direction
        // Check the guard, have we removed one wall
        if ( wallRemoved == false )
        {
            direction = direction + 1;
            // If the wall in this direction is NOT an exterior wall
            if ( cellToWallLUT[cellIndex][direction] >= 0 )
            {
                wallToRemove = cellToWallLUT[cellIndex][direction];
                // If this wall has not been already removed
                if ( wallList[wallToRemove] == true )
                {
                    // Remove the wall and indicate a wall was successfully removed
                    wallList[wallToRemove] = false;
                    wallRemoved = true;
                }
            }
        }
        
        // If we haven't removed a wall yet, try to remove one in the next direction
        // Check the guard, have we removed one wall
        if ( wallRemoved == false )
        {
            direction = direction + 1;
            // If the wall in this direction is NOT an exterior wall
            if ( cellToWallLUT[cellIndex][direction] >= 0 )
            {
                wallToRemove = cellToWallLUT[cellIndex][direction];
                // If this wall has not been already removed
                if ( wallList[wallToRemove] == true )
                {
                    // Remove the wall and indicate a wall was successfully removed
                    wallList[wallToRemove] = false;
                    wallRemoved = true;
                }
            }
        }
    }
    
    // Print maze after cell 6
    usleep( 500000 );
    
    // Print maze (duplicate of above print code)
    currentInteriorWall = 0;
    
    // +-+-+-+
    printf( "%c", '+' );
    printf( "%c", '-' );
    printf( "%c", '+' );
    printf( "%c", '-' );
    printf( "%c", '+' );
    printf( "%c", '-' );
    printf( "%c", '+' );
    printf( "\n" );
    
    // Row 1 vertical walls and cells
    printf( "%c", '|' );
    printf( "%c", ' ' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '|' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", ' ' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '|' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", ' ' );
    printf( "%c", '|' );
    printf( "\n" );
    
    // Row 2 horizontal walls
    printf( "%c", '+' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '-' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", '+' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '-' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", '+' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '-' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", '+' );
    printf( "\n" );
    
    // Row 2 vertical walls and cells
    printf( "%c", '|' );
    printf( "%c", ' ' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '|' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", ' ' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '|' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", ' ' );
    printf( "%c", '|' );
    printf( "\n" );
    
    // Row 3 horizontal walls
    printf( "%c", '+' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '-' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", '+' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '-' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", '+' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '-' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", '+' );
    printf( "\n" );
    
    // Row 3 vertical walls and cells
    printf( "%c", '|' );
    printf( "%c", ' ' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '|' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", ' ' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '|' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", ' ' );
    printf( "%c", '|' );
    printf( "\n" );
    
    // Bottom border
    printf( "%c", '+' );
    printf( "%c", '-' );
    printf( "%c", '+' );
    printf( "%c", '-' );
    printf( "%c", '+' );
    printf( "%c", '-' );
    printf( "%c", '+' );
    printf( "\n" );
    
    // Cell 7
    printf( "\nRemoving wall from cell 7:\n" );
    
    // Remove wall code:
    // Remove a cell wall if possible for cell 7
    {
        int cellIndex = 7;
        int wallToRemove;
        bool wallRemoved = false;
        int direction = LEFT;
        
        // If the wall in this direction is NOT an exterior wall
        if ( cellToWallLUT[cellIndex][direction] >= 0 )
        {
            wallToRemove = cellToWallLUT[cellIndex][direction];
            // If this wall has not been already removed
            if ( wallList[wallToRemove] == true )
            {
                // Remove the wall and indicate a wall was successfully removed
                wallList[wallToRemove] = false;
                wallRemoved = true;
            }
        }
        
        // If we haven't removed a wall yet, try to remove one in the next direction
        // Check the guard, have we removed one wall
        if ( wallRemoved == false )
        {
            direction = direction + 1;
            // If the wall in this direction is NOT an exterior wall
            if ( cellToWallLUT[cellIndex][direction] >= 0 )
            {
                wallToRemove = cellToWallLUT[cellIndex][direction];
                // If this wall has not been already removed
                if ( wallList[wallToRemove] == true )
                {
                    // Remove the wall and indicate a wall was successfully removed
                    wallList[wallToRemove] = false;
                    wallRemoved = true;
                }
            }
        }
        
        // If we haven't removed a wall yet, try to remove one in the next direction
        // Check the guard, have we removed one wall
        if ( wallRemoved == false )
        {
            direction = direction + 1;
            // If the wall in this direction is NOT an exterior wall
            if ( cellToWallLUT[cellIndex][direction] >= 0 )
            {
                wallToRemove = cellToWallLUT[cellIndex][direction];
                // If this wall has not been already removed
                if ( wallList[wallToRemove] == true )
                {
                    // Remove the wall and indicate a wall was successfully removed
                    wallList[wallToRemove] = false;
                    wallRemoved = true;
                }
            }
        }
        
        // If we haven't removed a wall yet, try to remove one in the next direction
        // Check the guard, have we removed one wall
        if ( wallRemoved == false )
        {
            direction = direction + 1;
            // If the wall in this direction is NOT an exterior wall
            if ( cellToWallLUT[cellIndex][direction] >= 0 )
            {
                wallToRemove = cellToWallLUT[cellIndex][direction];
                // If this wall has not been already removed
                if ( wallList[wallToRemove] == true )
                {
                    // Remove the wall and indicate a wall was successfully removed
                    wallList[wallToRemove] = false;
                    wallRemoved = true;
                }
            }
        }
    }
    
    // Print maze after cell 7
    usleep( 500000 );
    
    // Print maze (duplicate of above print code)
    currentInteriorWall = 0;
    
    // +-+-+-+
    printf( "%c", '+' );
    printf( "%c", '-' );
    printf( "%c", '+' );
    printf( "%c", '-' );
    printf( "%c", '+' );
    printf( "%c", '-' );
    printf( "%c", '+' );
    printf( "\n" );
    
    // Row 1 vertical walls and cells
    printf( "%c", '|' );
    printf( "%c", ' ' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '|' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", ' ' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '|' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", ' ' );
    printf( "%c", '|' );
    printf( "\n" );
    
    // Row 2 horizontal walls
    printf( "%c", '+' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '-' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", '+' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '-' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", '+' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '-' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", '+' );
    printf( "\n" );
    
    // Row 2 vertical walls and cells
    printf( "%c", '|' );
    printf( "%c", ' ' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '|' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", ' ' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '|' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", ' ' );
    printf( "%c", '|' );
    printf( "\n" );
    
    // Row 3 horizontal walls
    printf( "%c", '+' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '-' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", '+' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '-' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", '+' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '-' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", '+' );
    printf( "\n" );
    
    // Row 3 vertical walls and cells
    printf( "%c", '|' );
    printf( "%c", ' ' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '|' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", ' ' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '|' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", ' ' );
    printf( "%c", '|' );
    printf( "\n" );
    
    // Bottom border
    printf( "%c", '+' );
    printf( "%c", '-' );
    printf( "%c", '+' );
    printf( "%c", '-' );
    printf( "%c", '+' );
    printf( "%c", '-' );
    printf( "%c", '+' );
    printf( "\n" );
    
    // Cell 8
    printf( "\nRemoving wall from cell 8:\n" );
    
    // Remove wall code:
    // Remove a cell wall if possible for cell 0
    {
        int cellIndex = 8;
        int wallToRemove;
        bool wallRemoved = false;
        int direction = LEFT;
        
        // If the wall in this direction is NOT an exterior wall
        if ( cellToWallLUT[cellIndex][direction] >= 0 )
        {
            wallToRemove = cellToWallLUT[cellIndex][direction];
            // If this wall has not been already removed
            if ( wallList[wallToRemove] == true )
            {
                // Remove the wall and indicate a wall was successfully removed
                wallList[wallToRemove] = false;
                wallRemoved = true;
            }
        }
        
        // If we haven't removed a wall yet, try to remove one in the next direction
        // Check the guard, have we removed one wall
        if ( wallRemoved == false )
        {
            direction = direction + 1;
            // If the wall in this direction is NOT an exterior wall
            if ( cellToWallLUT[cellIndex][direction] >= 0 )
            {
                wallToRemove = cellToWallLUT[cellIndex][direction];
                // If this wall has not been already removed
                if ( wallList[wallToRemove] == true )
                {
                    // Remove the wall and indicate a wall was successfully removed
                    wallList[wallToRemove] = false;
                    wallRemoved = true;
                }
            }
        }
        
        // If we haven't removed a wall yet, try to remove one in the next direction
        // Check the guard, have we removed one wall
        if ( wallRemoved == false )
        {
            direction = direction + 1;
            // If the wall in this direction is NOT an exterior wall
            if ( cellToWallLUT[cellIndex][direction] >= 0 )
            {
                wallToRemove = cellToWallLUT[cellIndex][direction];
                // If this wall has not been already removed
                if ( wallList[wallToRemove] == true )
                {
                    // Remove the wall and indicate a wall was successfully removed
                    wallList[wallToRemove] = false;
                    wallRemoved = true;
                }
            }
        }
        
        // If we haven't removed a wall yet, try to remove one in the next direction
        // Check the guard, have we removed one wall
        if ( wallRemoved == false )
        {
            direction = direction + 1;
            // If the wall in this direction is NOT an exterior wall
            if ( cellToWallLUT[cellIndex][direction] >= 0 )
            {
                wallToRemove = cellToWallLUT[cellIndex][direction];
                // If this wall has not been already removed
                if ( wallList[wallToRemove] == true )
                {
                    // Remove the wall and indicate a wall was successfully removed
                    wallList[wallToRemove] = false;
                    wallRemoved = true;
                }
            }
        }
    }
    
    // Print maze after cell 8
    usleep( 500000 );
    
    // Print maze (duplicate of above print code)
    currentInteriorWall = 0;
    
    // +-+-+-+
    printf( "%c", '+' );
    printf( "%c", '-' );
    printf( "%c", '+' );
    printf( "%c", '-' );
    printf( "%c", '+' );
    printf( "%c", '-' );
    printf( "%c", '+' );
    printf( "\n" );
    
    // Row 1 vertical walls and cells
    printf( "%c", '|' );
    printf( "%c", ' ' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '|' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", ' ' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '|' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", ' ' );
    printf( "%c", '|' );
    printf( "\n" );
    
    // Row 2 horizontal walls
    printf( "%c", '+' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '-' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", '+' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '-' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", '+' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '-' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", '+' );
    printf( "\n" );
    
    // Row 2 vertical walls and cells
    printf( "%c", '|' );
    printf( "%c", ' ' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '|' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", ' ' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '|' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", ' ' );
    printf( "%c", '|' );
    printf( "\n" );
    
    // Row 3 horizontal walls
    printf( "%c", '+' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '-' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", '+' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '-' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", '+' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '-' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", '+' );
    printf( "\n" );
    
    // Row 3 vertical walls and cells
    printf( "%c", '|' );
    printf( "%c", ' ' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '|' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", ' ' );
    if ( wallList[currentInteriorWall] == true )
    {
        printf( "%c", '|' );
    }
    else
    {
        printf( "%c", ' ' );
    }
    currentInteriorWall = currentInteriorWall + 1;
    printf( "%c", ' ' );
    printf( "%c", '|' );
    printf( "\n" );
    
    // Bottom border
    printf( "%c", '+' );
    printf( "%c", '-' );
    printf( "%c", '+' );
    printf( "%c", '-' );
    printf( "%c", '+' );
    printf( "%c", '-' );
    printf( "%c", '+' );
    printf( "\n" );
    
    return 0;
}
