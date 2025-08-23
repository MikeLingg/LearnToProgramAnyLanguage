using System;
using System.Threading;

class Program
{
    static void Main()
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
        int[,] cellToWallLUT = {
            {-1, -1, 0, 2},   {0, -1, 1, 3},   {1, -1, -1, 4},
            {-1, 2, 5, 7},    {5, 3, 6, 8},    {6, 4, -1, 9},
            {-1, 7, 10, -1},  {10, 8, 11, -1}, {11, 9, -1, -1}
        };
        
        // 12 interior walls in a 3x3 maze. Start with all the walls up.
        bool[] wallList = {true, true, true, true, true, true, true, true, true, true, true, true};
        
        // Print initial maze
        Console.WriteLine( "Initial Maze:" );
        
        // Print out the maze, this is a rather painful copy/paste job without loops and functions.
        // Note Console.Write is meant to print a single character with no newlines.
        // Horizontal walls above row 1 - All are exterior walls, no conditions.
        int currentInteriorWall = 0;
        
        // +-+-+-+
        Console.Write( '+' );
        Console.Write( '-' );
        Console.Write( '+' );
        Console.Write( '-' );
        Console.Write( '+' );
        Console.Write( '-' );
        Console.Write( '+' );
        Console.WriteLine();
        
        // Vertical walls and cells row 1.
        // The left and right vertical walls are exterior, always up.
        // | | | |
        Console.Write( '|' );
        Console.Write( ' ' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '|' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( ' ' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '|' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( ' ' );
        Console.Write( '|' );
        Console.WriteLine();
        
        // Horizontal walls above row 2
        // +-+-+-+
        Console.Write( '+' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '-' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( '+' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '-' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( '+' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '-' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( '+' );
        Console.WriteLine();
        
        // Vertical walls and cells row 2.
        // The left and right vertical walls are exterior, always up.
        // | | | |
        Console.Write( '|' );
        Console.Write( ' ' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '|' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( ' ' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '|' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( ' ' );
        Console.Write( '|' );
        Console.WriteLine();
        
        // Horizontal walls above row 3
        // +-+-+-+
        Console.Write( '+' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '-' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( '+' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '-' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( '+' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '-' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( '+' );
        Console.WriteLine();
        
        // Vertical walls and cells row 3.
        // The left and right vertical walls are exterior, always up.
        // | | | |
        Console.Write( '|' );
        Console.Write( ' ' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '|' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( ' ' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '|' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( ' ' );
        Console.Write( '|' );
        Console.WriteLine();
        
        // Horizontal walls below row 3 - All are exterior walls, no conditions.
        // +-+-+-+
        Console.Write( '+' );
        Console.Write( '-' );
        Console.Write( '+' );
        Console.Write( '-' );
        Console.Write( '+' );
        Console.Write( '-' );
        Console.Write( '+' );
        Console.WriteLine();
        
        // Now process each cell 0-8
        // Cell 0
        Console.WriteLine( "\nRemoving wall from cell 0:" );
        
        // Remove wall code:
        // Remove a cell wall if possible for cell 0
        int cellIndex = 0;
        int wallToRemove;
        
        bool wallRemoved = false;
        int direction = LEFT;
        
        // If the wall in this direction is NOT an exterior wall
        if ( cellToWallLUT[cellIndex, direction] >= 0 )
        {
            wallToRemove = cellToWallLUT[cellIndex, direction];
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
            if ( cellToWallLUT[cellIndex, direction] >= 0 )
            {
                wallToRemove = cellToWallLUT[cellIndex, direction];
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
            if ( cellToWallLUT[cellIndex, direction] >= 0 )
            {
                wallToRemove = cellToWallLUT[cellIndex, direction];
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
            if ( cellToWallLUT[cellIndex, direction] >= 0 )
            {
                wallToRemove = cellToWallLUT[cellIndex, direction];
                // If this wall has not been already removed
                if ( wallList[wallToRemove] == true )
                {
                    // Remove the wall and indicate a wall was successfully removed
                    wallList[wallToRemove] = false;
                    wallRemoved = true;
                }
            }
        }
        
        // Print maze after cell 0
        Thread.Sleep( 500 );
        
        // Print maze (duplicate of above print code)
        currentInteriorWall = 0;
        
        // +-+-+-+
        Console.Write( '+' );
        Console.Write( '-' );
        Console.Write( '+' );
        Console.Write( '-' );
        Console.Write( '+' );
        Console.Write( '-' );
        Console.Write( '+' );
        Console.WriteLine();
        
        // Row 1 vertical walls and cells
        Console.Write( '|' );
        Console.Write( ' ' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '|' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( ' ' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '|' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( ' ' );
        Console.Write( '|' );
        Console.WriteLine();
        
        // Row 2 horizontal walls
        Console.Write( '+' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '-' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( '+' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '-' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( '+' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '-' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( '+' );
        Console.WriteLine();
        
        // Row 2 vertical walls and cells
        Console.Write( '|' );
        Console.Write( ' ' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '|' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( ' ' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '|' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( ' ' );
        Console.Write( '|' );
        Console.WriteLine();
        
        // Row 3 horizontal walls
        Console.Write( '+' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '-' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( '+' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '-' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( '+' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '-' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( '+' );
        Console.WriteLine();
        
        // Row 3 vertical walls and cells
        Console.Write( '|' );
        Console.Write( ' ' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '|' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( ' ' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '|' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( ' ' );
        Console.Write( '|' );
        Console.WriteLine();
        
        // Bottom border
        Console.Write( '+' );
        Console.Write( '-' );
        Console.Write( '+' );
        Console.Write( '-' );
        Console.Write( '+' );
        Console.Write( '-' );
        Console.Write( '+' );
        Console.WriteLine();

        // Cell 1
        Console.WriteLine( "\nRemoving wall from cell 1:" );
        
        // Remove wall code:
        // Remove a cell wall if possible for cell 1
        cellIndex = 1;
        
        wallRemoved = false;
        direction = LEFT;
        
        // If the wall in this direction is NOT an exterior wall
        if ( cellToWallLUT[cellIndex, direction] >= 0 )
        {
            wallToRemove = cellToWallLUT[cellIndex, direction];
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
            if ( cellToWallLUT[cellIndex, direction] >= 0 )
            {
                wallToRemove = cellToWallLUT[cellIndex, direction];
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
            if ( cellToWallLUT[cellIndex, direction] >= 0 )
            {
                wallToRemove = cellToWallLUT[cellIndex, direction];
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
            if ( cellToWallLUT[cellIndex, direction] >= 0 )
            {
                wallToRemove = cellToWallLUT[cellIndex, direction];
                // If this wall has not been already removed
                if ( wallList[wallToRemove] == true )
                {
                    // Remove the wall and indicate a wall was successfully removed
                    wallList[wallToRemove] = false;
                    wallRemoved = true;
                }
            }
        }
        
        // Print maze after cell 1
        Thread.Sleep( 500 );
        
        // Print maze (duplicate of above print code)
        currentInteriorWall = 0;
        
        // +-+-+-+
        Console.Write( '+' );
        Console.Write( '-' );
        Console.Write( '+' );
        Console.Write( '-' );
        Console.Write( '+' );
        Console.Write( '-' );
        Console.Write( '+' );
        Console.WriteLine();
        
        // Row 1 vertical walls and cells
        Console.Write( '|' );
        Console.Write( ' ' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '|' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( ' ' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '|' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( ' ' );
        Console.Write( '|' );
        Console.WriteLine();
        
        // Row 2 horizontal walls
        Console.Write( '+' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '-' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( '+' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '-' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( '+' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '-' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( '+' );
        Console.WriteLine();
        
        // Row 2 vertical walls and cells
        Console.Write( '|' );
        Console.Write( ' ' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '|' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( ' ' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '|' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( ' ' );
        Console.Write( '|' );
        Console.WriteLine();
        
        // Row 3 horizontal walls
        Console.Write( '+' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '-' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( '+' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '-' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( '+' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '-' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( '+' );
        Console.WriteLine();
        
        // Row 3 vertical walls and cells
        Console.Write( '|' );
        Console.Write( ' ' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '|' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( ' ' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '|' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( ' ' );
        Console.Write( '|' );
        Console.WriteLine();
        
        // Bottom border
        Console.Write( '+' );
        Console.Write( '-' );
        Console.Write( '+' );
        Console.Write( '-' );
        Console.Write( '+' );
        Console.Write( '-' );
        Console.Write( '+' );
        Console.WriteLine();

        // Cell 2
        Console.WriteLine( "\nRemoving wall from cell 2:" );
        
        // Remove wall code:
        // Remove a cell wall if possible for cell 2
        cellIndex = 2;
        
        wallRemoved = false;
        direction = LEFT;
        
        // If the wall in this direction is NOT an exterior wall
        if ( cellToWallLUT[cellIndex, direction] >= 0 )
        {
            wallToRemove = cellToWallLUT[cellIndex, direction];
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
            if ( cellToWallLUT[cellIndex, direction] >= 0 )
            {
                wallToRemove = cellToWallLUT[cellIndex, direction];
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
            if ( cellToWallLUT[cellIndex, direction] >= 0 )
            {
                wallToRemove = cellToWallLUT[cellIndex, direction];
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
            if ( cellToWallLUT[cellIndex, direction] >= 0 )
            {
                wallToRemove = cellToWallLUT[cellIndex, direction];
                // If this wall has not been already removed
                if ( wallList[wallToRemove] == true )
                {
                    // Remove the wall and indicate a wall was successfully removed
                    wallList[wallToRemove] = false;
                    wallRemoved = true;
                }
            }
        }
        
        // Print maze after cell 2
        Thread.Sleep( 500 );
        
        // Print maze (duplicate of above print code)
        currentInteriorWall = 0;
        
        // +-+-+-+
        Console.Write( '+' );
        Console.Write( '-' );
        Console.Write( '+' );
        Console.Write( '-' );
        Console.Write( '+' );
        Console.Write( '-' );
        Console.Write( '+' );
        Console.WriteLine();
        
        // Row 1 vertical walls and cells
        Console.Write( '|' );
        Console.Write( ' ' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '|' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( ' ' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '|' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( ' ' );
        Console.Write( '|' );
        Console.WriteLine();
        
        // Row 2 horizontal walls
        Console.Write( '+' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '-' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( '+' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '-' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( '+' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '-' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( '+' );
        Console.WriteLine();
        
        // Row 2 vertical walls and cells
        Console.Write( '|' );
        Console.Write( ' ' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '|' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( ' ' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '|' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( ' ' );
        Console.Write( '|' );
        Console.WriteLine();
        
        // Row 3 horizontal walls
        Console.Write( '+' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '-' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( '+' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '-' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( '+' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '-' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( '+' );
        Console.WriteLine();
        
        // Row 3 vertical walls and cells
        Console.Write( '|' );
        Console.Write( ' ' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '|' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( ' ' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '|' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( ' ' );
        Console.Write( '|' );
        Console.WriteLine();
        
        // Bottom border
        Console.Write( '+' );
        Console.Write( '-' );
        Console.Write( '+' );
        Console.Write( '-' );
        Console.Write( '+' );
        Console.Write( '-' );
        Console.Write( '+' );
        Console.WriteLine();

        // Cell 3
        Console.WriteLine( "\nRemoving wall from cell 3:" );
        
        // Remove wall code:
        // Remove a cell wall if possible for cell 3
        cellIndex = 3;
        
        wallRemoved = false;
        direction = LEFT;
        
        // If the wall in this direction is NOT an exterior wall
        if ( cellToWallLUT[cellIndex, direction] >= 0 )
        {
            wallToRemove = cellToWallLUT[cellIndex, direction];
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
            if ( cellToWallLUT[cellIndex, direction] >= 0 )
            {
                wallToRemove = cellToWallLUT[cellIndex, direction];
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
            if ( cellToWallLUT[cellIndex, direction] >= 0 )
            {
                wallToRemove = cellToWallLUT[cellIndex, direction];
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
            if ( cellToWallLUT[cellIndex, direction] >= 0 )
            {
                wallToRemove = cellToWallLUT[cellIndex, direction];
                // If this wall has not been already removed
                if ( wallList[wallToRemove] == true )
                {
                    // Remove the wall and indicate a wall was successfully removed
                    wallList[wallToRemove] = false;
                    wallRemoved = true;
                }
            }
        }
        
        // Print maze after cell 3
        Thread.Sleep( 500 );
        
        // Print maze (duplicate of above print code)
        currentInteriorWall = 0;
        
        // +-+-+-+
        Console.Write( '+' );
        Console.Write( '-' );
        Console.Write( '+' );
        Console.Write( '-' );
        Console.Write( '+' );
        Console.Write( '-' );
        Console.Write( '+' );
        Console.WriteLine();
        
        // Row 1 vertical walls and cells
        Console.Write( '|' );
        Console.Write( ' ' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '|' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( ' ' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '|' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( ' ' );
        Console.Write( '|' );
        Console.WriteLine();
        
        // Row 2 horizontal walls
        Console.Write( '+' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '-' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( '+' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '-' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( '+' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '-' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( '+' );
        Console.WriteLine();
        
        // Row 2 vertical walls and cells
        Console.Write( '|' );
        Console.Write( ' ' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '|' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( ' ' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '|' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( ' ' );
        Console.Write( '|' );
        Console.WriteLine();
        
        // Row 3 horizontal walls
        Console.Write( '+' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '-' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( '+' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '-' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( '+' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '-' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( '+' );
        Console.WriteLine();
        
        // Row 3 vertical walls and cells
        Console.Write( '|' );
        Console.Write( ' ' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '|' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( ' ' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '|' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( ' ' );
        Console.Write( '|' );
        Console.WriteLine();
        
        // Bottom border
        Console.Write( '+' );
        Console.Write( '-' );
        Console.Write( '+' );
        Console.Write( '-' );
        Console.Write( '+' );
        Console.Write( '-' );
        Console.Write( '+' );
        Console.WriteLine();

        // Cell 4
        Console.WriteLine( "\nRemoving wall from cell 4:" );
        
        // Remove wall code:
        // Remove a cell wall if possible for cell 4
        cellIndex = 4;
        
        wallRemoved = false;
        direction = LEFT;
        
        // If the wall in this direction is NOT an exterior wall
        if ( cellToWallLUT[cellIndex, direction] >= 0 )
        {
            wallToRemove = cellToWallLUT[cellIndex, direction];
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
            if ( cellToWallLUT[cellIndex, direction] >= 0 )
            {
                wallToRemove = cellToWallLUT[cellIndex, direction];
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
            if ( cellToWallLUT[cellIndex, direction] >= 0 )
            {
                wallToRemove = cellToWallLUT[cellIndex, direction];
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
            if ( cellToWallLUT[cellIndex, direction] >= 0 )
            {
                wallToRemove = cellToWallLUT[cellIndex, direction];
                // If this wall has not been already removed
                if ( wallList[wallToRemove] == true )
                {
                    // Remove the wall and indicate a wall was successfully removed
                    wallList[wallToRemove] = false;
                    wallRemoved = true;
                }
            }
        }
        
        // Print maze after cell 4
        Thread.Sleep( 500 );
        
        // Print maze (duplicate of above print code)
        currentInteriorWall = 0;
        
        // +-+-+-+
        Console.Write( '+' );
        Console.Write( '-' );
        Console.Write( '+' );
        Console.Write( '-' );
        Console.Write( '+' );
        Console.Write( '-' );
        Console.Write( '+' );
        Console.WriteLine();
        
        // Row 1 vertical walls and cells
        Console.Write( '|' );
        Console.Write( ' ' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '|' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( ' ' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '|' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( ' ' );
        Console.Write( '|' );
        Console.WriteLine();
        
        // Row 2 horizontal walls
        Console.Write( '+' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '-' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( '+' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '-' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( '+' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '-' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( '+' );
        Console.WriteLine();
        
        // Row 2 vertical walls and cells
        Console.Write( '|' );
        Console.Write( ' ' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '|' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( ' ' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '|' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( ' ' );
        Console.Write( '|' );
        Console.WriteLine();
        
        // Row 3 horizontal walls
        Console.Write( '+' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '-' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( '+' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '-' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( '+' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '-' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( '+' );
        Console.WriteLine();
        
        // Row 3 vertical walls and cells
        Console.Write( '|' );
        Console.Write( ' ' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '|' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( ' ' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '|' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( ' ' );
        Console.Write( '|' );
        Console.WriteLine();
        
        // Bottom border
        Console.Write( '+' );
        Console.Write( '-' );
        Console.Write( '+' );
        Console.Write( '-' );
        Console.Write( '+' );
        Console.Write( '-' );
        Console.Write( '+' );
        Console.WriteLine();

        // Cell 5
        Console.WriteLine( "\nRemoving wall from cell 5:" );
        
        // Remove wall code:
        // Remove a cell wall if possible for cell 5
        cellIndex = 5;
        
        wallRemoved = false;
        direction = LEFT;
        
        // If the wall in this direction is NOT an exterior wall
        if ( cellToWallLUT[cellIndex, direction] >= 0 )
        {
            wallToRemove = cellToWallLUT[cellIndex, direction];
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
            if ( cellToWallLUT[cellIndex, direction] >= 0 )
            {
                wallToRemove = cellToWallLUT[cellIndex, direction];
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
            if ( cellToWallLUT[cellIndex, direction] >= 0 )
            {
                wallToRemove = cellToWallLUT[cellIndex, direction];
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
            if ( cellToWallLUT[cellIndex, direction] >= 0 )
            {
                wallToRemove = cellToWallLUT[cellIndex, direction];
                // If this wall has not been already removed
                if ( wallList[wallToRemove] == true )
                {
                    // Remove the wall and indicate a wall was successfully removed
                    wallList[wallToRemove] = false;
                    wallRemoved = true;
                }
            }
        }
        
        // Print maze after cell 5
        Thread.Sleep( 500 );
        
        // Print maze (duplicate of above print code)
        currentInteriorWall = 0;
        
        // +-+-+-+
        Console.Write( '+' );
        Console.Write( '-' );
        Console.Write( '+' );
        Console.Write( '-' );
        Console.Write( '+' );
        Console.Write( '-' );
        Console.Write( '+' );
        Console.WriteLine();
        
        // Row 1 vertical walls and cells
        Console.Write( '|' );
        Console.Write( ' ' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '|' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( ' ' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '|' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( ' ' );
        Console.Write( '|' );
        Console.WriteLine();
        
        // Row 2 horizontal walls
        Console.Write( '+' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '-' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( '+' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '-' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( '+' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '-' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( '+' );
        Console.WriteLine();
        
        // Row 2 vertical walls and cells
        Console.Write( '|' );
        Console.Write( ' ' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '|' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( ' ' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '|' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( ' ' );
        Console.Write( '|' );
        Console.WriteLine();
        
        // Row 3 horizontal walls
        Console.Write( '+' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '-' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( '+' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '-' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( '+' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '-' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( '+' );
        Console.WriteLine();
        
        // Row 3 vertical walls and cells
        Console.Write( '|' );
        Console.Write( ' ' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '|' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( ' ' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '|' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( ' ' );
        Console.Write( '|' );
        Console.WriteLine();
        
        // Bottom border
        Console.Write( '+' );
        Console.Write( '-' );
        Console.Write( '+' );
        Console.Write( '-' );
        Console.Write( '+' );
        Console.Write( '-' );
        Console.Write( '+' );
        Console.WriteLine();

        // Cell 6
        Console.WriteLine( "\nRemoving wall from cell 6:" );
        
        // Remove wall code:
        // Remove a cell wall if possible for cell 6
        cellIndex = 6;
        
        wallRemoved = false;
        direction = LEFT;
        
        // If the wall in this direction is NOT an exterior wall
        if ( cellToWallLUT[cellIndex, direction] >= 0 )
        {
            wallToRemove = cellToWallLUT[cellIndex, direction];
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
            if ( cellToWallLUT[cellIndex, direction] >= 0 )
            {
                wallToRemove = cellToWallLUT[cellIndex, direction];
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
            if ( cellToWallLUT[cellIndex, direction] >= 0 )
            {
                wallToRemove = cellToWallLUT[cellIndex, direction];
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
            if ( cellToWallLUT[cellIndex, direction] >= 0 )
            {
                wallToRemove = cellToWallLUT[cellIndex, direction];
                // If this wall has not been already removed
                if ( wallList[wallToRemove] == true )
                {
                    // Remove the wall and indicate a wall was successfully removed
                    wallList[wallToRemove] = false;
                    wallRemoved = true;
                }
            }
        }
        
        // Print maze after cell 6
        Thread.Sleep( 500 );
        
        // Print maze (duplicate of above print code)
        currentInteriorWall = 0;
        
        // +-+-+-+
        Console.Write( '+' );
        Console.Write( '-' );
        Console.Write( '+' );
        Console.Write( '-' );
        Console.Write( '+' );
        Console.Write( '-' );
        Console.Write( '+' );
        Console.WriteLine();
        
        // Row 1 vertical walls and cells
        Console.Write( '|' );
        Console.Write( ' ' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '|' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( ' ' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '|' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( ' ' );
        Console.Write( '|' );
        Console.WriteLine();
        
        // Row 2 horizontal walls
        Console.Write( '+' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '-' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( '+' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '-' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( '+' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '-' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( '+' );
        Console.WriteLine();
        
        // Row 2 vertical walls and cells
        Console.Write( '|' );
        Console.Write( ' ' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '|' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( ' ' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '|' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( ' ' );
        Console.Write( '|' );
        Console.WriteLine();
        
        // Row 3 horizontal walls
        Console.Write( '+' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '-' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( '+' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '-' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( '+' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '-' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( '+' );
        Console.WriteLine();
        
        // Row 3 vertical walls and cells
        Console.Write( '|' );
        Console.Write( ' ' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '|' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( ' ' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '|' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( ' ' );
        Console.Write( '|' );
        Console.WriteLine();
        
        // Bottom border
        Console.Write( '+' );
        Console.Write( '-' );
        Console.Write( '+' );
        Console.Write( '-' );
        Console.Write( '+' );
        Console.Write( '-' );
        Console.Write( '+' );
        Console.WriteLine();

        // Cell 7
        Console.WriteLine( "\nRemoving wall from cell 7:" );
        
        // Remove wall code:
        // Remove a cell wall if possible for cell 7
        cellIndex = 7;
        
        wallRemoved = false;
        direction = LEFT;
        
        // If the wall in this direction is NOT an exterior wall
        if ( cellToWallLUT[cellIndex, direction] >= 0 )
        {
            wallToRemove = cellToWallLUT[cellIndex, direction];
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
            if ( cellToWallLUT[cellIndex, direction] >= 0 )
            {
                wallToRemove = cellToWallLUT[cellIndex, direction];
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
            if ( cellToWallLUT[cellIndex, direction] >= 0 )
            {
                wallToRemove = cellToWallLUT[cellIndex, direction];
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
            if ( cellToWallLUT[cellIndex, direction] >= 0 )
            {
                wallToRemove = cellToWallLUT[cellIndex, direction];
                // If this wall has not been already removed
                if ( wallList[wallToRemove] == true )
                {
                    // Remove the wall and indicate a wall was successfully removed
                    wallList[wallToRemove] = false;
                    wallRemoved = true;
                }
            }
        }
        
        // Print maze after cell 7
        Thread.Sleep( 500 );
        
        // Print maze (duplicate of above print code)
        currentInteriorWall = 0;
        
        // +-+-+-+
        Console.Write( '+' );
        Console.Write( '-' );
        Console.Write( '+' );
        Console.Write( '-' );
        Console.Write( '+' );
        Console.Write( '-' );
        Console.Write( '+' );
        Console.WriteLine();
        
        // Row 1 vertical walls and cells
        Console.Write( '|' );
        Console.Write( ' ' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '|' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( ' ' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '|' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( ' ' );
        Console.Write( '|' );
        Console.WriteLine();
        
        // Row 2 horizontal walls
        Console.Write( '+' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '-' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( '+' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '-' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( '+' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '-' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( '+' );
        Console.WriteLine();
        
        // Row 2 vertical walls and cells
        Console.Write( '|' );
        Console.Write( ' ' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '|' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( ' ' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '|' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( ' ' );
        Console.Write( '|' );
        Console.WriteLine();
        
        // Row 3 horizontal walls
        Console.Write( '+' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '-' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( '+' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '-' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( '+' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '-' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( '+' );
        Console.WriteLine();
        
        // Row 3 vertical walls and cells
        Console.Write( '|' );
        Console.Write( ' ' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '|' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( ' ' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '|' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( ' ' );
        Console.Write( '|' );
        Console.WriteLine();
        
        // Bottom border
        Console.Write( '+' );
        Console.Write( '-' );
        Console.Write( '+' );
        Console.Write( '-' );
        Console.Write( '+' );
        Console.Write( '-' );
        Console.Write( '+' );
        Console.WriteLine();

        // Cell 8
        Console.WriteLine( "\nRemoving wall from cell 8:" );
        
        // Remove wall code:
        // Remove a cell wall if possible for cell 8
        cellIndex = 8;
        
        wallRemoved = false;
        direction = LEFT;
        
        // If the wall in this direction is NOT an exterior wall
        if ( cellToWallLUT[cellIndex, direction] >= 0 )
        {
            wallToRemove = cellToWallLUT[cellIndex, direction];
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
            if ( cellToWallLUT[cellIndex, direction] >= 0 )
            {
                wallToRemove = cellToWallLUT[cellIndex, direction];
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
            if ( cellToWallLUT[cellIndex, direction] >= 0 )
            {
                wallToRemove = cellToWallLUT[cellIndex, direction];
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
            if ( cellToWallLUT[cellIndex, direction] >= 0 )
            {
                wallToRemove = cellToWallLUT[cellIndex, direction];
                // If this wall has not been already removed
                if ( wallList[wallToRemove] == true )
                {
                    // Remove the wall and indicate a wall was successfully removed
                    wallList[wallToRemove] = false;
                    wallRemoved = true;
                }
            }
        }
        
        // Print maze after cell 8
        Thread.Sleep( 500 );
        
        // Print maze (duplicate of above print code)
        currentInteriorWall = 0;
        
        // +-+-+-+
        Console.Write( '+' );
        Console.Write( '-' );
        Console.Write( '+' );
        Console.Write( '-' );
        Console.Write( '+' );
        Console.Write( '-' );
        Console.Write( '+' );
        Console.WriteLine();
        
        // Row 1 vertical walls and cells
        Console.Write( '|' );
        Console.Write( ' ' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '|' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( ' ' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '|' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( ' ' );
        Console.Write( '|' );
        Console.WriteLine();
        
        // Row 2 horizontal walls
        Console.Write( '+' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '-' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( '+' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '-' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( '+' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '-' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( '+' );
        Console.WriteLine();
        
        // Row 2 vertical walls and cells
        Console.Write( '|' );
        Console.Write( ' ' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '|' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( ' ' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '|' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( ' ' );
        Console.Write( '|' );
        Console.WriteLine();
        
        // Row 3 horizontal walls
        Console.Write( '+' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '-' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( '+' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '-' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( '+' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '-' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( '+' );
        Console.WriteLine();
        
        // Row 3 vertical walls and cells
        Console.Write( '|' );
        Console.Write( ' ' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '|' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( ' ' );
        if ( wallList[currentInteriorWall] == true )
        {
            Console.Write( '|' );
        }
        else
        {
            Console.Write( ' ' );
        }
        currentInteriorWall = currentInteriorWall + 1;
        Console.Write( ' ' );
        Console.Write( '|' );
        Console.WriteLine();
        
        // Bottom border
        Console.Write( '+' );
        Console.Write( '-' );
        Console.Write( '+' );
        Console.Write( '-' );
        Console.Write( '+' );
        Console.Write( '-' );
        Console.Write( '+' );
        Console.WriteLine();
    }
}
