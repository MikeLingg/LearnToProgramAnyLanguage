using System;
using System.Threading;

class Program
{
    static void Main()
    {
        // Define directions
        //const int LEFT = 0;
        //const int UP = 1;
        //const int RIGHT = 2;
        //const int DOWN = 3;

        const int FIRST_CELL = 0;
        const int SECOND_CELL = 1;

        const int NO_CELL = -1;

        const int interiorWallCount = 12;

        // For each cell 0-8, indicate if a wall is exterior and cannot be removed ( -1 ) or its interior index for each of
        // the four directions, LEFT, UP, RIGHT, DOWN.
        /*int[,] cellToWallLUT = new int[9, 4] {
            {-1, -1, 0, 2},  {0, -1, 1, 3},   {1, -1, -1, 4},
            {-1, 2, 5, 7},   {5, 3, 6, 8},    {6, 4, -1, 9},
            {-1, 7, 10, -1}, {10, 8, 11, -1}, {11, 9, -1, -1}
        };*/

        // 12 interior walls in a 3x3 maze. Start with all the walls up.
        bool[] wallsUp = new bool[12] { true, true, true, true, true, true, true, true, true, true, true, true };

        // Identify the cells each wall connects.
        int[,] wallConnections = new int[12, 2]
        {
            { 0, 1 }, { 1, 2 }, { 0, 3 }, { 1, 4 }, { 2, 5 }, { 3, 4 },
            { 4, 5 }, { 3, 6 }, { 4, 7 }, { 5, 8 }, { 6, 7 }, { 7, 8 }
        };

        // Identify which group each cell is a part of.
        int[] cellToGroup = new int[9] { 0, 1, 2, 3, 4, 5, 6, 7, 8 };

        // Identify which cells are a part of each group
        int[,] groupCells = new int[9, 9]
        {
            { 0, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL },
            { 1, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL },
            { 2, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL },
            { 3, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL },
            { 4, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL },
            { 5, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL },
            { 6, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL },
            { 7, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL },
            { 8, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL }
        };

        // Print maze code:
        // Print out the maze, this is a less painful copy/paste job without functions, but better with loops.
        int currentInteriorWall = 0;

        // Print the horizontal walls above row 1 - All are exterior walls, no conditions.
        // +-+-+-+
        // One initial cell with + followed by 3 cells with -+
        Console.Write("+");
        for (int cellIndex = 0; cellIndex <= 2; cellIndex++)
        {
            Console.Write("-");
            Console.Write("+");
        }
        Console.WriteLine();

        for (int rowIndex = 0; rowIndex <= 2; rowIndex++)
        {
            // Vertical walls and cells row 1.
            // The left and right vertical walls are exterior, always up.
            // | | | |
            // Or print one |, followed by 3 cells of <space>| where the |
            // may be down ( <space> ).
            Console.Write("|");
            for (int cellIndex = 0; cellIndex <= 2; cellIndex++)
            {
                Console.Write(" ");

                // Always print the right most vertical wall,
                // if interior wall, print if the wall is up.
                if (cellIndex == 2 || wallsUp[currentInteriorWall] == true)
                {
                    Console.Write("|");
                }
                else
                {
                    Console.Write(" ");
                }
                if (cellIndex < 2)
                {
                    currentInteriorWall = currentInteriorWall + 1;
                }
            }
            Console.WriteLine();

            // One fewer horizontal wall than vertical
            if (rowIndex < 2)
            {
                // Horizontal walls above row rowIndex
                // +-+-+-+
                Console.Write("+");
                for (int cellIndex = 0; cellIndex <= 2; cellIndex++)
                {
                    if (wallsUp[currentInteriorWall] == true)
                    {
                        Console.Write("-");
                    }
                    else
                    {
                        Console.Write(" ");
                    }
                    Console.Write("+");
                }
                Console.WriteLine();
            }

            currentInteriorWall = currentInteriorWall + 1;
        }

        // Horizontal walls below row 3 - All are exterior walls, no conditions.
        // +-+-+-+
        Console.Write("+");
        for (int cellIndex = 0; cellIndex <= 2; cellIndex++)
        {
            Console.Write("-");
            Console.Write("+");
        }
        Console.WriteLine();

        // Create and randomize wall removal list
        int[] wallRemoveList = new int[12] { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 };

        Random rand = new Random();

        // Fisher-Yates shuffle algorithm
        for (int shuffleIndex = 11; shuffleIndex > 0; shuffleIndex = shuffleIndex - 1)
        {
            // Generate random index from 0 to shuffleIndex ( inclusive )
            int otherIndex = rand.Next(shuffleIndex + 1);

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
        for (int removeWallIndex = 0; removeWallIndex <= interiorWallCount - 1; removeWallIndex++)
        {
            int nextWallToCheck = wallRemoveList[removeWallIndex];

            // If the two cells connected to this wall are not part 
            // of the same group, remove the wall and merge the 
            // groups.
            int firstCell = wallConnections[nextWallToCheck, FIRST_CELL];
            int firstCellGroupIndex = cellToGroup[firstCell];
            int secondCell = wallConnections[nextWallToCheck, SECOND_CELL];
            int secondCellGroupIndex = cellToGroup[secondCell];

            if (firstCellGroupIndex != secondCellGroupIndex)
            {
                wallsUp[nextWallToCheck] = false;

                // Loop through the indices of all cells in the first 
                // group until we find a NO_CELL indicating no cell here.
                int nextEmptyFirstGroupIndex = 0;
                for (int cellIndex = 0; cellIndex <= 8; cellIndex++)
                {
                    if (groupCells[firstCellGroupIndex, cellIndex] == NO_CELL)
                    {
                        nextEmptyFirstGroupIndex = cellIndex;
                        break;
                    }
                }

                // Loop through the indices of all cells in the second group,
                // move each cell to the first group, and set that cell's 
                // group to the first group index.
                for (int groupCellIndex = 8; groupCellIndex >= 0; groupCellIndex--)
                {
                    // Skip until we reach valid cells
                    if (groupCells[secondCellGroupIndex, groupCellIndex] != NO_CELL)
                    {
                        // Get the id number of the cell to move from 
                        // the second group to the first group
                        int cellToMove = groupCells[secondCellGroupIndex, groupCellIndex];

                        // Move the cell number from the second group 
                        // to the first group
                        groupCells[firstCellGroupIndex, nextEmptyFirstGroupIndex] = cellToMove;
                        // Move our empty index to the next cell in this array.
                        nextEmptyFirstGroupIndex = nextEmptyFirstGroupIndex + 1;
                        // Mark this cell as part of the first group.
                        cellToGroup[cellToMove] = firstCellGroupIndex;
                        // Remove the cell from the second group ( set the
                        // array entry to NO_CELL )
                        groupCells[secondCellGroupIndex, groupCellIndex] = NO_CELL;

                        if (nextEmptyFirstGroupIndex >= 9)
                        {
                            mazeComplete = true;
                        }
                    }
                }

                // Print maze code ( copied from above ):
                Thread.Sleep(500); // Sleep for 500 milliseconds
                currentInteriorWall = 0;

                Console.WriteLine();
                Console.Write("+");
                for (int cellIndex = 0; cellIndex <= 2; cellIndex++)
                {
                    Console.Write("-");
                    Console.Write("+");
                }
                Console.WriteLine();

                for (int rowIndex = 0; rowIndex <= 2; rowIndex++)
                {
                    Console.Write("|");
                    for (int cellIndex = 0; cellIndex <= 2; cellIndex++)
                    {
                        Console.Write(" ");

                        if (cellIndex == 2 || wallsUp[currentInteriorWall] == true)
                        {
                            Console.Write("|");
                        }
                        else
                        {
                            Console.Write(" ");
                        }
                        if (cellIndex < 2)
                        {
                            currentInteriorWall = currentInteriorWall + 1;
                        }
                    }
                    Console.WriteLine();

                    if (rowIndex < 2)
                    {
                        Console.Write("+");
                        for (int cellIndex = 0; cellIndex <= 2; cellIndex++)
                        {
                            if (wallsUp[currentInteriorWall] == true)
                            {
                                Console.Write("-");
                            }
                            else
                            {
                                Console.Write(" ");
                            }
                            Console.Write("+");

                            currentInteriorWall = currentInteriorWall + 1;
                        }
                        Console.WriteLine();
                    }
                }

                Console.Write("+");
                for (int cellIndex = 0; cellIndex <= 2; cellIndex++)
                {
                    Console.Write("-");
                    Console.Write("+");
                }
                Console.WriteLine();

                if (mazeComplete == true)
                {
                    break;
                }
            }
        }
    }
}
