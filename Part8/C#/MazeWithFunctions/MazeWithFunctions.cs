using System;
using System.Threading;



class MazeGenerator
{
    // Define directions
    const int LEFT = 0;
    const int UP = 1;
    const int RIGHT = 2;
    const int DOWN = 3;

    const int FIRST_CELL = 0;
    const int SECOND_CELL = 1;

    const int NO_CELL = -1;



    // Global variables
    static bool[] AllHorizontalWallsUp_Glob;
    static bool[] WallsUp_Glob;
    static int InteriorWallCount_Glob;
    static int MazeRows_Glob;
    static int MazeColumns_Glob;



    // +-+-+-+
    // Prints a horizontal wall, similar to the example above, 
    // with walls down based on the provided parameters.
    // HorizontalWallsUp_Par: Boolean array of size mazeColumns, 
    //     True indicates the wall should be printed as up.
    static void printHorizontalWalls(bool[] HorizontalWallsUp_Par)
    {
        Console.Write("+");
        for (int horizontalWallIndex = 0; horizontalWallIndex < MazeColumns_Glob; horizontalWallIndex++)
        {
            if (HorizontalWallsUp_Par[horizontalWallIndex] == true)
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



    // +-+-+-+
    // Prints a horizontal wall, similar to the example above, with all walls up.
    static void printHorizontalWallsAll()
    {
        printHorizontalWalls(AllHorizontalWallsUp_Glob);
    }



    // | | | |
    // Prints a vertical wall, similar to the example above, 
    // with walls down based on the provided parameters.
    // VerticalWallsUp_Par: Boolean array of size mazeColumns - 1, 
    //     True indicates the wall should be printed as up.
    static void printVerticalWalls(bool[] VerticalWallsUp_Par)
    {
        // First wall is an exterior wall, always up.
        Console.Write("|");
        for (int verticalWallIndex = 0; verticalWallIndex < MazeColumns_Glob - 1; verticalWallIndex++)
        {
            Console.Write(" ");
            if (VerticalWallsUp_Par[verticalWallIndex] == true)
            {
                Console.Write("|");
            }
            else
            {
                Console.Write(" ");
            }
        }
        // Last wall exterior, always up.
        Console.Write(" ");
        Console.Write("|");
        Console.WriteLine();
    }



    // Loop through the rows of the maze and print the maze 
    // based on WallsUp_Glob
    static void printMaze()
    {
        int interiorWallIndex = 0;

        // First row is exterior walls
        printHorizontalWallsAll();
        for (int rowIndex = 0; rowIndex < MazeRows_Glob; rowIndex++)
        {
            bool[] verticalWallsUp = new bool[MazeColumns_Glob - 1];
            for (int columnIndex = 0; columnIndex < MazeColumns_Glob - 1; columnIndex++)
            {
                verticalWallsUp[columnIndex] = WallsUp_Glob[interiorWallIndex];
                interiorWallIndex = interiorWallIndex + 1;
            }

            printVerticalWalls(verticalWallsUp);

            if (rowIndex == MazeRows_Glob - 1)
            {
                printHorizontalWallsAll();
            }
            else
            {
                bool[] horizontalWallsUp = new bool[MazeColumns_Glob];
                for (int columnIndex = 0; columnIndex < MazeColumns_Glob; columnIndex++)
                {
                    horizontalWallsUp[columnIndex] = WallsUp_Glob[interiorWallIndex];
                    interiorWallIndex = interiorWallIndex + 1;
                }

                printHorizontalWalls(horizontalWallsUp);
            }
        }

        Console.WriteLine();
    }



    // Simple sleep function
    static void sleepHalfSecond()
    {
        Thread.Sleep(500);
    }



    // Kruskal's algorithm.
    // The simple description of the algorithm is first place each 
    // cell in its own group.  Then process all walls in random order,
    // if the cells on either side of the wall are in separate groups, 
    // remove the wall and merge the groups.  Repeat until all 
    // cells are now in the same group.
    static void buildMazeKruskal()
    {
        // Identify the cells each wall connects.
        int[,] wallConnections = new int[InteriorWallCount_Glob, 2];
        int[] wallRemoveList = new int[InteriorWallCount_Glob];
        int[] cellToGroup = new int[MazeRows_Glob * MazeColumns_Glob];
        int[,] groupCells = new int[MazeRows_Glob * MazeColumns_Glob, MazeRows_Glob * MazeColumns_Glob];

        int wallIndex = 0;
        for (int rowIndex = 0; rowIndex < MazeRows_Glob; rowIndex++)
        {
            // Track the first cell in the current row
            int firstCellInRow = rowIndex * MazeColumns_Glob;

            // Note the 0..mazeColumns minus 2, one less vertical wall
            // than the number of columns.
            for (int verticalWallIndex = 0; verticalWallIndex < MazeColumns_Glob - 1; verticalWallIndex++)
            {
                int leftCell = firstCellInRow + verticalWallIndex;
                int rightCell = leftCell + 1;
                wallConnections[wallIndex, FIRST_CELL] = leftCell;
                wallConnections[wallIndex, SECOND_CELL] = rightCell;
                wallIndex = wallIndex + 1;
            }

            // The last row will have no interior horizontal walls below
            // it, so will be skipped.
            if (wallIndex < InteriorWallCount_Glob)
            {
                for (int horizontalWallIndex = 0; horizontalWallIndex < MazeColumns_Glob; horizontalWallIndex++)
                {
                    int upperCell = firstCellInRow + horizontalWallIndex;
                    int lowerCell = upperCell + MazeColumns_Glob;
                    wallConnections[wallIndex, FIRST_CELL] = upperCell;
                    wallConnections[wallIndex, SECOND_CELL] = lowerCell;
                    wallIndex = wallIndex + 1;
                }
            }
        }

        for (int cellIndex = 0; cellIndex < MazeRows_Glob * MazeColumns_Glob; cellIndex++)
        {
            cellToGroup[cellIndex] = cellIndex;

            for (int innerCellIndex = 0; innerCellIndex < MazeRows_Glob * MazeColumns_Glob; innerCellIndex++)
            {
                if (innerCellIndex == 0)
                {
                    groupCells[cellIndex, innerCellIndex] = cellIndex;
                }
                else
                {
                    groupCells[cellIndex, innerCellIndex] = NO_CELL;
                }
            }
        }

        bool mazeComplete = false;

        for (int wallIndexLoop = 0; wallIndexLoop < InteriorWallCount_Glob; wallIndexLoop++)
        {
            wallRemoveList[wallIndexLoop] = wallIndexLoop;
        }

        // Fisher-Yates shuffle
        Random rand = new Random();
        for (int i = InteriorWallCount_Glob - 1; i > 0; i--)
        {
            int j = rand.Next(i + 1);
            int temp = wallRemoveList[i];
            wallRemoveList[i] = wallRemoveList[j];
            wallRemoveList[j] = temp;
        }

        // Perform Kruskal's algorithm.
        for (int removeWallIndex = 0; removeWallIndex < InteriorWallCount_Glob; removeWallIndex++)
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
                WallsUp_Glob[nextWallToCheck] = false;

                // Loop through the indices of all cells in the first 
                // group until we find a NO_CELL indicating no cell here.
                int nextEmptyFirstGroupIndex = 0;
                for (int cellIndex = 0; cellIndex < MazeColumns_Glob * MazeRows_Glob; cellIndex++)
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
                for (int groupCellIndex = MazeColumns_Glob * MazeRows_Glob - 1; groupCellIndex >= 0; groupCellIndex--)
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
                        // Remove the cell from the second group (set the
                        // array entry to NO_CELL)
                        groupCells[secondCellGroupIndex, groupCellIndex] = NO_CELL;

                        if (nextEmptyFirstGroupIndex >= MazeColumns_Glob * MazeRows_Glob)
                        {
                            mazeComplete = true;
                        }
                    }
                }

                sleepHalfSecond();

                printMaze();

                if (mazeComplete == true)
                {
                    break;
                }
            }
        }
    }



    // Note we could use command line arguments instead of user 
    // prompts to get the maze size.
    static void Main(string[] args)
    {
        // Prompt the user for maze size
        MazeColumns_Glob = 0;
        while (MazeColumns_Glob <= 0)
        {
            Console.Write("Please enter number of columns for maze, must be greater than 1: ");
            string userInput = Console.ReadLine();

            if (int.TryParse(userInput, out int tempValue) == true)
            {
                if (tempValue > 1)
                {
                    MazeColumns_Glob = tempValue;
                }
            }
        }

        MazeRows_Glob = 0;
        while (MazeRows_Glob <= 0)
        {
            Console.Write("Please enter number of rows for maze, must be greater than 1: ");
            string userInput = Console.ReadLine();

            if (int.TryParse(userInput, out int tempValue) == true)
            {
                if (tempValue > 1)
                {
                    MazeRows_Glob = tempValue;
                }
            }
        }

        // Setup maze datastructures for the user entered size.
        AllHorizontalWallsUp_Glob = new bool[MazeColumns_Glob];
        for (int i = 0; i < MazeColumns_Glob; i++)
        {
            AllHorizontalWallsUp_Glob[i] = true;
        }

        InteriorWallCount_Glob = MazeRows_Glob * (MazeColumns_Glob - 1) + (MazeRows_Glob - 1) * MazeColumns_Glob;

        WallsUp_Glob = new bool[InteriorWallCount_Glob];
        for (int i = 0; i < InteriorWallCount_Glob; i++)
        {
            WallsUp_Glob[i] = true;
        }

        buildMazeKruskal();
    }
}
