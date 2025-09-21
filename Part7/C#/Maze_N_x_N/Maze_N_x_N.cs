using System;
using System.Threading;

class Program
{
    const int FIRST_CELL = 0;
    const int SECOND_CELL = 1;
    const int NO_CELL = -1;

    static void Main()
    {
        // Prompt the user for maze size
        int mazeColumns = 0;
        while (mazeColumns <= 0)
        {
            Console.Write("Please enter number of columns for maze, must be greater than 1: ");
            string userInput = Console.ReadLine();

            if (int.TryParse(userInput, out int result))
            {
                mazeColumns = result;
            }
        }

        int mazeRows = 0;
        while (mazeRows <= 0)
        {
            Console.Write("Please enter number of rows for maze, must be greater than 1: ");
            string userInput = Console.ReadLine();

            if (int.TryParse(userInput, out int result))
            {
                mazeRows = result;
            }
        }

        int interiorWallCount = mazeRows * (mazeColumns - 1) + (mazeRows - 1) * mazeColumns;

        // Start with all the walls up
        bool[] wallsUp = new bool[interiorWallCount];
        for (int i = 0; i < interiorWallCount; i++)
        {
            wallsUp[i] = true;
        }

        // Identify the cells each wall connects
        int[,] wallConnections = new int[interiorWallCount, 2];

        int wallIndex = 0;
        for (int rowIndex = 0; rowIndex < mazeRows; rowIndex++)
        {
            int firstCellInRow = rowIndex * mazeColumns;

            // Vertical walls
            for (int verticalWallIndex = 0; verticalWallIndex < mazeColumns - 1; verticalWallIndex++)
            {
                int leftCell = firstCellInRow + verticalWallIndex;
                int rightCell = leftCell + 1;
                wallConnections[wallIndex, FIRST_CELL] = leftCell;
                wallConnections[wallIndex, SECOND_CELL] = rightCell;
                wallIndex++;
            }

            // Horizontal walls
            if (wallIndex < interiorWallCount)
            {
                for (int horizontalWallIndex = 0; horizontalWallIndex < mazeColumns; horizontalWallIndex++)
                {
                    int upperCell = firstCellInRow + horizontalWallIndex;
                    int lowerCell = upperCell + mazeColumns;
                    wallConnections[wallIndex, FIRST_CELL] = upperCell;
                    wallConnections[wallIndex, SECOND_CELL] = lowerCell;
                    wallIndex++;
                }
            }
        }

        // Identify which group each cell is a part of
        int[] cellToGroup = new int[mazeRows * mazeColumns];
        for (int cellIndex = 0; cellIndex < mazeRows * mazeColumns; cellIndex++)
        {
            cellToGroup[cellIndex] = cellIndex;
        }

        // Identify which cells are a part of each group
        int[,] groupCells = new int[mazeColumns * mazeRows, mazeColumns * mazeRows];
        for (int cellIndex = 0; cellIndex < mazeRows * mazeColumns; cellIndex++)
        {
            for (int innerCellIndex = 0; innerCellIndex < mazeRows * mazeColumns; innerCellIndex++)
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

        // Print initial maze
        int currentInteriorWall = 0;

        // Print top border
        Console.Write("+");
        for (int i = 0; i < mazeColumns; i++)
        {
            Console.Write("-+");
        }
        Console.WriteLine();

        for (int rowIndex = 0; rowIndex < mazeRows; rowIndex++)
        {
            // Print vertical walls and cells
            Console.Write("|");
            for (int columnIndex = 0; columnIndex < mazeColumns; columnIndex++)
            {
                Console.Write(" ");

                if (columnIndex == mazeColumns - 1 || wallsUp[currentInteriorWall])
                {
                    Console.Write("|");
                }
                else
                {
                    Console.Write(" ");
                }

                if (columnIndex < mazeColumns - 1)
                {
                    currentInteriorWall++;
                }
            }
            Console.WriteLine();

            // Print horizontal walls
            if (rowIndex < mazeRows - 1)
            {
                Console.Write("+");
                for (int columnIndex = 0; columnIndex < mazeColumns; columnIndex++)
                {
                    if (wallsUp[currentInteriorWall])
                    {
                        Console.Write("-");
                    }
                    else
                    {
                        Console.Write(" ");
                    }
                    Console.Write("+");
                    currentInteriorWall++;
                }
                Console.WriteLine();
            }
        }

        // Print bottom border
        Console.Write("+");
        for (int i = 0; i < mazeColumns; i++)
        {
            Console.Write("-+");
        }
        Console.WriteLine();

        // Create wall removal list
        int[] wallRemoveList = new int[interiorWallCount];
        for (int i = 0; i < interiorWallCount; i++)
        {
            wallRemoveList[i] = i;
        }

        // Seed random number generator
        Random random = new Random();

        // Fisher-Yates shuffle algorithm
        for (int shuffleIndex = interiorWallCount - 1; shuffleIndex > 0; shuffleIndex--)
        {
            int otherIndex = random.Next(shuffleIndex + 1);
            int temp = wallRemoveList[shuffleIndex];
            wallRemoveList[shuffleIndex] = wallRemoveList[otherIndex];
            wallRemoveList[otherIndex] = temp;
        }

        bool mazeComplete = false;

        // Kruskal's algorithm
        for (int removeWallIndex = 0; removeWallIndex < interiorWallCount; removeWallIndex++)
        {
            int nextWallToCheck = wallRemoveList[removeWallIndex];

            int firstCell = wallConnections[nextWallToCheck, FIRST_CELL];
            int firstCellGroupIndex = cellToGroup[firstCell];
            int secondCell = wallConnections[nextWallToCheck, SECOND_CELL];
            int secondCellGroupIndex = cellToGroup[secondCell];

            if (firstCellGroupIndex != secondCellGroupIndex)
            {
                wallsUp[nextWallToCheck] = false;

                // Find next empty position in first group
                int nextEmptyFirstGroupIndex = 0;
                for (int cellIndex = 0; cellIndex < mazeColumns * mazeRows; cellIndex++)
                {
                    if (groupCells[firstCellGroupIndex, cellIndex] == NO_CELL)
                    {
                        nextEmptyFirstGroupIndex = cellIndex;
                        break;
                    }
                }

                // Move all cells from second group to first group
                for (int groupCellIndex = mazeColumns * mazeRows - 1; groupCellIndex >= 0; groupCellIndex--)
                {
                    if (groupCells[secondCellGroupIndex, groupCellIndex] != NO_CELL)
                    {
                        int cellToMove = groupCells[secondCellGroupIndex, groupCellIndex];

                        groupCells[firstCellGroupIndex, nextEmptyFirstGroupIndex] = cellToMove;
                        nextEmptyFirstGroupIndex++;
                        cellToGroup[cellToMove] = firstCellGroupIndex;
                        groupCells[secondCellGroupIndex, groupCellIndex] = NO_CELL;

                        if (nextEmptyFirstGroupIndex >= mazeColumns * mazeRows)
                        {
                            mazeComplete = true;
                        }
                    }
                }

                Thread.Sleep(500);

                // Copy in Print maze code from above again here:
                currentInteriorWall = 0;

                Console.WriteLine();
                Console.Write("+");
                for (int cellIndex = 0; cellIndex < mazeColumns; cellIndex++)
                {
                    Console.Write("-+");
                }
                Console.WriteLine();

                for (int rowIndex = 0; rowIndex < mazeRows; rowIndex++)
                {
                    Console.Write("|");
                    for (int columnIndex = 0; columnIndex < mazeColumns; columnIndex++)
                    {
                        Console.Write(" ");

                        if (columnIndex == mazeColumns - 1 || wallsUp[currentInteriorWall])
                        {
                            Console.Write("|");
                        }
                        else
                        {
                            Console.Write(" ");
                        }

                        if (columnIndex < mazeColumns - 1)
                        {
                            currentInteriorWall++;
                        }
                    }
                    Console.WriteLine();

                    if (rowIndex < mazeRows - 1)
                    {
                        Console.Write("+");
                        for (int columnIndex = 0; columnIndex < mazeColumns; columnIndex++)
                        {
                            if (wallsUp[currentInteriorWall])
                            {
                                Console.Write("-");
                            }
                            else
                            {
                                Console.Write(" ");
                            }
                            Console.Write("+");
                            currentInteriorWall++;
                        }
                        Console.WriteLine();
                    }
                }

                Console.Write("+");
                for (int columnIndex = 0; columnIndex < mazeColumns; columnIndex++)
                {
                    Console.Write("-+");
                }
                Console.WriteLine();

                if (mazeComplete)
                {
                    break;
                }
            }
        }
    }
}
