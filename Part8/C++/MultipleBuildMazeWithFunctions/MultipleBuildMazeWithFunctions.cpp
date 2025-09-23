#include <iostream>
#include <cstdlib>
#include <ctime>
#include <cstring>



// Define directions
const int LEFT = 0;
const int UP = 1;
const int RIGHT = 2;
const int DOWN = 3;

const int FIRST_CELL = 0;
const int SECOND_CELL = 1;

const int NO_CELL = -1;

// Define algorithm types
const int KRUSKAL_ALGORITHM = 1;
const int PRIM_ALGORITHM = 2;
const int DEPTH_FIRST_ALGORITHM = 3;
const int BINARY_TREE_ALGORITHM = 4;
const int RECURSIVE_DIVISION_ALGORITHM = 5;



// Global variables
bool* AllHorizontalWallsUp_Glob;
bool* WallsUp_Glob;
int InteriorWallCount_Glob;
int MazeRows_Glob;
int MazeColumns_Glob;

// Additional globals for algorithm state sharing
bool* CellVisited_Glob;
int* FrontierWalls_Glob;
int FrontierWallCount_Glob;
bool* CellInMaze_Glob;

// Lookup tables for wall/cell relationships
int** WallToCells_Glob;
int** CellToWalls_Glob;



// +-+-+-+
// Prints a horizontal wall, similar to the example above, 
// with walls down based on the provided parameters.
// HorizontalWallsUp_Par: Boolean array of size MazeColumns_Glob, 
//     True indicates the wall should be printed as up.
void printHorizontalWalls ( const bool* HorizontalWallsUp_Par )
{
    printf ( "+" );
    for ( int horizontalWallIndex = 0; horizontalWallIndex < MazeColumns_Glob; horizontalWallIndex++ )
    {
        if ( HorizontalWallsUp_Par[horizontalWallIndex] == true )
        {
            printf ( "-" );
        }
        else
        {
            printf ( " " );
        }
        printf ( "+" );
    }
    printf ( "\n" );
}



// +-+-+-+
// Prints a horizontal wall, similar to the example above, with all walls up.
void printHorizontalWalls()
{
    printHorizontalWalls ( AllHorizontalWallsUp_Glob );
}



// | | | |
// Prints a vertical wall, similar to the example above, 
// with walls down based on the provided parameters.
// VerticalWallsUp_Par: Boolean array of size MazeColumns_Glob - 1, 
//     True indicates the wall should be printed as up.
void printVerticalWalls ( const bool* VerticalWallsUp_Par )
{
    // First wall is an exterior wall, always up.
    printf ( "|" );
    for ( int verticalWallIndex = 0; verticalWallIndex < MazeColumns_Glob - 1; verticalWallIndex++ )
    {
        printf ( " " );
        if ( VerticalWallsUp_Par[verticalWallIndex] == true )
        {
            printf ( "|" );
        }
        else
        {
            printf ( " " );
        }
    }
    // Last wall exterior, always up.
    printf ( " " );
    printf ( "|" );
    printf ( "\n" );
}



// Loop through the rows of the maze and print the maze 
// based on WallsUp_Glob
void printMaze()
{
    int interiorWallIndex = 0;

    // First row is exterior walls
    printHorizontalWalls();
    for ( int rowIndex = 0; rowIndex < MazeRows_Glob; rowIndex++ )
    {
        bool verticalWallsUp[MazeColumns_Glob - 1];
        for ( int columnIndex = 0; columnIndex < MazeColumns_Glob - 1; columnIndex++ )
        {
            verticalWallsUp[columnIndex] = WallsUp_Glob[interiorWallIndex];
            interiorWallIndex++;
        }

        printVerticalWalls ( verticalWallsUp );

        if ( rowIndex == MazeRows_Glob - 1 )
        {
            printHorizontalWalls();
        }
        else
        {
            bool horizontalWallsUp[MazeColumns_Glob];
            for ( int columnIndex = 0; columnIndex < MazeColumns_Glob; columnIndex++ )
            {
                horizontalWallsUp[columnIndex] = WallsUp_Glob[interiorWallIndex];
                interiorWallIndex++;
            }
            printHorizontalWalls ( horizontalWallsUp );
        }
    }
}



// Simple sleep function using busy wait
void sleepHalfSecond()
{
    clock_t start = clock();
    while ( ( clock() - start ) < CLOCKS_PER_SEC / 2 );
}



void sleepQuarterSecond()
{
    clock_t start = clock();
    while ( ( clock() - start ) < CLOCKS_PER_SEC / 4 );
}



void sleepTenthSecond()
{
    clock_t start = clock();
    while ( ( clock() - start ) < CLOCKS_PER_SEC / 10 );
}



void sleepOneSecond()
{
    clock_t start = clock();
    while ( ( clock() - start ) < CLOCKS_PER_SEC );
}



// Initialize lookup tables for wall/cell relationships.
// AlgorithmType_Par: Algorithm constant to determine which tables to build
// Must be called after maze dimensions are set.
void initializeLookupTables ( int AlgorithmType_Par )
{
    // Build WallToCells_Glob for algorithms that need wall-to-cell lookups
    if ( AlgorithmType_Par == KRUSKAL_ALGORITHM || AlgorithmType_Par == PRIM_ALGORITHM || AlgorithmType_Par == DEPTH_FIRST_ALGORITHM || AlgorithmType_Par == BINARY_TREE_ALGORITHM || AlgorithmType_Par == RECURSIVE_DIVISION_ALGORITHM )
    {
        int wallIndex = 0;
        for ( int rowIndex = 0; rowIndex < MazeRows_Glob; rowIndex++ )
        {
            // Track the first cell in the current row
            int firstCellInRow = rowIndex * MazeColumns_Glob;

            // Note the 0..MazeColumns_Glob - 2, one less vertical wall than the number of columns.
            for ( int verticalWallIndex = 0; verticalWallIndex < MazeColumns_Glob - 1; verticalWallIndex++ )
            {
                int leftCell = firstCellInRow + verticalWallIndex;
                int rightCell = leftCell + 1;
                WallToCells_Glob[wallIndex][FIRST_CELL] = leftCell;
                WallToCells_Glob[wallIndex][SECOND_CELL] = rightCell;
                wallIndex++;
            }

            // The last row will have no interior horizontal walls below it, so will be skipped.
            if ( wallIndex < InteriorWallCount_Glob )
            {
                for ( int horizontalWallIndex = 0; horizontalWallIndex < MazeColumns_Glob; horizontalWallIndex++ )
                {
                    int upperCell = firstCellInRow + horizontalWallIndex;
                    int lowerCell = upperCell + MazeColumns_Glob;
                    WallToCells_Glob[wallIndex][FIRST_CELL] = upperCell;
                    WallToCells_Glob[wallIndex][SECOND_CELL] = lowerCell;
                    wallIndex++;
                }
            }
        }
    }

    // Build CellToWalls_Glob for algorithms that need cell-to-wall lookups
    if ( AlgorithmType_Par == PRIM_ALGORITHM || AlgorithmType_Par == DEPTH_FIRST_ALGORITHM || AlgorithmType_Par == BINARY_TREE_ALGORITHM || AlgorithmType_Par == RECURSIVE_DIVISION_ALGORITHM )
    {
        for ( int cellIndex = 0; cellIndex < MazeRows_Glob * MazeColumns_Glob; cellIndex++ )
        {
            int cellRow = cellIndex / MazeColumns_Glob;
            int cellCol = cellIndex % MazeColumns_Glob;
            
            // Initialize all directions to NO_CELL ( invalid wall )
            CellToWalls_Glob[cellIndex][LEFT] = NO_CELL;
            CellToWalls_Glob[cellIndex][UP] = NO_CELL;
            CellToWalls_Glob[cellIndex][RIGHT] = NO_CELL;
            CellToWalls_Glob[cellIndex][DOWN] = NO_CELL;
            
            // Find walls by checking which walls connect to this cell
            for ( int wallIndex = 0; wallIndex < InteriorWallCount_Glob; wallIndex++ )
            {
                int firstCellIndex = WallToCells_Glob[wallIndex][FIRST_CELL];
                int secondCellIndex = WallToCells_Glob[wallIndex][SECOND_CELL];
                
                if ( firstCellIndex == cellIndex )
                {
                    // This wall connects from our cell to secondCell
                    int secondRow = secondCellIndex / MazeColumns_Glob;
                    int secondCol = secondCellIndex % MazeColumns_Glob;
                    
                    if ( secondRow == cellRow && secondCol == cellCol + 1 )
                    {
                        // Wall goes RIGHT
                        CellToWalls_Glob[cellIndex][RIGHT] = wallIndex;
                    }
                    else if ( secondRow == cellRow + 1 && secondCol == cellCol )
                    {
                        // Wall goes DOWN
                        CellToWalls_Glob[cellIndex][DOWN] = wallIndex;
                    }
                }
                else if ( secondCellIndex == cellIndex )
                {
                    // This wall connects from firstCellIndex to our cell
                    int firstRow = firstCellIndex / MazeColumns_Glob;
                    int firstCol = firstCellIndex % MazeColumns_Glob;
                    
                    if ( firstRow == cellRow && firstCol == cellCol - 1 )
                    {
                        // Wall comes from LEFT
                        CellToWalls_Glob[cellIndex][LEFT] = wallIndex;
                    }
                    else if ( firstRow == cellRow - 1 && firstCol == cellCol )
                    {
                        // Wall comes from UP
                        CellToWalls_Glob[cellIndex][UP] = wallIndex;
                    }
                }
            }
        }
    }
}



// Add a wall to frontier if not already there.
// WallIndex_Par: Index of wall to add to frontier, must be 0 to InteriorWallCount_Glob-1
void addWallToFrontier ( int WallIndex_Par )
{
    // Check if wall already in frontier
    bool alreadyInFrontier = false;
    for ( int wallIndex = 0; wallIndex < FrontierWallCount_Glob; wallIndex++ )
    {
        if ( FrontierWalls_Glob[wallIndex] == WallIndex_Par )
        {
            alreadyInFrontier = true;
            break;
        }
    }

    if ( alreadyInFrontier == false )
    {
        FrontierWalls_Glob[FrontierWallCount_Glob] = WallIndex_Par;
        FrontierWallCount_Glob++;
    }
}



// Add all walls adjacent to a cell to the frontier list.
// CellIndex_Par: Index of the cell whose adjacent walls should be added to frontier: 0 .. ( MazeRows_Glob * MazeColumns_Glob - 1 )
void addCellWallsToFrontier ( int CellIndex_Par )
{
    // Check all four directions
    if ( CellToWalls_Glob[CellIndex_Par][UP] != NO_CELL )
    {
        addWallToFrontier ( CellToWalls_Glob[CellIndex_Par][UP] );
    }
    if ( CellToWalls_Glob[CellIndex_Par][DOWN] != NO_CELL )
    {
        addWallToFrontier ( CellToWalls_Glob[CellIndex_Par][DOWN] );
    }
    if ( CellToWalls_Glob[CellIndex_Par][LEFT] != NO_CELL )
    {
        addWallToFrontier ( CellToWalls_Glob[CellIndex_Par][LEFT] );
    }
    if ( CellToWalls_Glob[CellIndex_Par][RIGHT] != NO_CELL )
    {
        addWallToFrontier ( CellToWalls_Glob[CellIndex_Par][RIGHT] );
    }
}



// Recursively divide an area with walls.
// StartRow_Par: Starting row of area to divide, must be 0 to MazeRows_Glob-1
// EndRow_Par: Ending row of area to divide, must be StartRow_Par to MazeRows_Glob-1
// StartCol_Par: Starting column of area to divide, must be 0 to MazeColumns_Glob-1
// EndCol_Par: Ending column of area to divide, must be StartCol_Par to MazeColumns_Glob-1
void divideArea ( int StartRow_Par, int EndRow_Par, int StartCol_Par, int EndCol_Par )
{
    int height = EndRow_Par - StartRow_Par + 1;
    int width = EndCol_Par - StartCol_Par + 1;

    // Base case - area too small to divide ( need at least 2x2 to create a wall )
    if ( height < 2 && width < 2 )
    {
        return;
    }

    // Choose whether to divide horizontally or vertically
    bool divideHorizontally = false;
    if ( height > width )
    {
        divideHorizontally = true;
    }
    else if ( width > height )
    {
        divideHorizontally = false;
    }
    else
    {
        // Square area - choose randomly
        divideHorizontally = ( rand() % 2 == 0 );
    }

    if ( divideHorizontally == true && height > 1 )
    {
        // Choose random row to divide on
        int divideRow = StartRow_Par + rand() % ( EndRow_Par - StartRow_Par );

        // Add horizontal wall
        for ( int wallCol = StartCol_Par; wallCol <= EndCol_Par; wallCol++ )
        {
            int cellIndex = divideRow * MazeColumns_Glob + wallCol;
            if ( cellIndex < MazeRows_Glob * MazeColumns_Glob )
            {
                int wallIndex = CellToWalls_Glob[cellIndex][DOWN];
                if ( wallIndex != NO_CELL )
                {
                    WallsUp_Glob[wallIndex] = true;
                }
            }
        }

        // Choose random gap in the wall
        int gapCol = StartCol_Par + rand() % ( EndCol_Par - StartCol_Par + 1 );
        int gapCellIndex = divideRow * MazeColumns_Glob + gapCol;
        if ( gapCellIndex < MazeRows_Glob * MazeColumns_Glob )
        {
            int gapWallIndex = CellToWalls_Glob[gapCellIndex][DOWN];
            if ( gapWallIndex != NO_CELL )
            {
                WallsUp_Glob[gapWallIndex] = false;
            }
        }

        sleepHalfSecond();
        printMaze();

        // Recursively divide the two areas
        divideArea ( StartRow_Par, divideRow, StartCol_Par, EndCol_Par );
        divideArea ( divideRow + 1, EndRow_Par, StartCol_Par, EndCol_Par );
    }
    else if ( divideHorizontally == false && width > 1 )
    {
        // Choose random column to divide on
        int divideCol = StartCol_Par + rand() % ( EndCol_Par - StartCol_Par );

        // Add vertical wall
        for ( int cellRow = StartRow_Par; cellRow <= EndRow_Par; cellRow++ )
        {
            int cellIndex = cellRow * MazeColumns_Glob + divideCol;
            if ( cellIndex < MazeRows_Glob * MazeColumns_Glob )
            {
                int wallIndex = CellToWalls_Glob[cellIndex][RIGHT];
                if ( wallIndex != NO_CELL )
                {
                    WallsUp_Glob[wallIndex] = true;
                }
            }
        }

        // Choose random gap in the wall
        int gapRow = StartRow_Par + rand() % ( EndRow_Par - StartRow_Par + 1 );
        int gapCellIndex = gapRow * MazeColumns_Glob + divideCol;
        if ( gapCellIndex < MazeRows_Glob * MazeColumns_Glob )
        {
            int gapWallIndex = CellToWalls_Glob[gapCellIndex][RIGHT];
            if ( gapWallIndex != NO_CELL )
            {
                WallsUp_Glob[gapWallIndex] = false;
            }
        }

        sleepHalfSecond();
        printMaze();

        // Recursively divide the two areas
        divideArea ( StartRow_Par, EndRow_Par, StartCol_Par, divideCol );
        divideArea ( StartRow_Par, EndRow_Par, divideCol + 1, EndCol_Par );
    }
}



// Kruskal's algorithm.
// The simple description of the algorithm is first place each 
// cell in its own group.  Then process all walls in random order,
// if the cells on either side of the wall are in separate groups, 
// remove the wall and merge the groups.  Repeat until all 
// cells are now in the same group.
void buildMazeKruskal()
{
    // Identify which cells are a part of each group
    // Note each array is large enough to fit all cells,
    // NO_CELL indicates no cell is assigned to this index.
    // Languages like python that make adding/removing 
    // array items easy will not need this, for C arrays
    // we will discuss reallocation or linked list in 
    // later videos for covering changing array sizes.
    int* cellToGroup = new int[MazeRows_Glob * MazeColumns_Glob];
    int** groupCells = new int*[MazeRows_Glob * MazeColumns_Glob];
    for ( int i = 0; i < MazeRows_Glob * MazeColumns_Glob; i++ )
    {
        groupCells[i] = new int[MazeRows_Glob * MazeColumns_Glob];
    }

    for ( int cellIndex = 0; cellIndex < MazeRows_Glob * MazeColumns_Glob; cellIndex++ )
    {
        cellToGroup[cellIndex] = cellIndex;

        for ( int innerCellIndex = 0; innerCellIndex < MazeRows_Glob * MazeColumns_Glob; innerCellIndex++ )
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

    bool mazeComplete = false;

    int* wallRemoveList = new int[InteriorWallCount_Glob];
    for ( int wallIndex = 0; wallIndex < InteriorWallCount_Glob; wallIndex++ )
    {
        wallRemoveList[wallIndex] = wallIndex;
    }
    
    // Fisher-Yates shuffle
    for ( int arrayIndex = InteriorWallCount_Glob - 1; arrayIndex >= 1; arrayIndex-- )
    {
        int randomizedIndex = rand() % ( arrayIndex + 1 );
        int temp = wallRemoveList[arrayIndex];
        wallRemoveList[arrayIndex] = wallRemoveList[randomizedIndex];
        wallRemoveList[randomizedIndex] = temp;
    }

    // Perform Kruskal's algorithm.
    for ( int removeWallIndex = 0; removeWallIndex < InteriorWallCount_Glob; removeWallIndex++ )
    {
        int nextWallToCheck = wallRemoveList[removeWallIndex];

        // If the two cells connected to this wall are not part 
        // of the same group, remove the wall and merge the 
        // groups.
        int firstCellIndex = WallToCells_Glob[nextWallToCheck][FIRST_CELL];
        int firstCellGroupIndex = cellToGroup[firstCellIndex];
        int secondCellIndex = WallToCells_Glob[nextWallToCheck][SECOND_CELL];
        int secondCellGroupIndex = cellToGroup[secondCellIndex];
        
        if ( firstCellGroupIndex != secondCellGroupIndex )
        {
            WallsUp_Glob[nextWallToCheck] = false;

            // Loop through the indices of all cells in the first group until we find a NO_CELL
            int nextEmptyFirstGroupIndex = 0;
            for ( int cellIndex = 0; cellIndex < MazeColumns_Glob * MazeRows_Glob; cellIndex++ )
            {
                if ( groupCells[firstCellGroupIndex][cellIndex] == NO_CELL )
                {
                    nextEmptyFirstGroupIndex = cellIndex;
                    break;
                }
            }

            // Loop through the indices of all cells in the second group,
            // move each cell to the first group, and set that cell's group to the first group index.
            for ( int groupCellIndex = MazeColumns_Glob * MazeRows_Glob - 1; groupCellIndex >= 0; groupCellIndex-- )
            {
                // Skip until we reach valid cells
                if ( groupCells[secondCellGroupIndex][groupCellIndex] != NO_CELL )
                {
                    // Get the id number of the cell to move from the second group to the first group
                    int cellToMove = groupCells[secondCellGroupIndex][groupCellIndex];

                    // Move the cell number from the second group to the first group
                    groupCells[firstCellGroupIndex][nextEmptyFirstGroupIndex] = cellToMove;
                    // Move our empty index to the next cell in this array.
                    nextEmptyFirstGroupIndex++;
                    // Mark this cell as part of the first group.
                    cellToGroup[cellToMove] = firstCellGroupIndex;
                    // Remove the cell from the second group ( set the array entry to NO_CELL )
                    groupCells[secondCellGroupIndex][groupCellIndex] = NO_CELL;
                    
                    if ( nextEmptyFirstGroupIndex >= MazeColumns_Glob * MazeRows_Glob )
                    {
                        mazeComplete = true;
                    }
                }
            }

            sleepHalfSecond();
            printMaze();

            if ( mazeComplete == true )
            {
                break;
            }
        }
    }

    // Clean up memory
    delete[] cellToGroup;
    for ( int i = 0; i < MazeRows_Glob * MazeColumns_Glob; i++ )
    {
        delete[] groupCells[i];
    }
    delete[] groupCells;
    delete[] wallRemoveList;
}



// Prim's algorithm for maze generation.
// Start with a random cell, mark it as part of the maze.
// Repeatedly pick a random wall from cells in the maze that 
// connects to a cell not in the maze, remove the wall and 
// add the new cell to the maze.
void buildMazePrim()
{
    // Initialize algorithm state
    FrontierWallCount_Glob = 0;

    // Track how many cells have been added to the maze
    int cellsInMaze = 0;

    // Start with a random cell
    int startCell = rand() % ( MazeRows_Glob * MazeColumns_Glob );
    CellInMaze_Glob[startCell] = true;
    cellsInMaze++;

    // Add all walls adjacent to the start cell to frontier
    addCellWallsToFrontier ( startCell );

    // Continue until all cells are in the maze
    while ( cellsInMaze < MazeRows_Glob * MazeColumns_Glob )
    {
        // Pick a random wall from frontier
        int randomWallIndex = rand() % FrontierWallCount_Glob;
        int wallToCheck = FrontierWalls_Glob[randomWallIndex];

        // Remove this wall from frontier list by replacing it with the last wall in the list
        FrontierWalls_Glob[randomWallIndex] = FrontierWalls_Glob[FrontierWallCount_Glob - 1];
        FrontierWallCount_Glob--;

        // Get the two cells this wall connects
        int firstCellIndex = WallToCells_Glob[wallToCheck][FIRST_CELL];
        int secondCellIndex = WallToCells_Glob[wallToCheck][SECOND_CELL];

        // If one cell is already in the maze and the other is not, remove the wall to connect
        if ( CellInMaze_Glob[firstCellIndex] != CellInMaze_Glob[secondCellIndex] )
        {
            WallsUp_Glob[wallToCheck] = false;

            int outerCellIndex = -1;

            // Add the outside cell to the maze
            if ( CellInMaze_Glob[firstCellIndex] == false )
            {
                outerCellIndex = firstCellIndex;
            }
            else
            {
                outerCellIndex = secondCellIndex;
            }

            CellInMaze_Glob[outerCellIndex] = true;
            cellsInMaze++;
            addCellWallsToFrontier ( outerCellIndex );

            sleepHalfSecond();
            printMaze();
        }
    }
}



// Depth-first search maze generation using recursive backtracking.
// Start at a random cell, randomly walk to neighbors outside 
// the maze,removing walls as you go. 
// When all neighbors are in the maze, backtrack to a cell 
// with neighbors outside the maze.
void buildMazeDepthFirst()
{
    // Stack for backtracking - store cell indices
    int* cellStack = new int[MazeRows_Glob * MazeColumns_Glob];
    int stackSize = 0;

    // Start with random cell
    int currentCellIndex = rand() % ( MazeRows_Glob * MazeColumns_Glob );
    CellVisited_Glob[currentCellIndex] = true;

    // Push starting cell onto stack
    cellStack[stackSize] = currentCellIndex;
    stackSize++;

    while ( stackSize > 0 )
    {
        // Create randomized direction list
        int randomizedDirections[4] = { LEFT, UP, RIGHT, DOWN };
        
        // Fisher-Yates shuffle the directions
        for ( int arrayIndex = 3; arrayIndex >= 1; arrayIndex-- )
        {
            int randomizedIndex = rand() % ( arrayIndex + 1 );
            int temp = randomizedDirections[arrayIndex];
            randomizedDirections[arrayIndex] = randomizedDirections[randomizedIndex];
            randomizedDirections[randomizedIndex] = temp;
        }

        bool foundNeighbor = false;
        int nextCellIndex = NO_CELL;
        int wallIndex = NO_CELL;

        // Check directions in random order until we find an unvisited neighbor
        for ( int directionIndex = 0; directionIndex <= 3; directionIndex++ )
        {
            int direction = randomizedDirections[directionIndex];
            wallIndex = CellToWalls_Glob[currentCellIndex][direction];
            if ( wallIndex != NO_CELL )
            {
                // Find the cell on the other side of this wall
                int firstCellIndex = WallToCells_Glob[wallIndex][FIRST_CELL];
                int secondCellIndex = WallToCells_Glob[wallIndex][SECOND_CELL];
                
                if ( firstCellIndex == currentCellIndex )
                {
                    nextCellIndex = secondCellIndex;
                }
                else
                {
                    nextCellIndex = firstCellIndex;
                }
                
                // Check if neighbor is unvisited
                if ( CellVisited_Glob[nextCellIndex] == false )
                {
                    foundNeighbor = true;
                    break;
                }
            }
        }

        if ( foundNeighbor == true )
        {
            // Find wall between current and next cell using lookup tables
            wallIndex = NO_CELL;
            for ( int direction = 0; direction <= 3; direction++ )
            {
                int wallCandidate = CellToWalls_Glob[currentCellIndex][direction];
                if ( wallCandidate != NO_CELL )
                {
                    int firstCellIndex = WallToCells_Glob[wallCandidate][FIRST_CELL];
                    int secondCellIndex = WallToCells_Glob[wallCandidate][SECOND_CELL];
                    if ( ( firstCellIndex == currentCellIndex && secondCellIndex == nextCellIndex ) || ( firstCellIndex == nextCellIndex && secondCellIndex == currentCellIndex ) )
                    {
                        wallIndex = wallCandidate;
                        break;
                    }
                }
            }
            WallsUp_Glob[wallIndex] = false;

            // Mark next cell as visited
            CellVisited_Glob[nextCellIndex] = true;

            // Push next cell onto stack
            cellStack[stackSize] = nextCellIndex;
            stackSize++;

            currentCellIndex = nextCellIndex;

            sleepQuarterSecond();
            printMaze();
        }
        else
        {
            // Backtrack - pop from stack
            stackSize--;
            if ( stackSize > 0 )
            {
                currentCellIndex = cellStack[stackSize - 1];
            }
        }
    }

    // Clean up memory
    delete[] cellStack;
}



// Binary Tree maze generation algorithm.
// For each cell, randomly choose to either remove the wall 
// to the north or the wall to the east ( if they exist ).
// This creates a maze with a distinctive bias.
void buildMazeBinaryTree()
{
    for ( int cellRow = 0; cellRow < MazeRows_Glob; cellRow++ )
    {
        for ( int cellCol = 0; cellCol < MazeColumns_Glob; cellCol++ )
        {
            int currentCellIndex = cellRow * MazeColumns_Glob + cellCol;
            int validWalls[2];
            int validWallCount = 0;

            // Check if we can go north ( UP ) - only if not in top row
            if ( cellRow > 0 )
            {
                int wallIndex = CellToWalls_Glob[currentCellIndex][UP];
                if ( wallIndex != NO_CELL )
                {
                    validWalls[validWallCount] = wallIndex;
                    validWallCount++;
                }
            }

            // Check if we can go east ( RIGHT ) - only if not in rightmost column
            if ( cellCol < MazeColumns_Glob - 1 )
            {
                int wallIndex = CellToWalls_Glob[currentCellIndex][RIGHT];
                if ( wallIndex != NO_CELL )
                {
                    validWalls[validWallCount] = wallIndex;
                    validWallCount++;
                }
            }

            // If we have at least one valid wall, pick one randomly and remove it
            if ( validWallCount > 0 )
            {
                int randomWallIndex = rand() % validWallCount;
                int wallToRemove = validWalls[randomWallIndex];
                WallsUp_Glob[wallToRemove] = false;
            }

            sleepTenthSecond();
            printMaze();
        }
    }
}



// Recursive Division maze generation algorithm.
// Start with an empty area ( all walls down ), then recursively
// divide the area with walls, leaving random gaps.
void buildMazeRecursiveDivision()
{
    // Start with all interior walls down
    for ( int wallIndex = 0; wallIndex < InteriorWallCount_Glob; wallIndex++ )
    {
        WallsUp_Glob[wallIndex] = false;
    }

    printMaze();
    sleepOneSecond();

    // Recursively divide the entire maze area
    divideArea ( 0, MazeRows_Glob - 1, 0, MazeColumns_Glob - 1 );
}



// Note we could use command line arguments instead of user 
// prompts to get the maze size and algorithm choice.
int main()
{
    // Seed random number generator
    srand ( time ( nullptr ) );

    // Prompt the user for maze size
    MazeColumns_Glob = 0;
    while ( MazeColumns_Glob <= 0 )
    {
        printf ( "Please enter number of columns for maze, must be greater than 1: " );
        char userInput[100];
        if ( fgets ( userInput, sizeof ( userInput ), stdin ) )
        {
            // Remove newline if present
            int len = strlen ( userInput );
            if ( len > 0 && userInput[len - 1] == '\n' )
            {
                userInput[len - 1] = '\0';
            }
            
            // Check if string is a valid number
            bool isValid = true;
            if ( userInput[0] == '\0' )
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
            
            if ( isValid == true )
            {
                MazeColumns_Glob = atoi ( userInput );
            }
        }
    }

    MazeRows_Glob = 0;
    while ( MazeRows_Glob <= 0 )
    {
        printf ( "Please enter number of rows for maze, must be greater than 1: " );
        char userInput[100];
        if ( fgets ( userInput, sizeof ( userInput ), stdin ) )
        {
            // Remove newline if present
            int len = strlen ( userInput );
            if ( len > 0 && userInput[len - 1] == '\n' )
            {
                userInput[len - 1] = '\0';
            }
            
            // Check if string is a valid number
            bool isValid = true;
            if ( userInput[0] == '\0' )
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
            
            if ( isValid == true )
            {
                MazeRows_Glob = atoi ( userInput );
            }
        }
    }

    // Prompt the user for algorithm choice
    int algorithmChoice = 0;
    while ( algorithmChoice < 1 || algorithmChoice > 5 )
    {
        printf ( "Please choose maze generation algorithm:\n" );
        printf ( "1 - Kruskal's Algorithm\n" );
        printf ( "2 - Prim's Algorithm\n" );
        printf ( "3 - Depth-First Search\n" );
        printf ( "4 - Binary Tree Algorithm\n" );
        printf ( "5 - Recursive Division Algorithm\n" );
        char userInput[100];
        userInput[0] = '\0';
        if ( fgets ( userInput, sizeof ( userInput ), stdin ) )
        {
            // Remove newline if present
            int len = strlen ( userInput );
            if ( len > 0 && userInput[len - 1] == '\n' )
            {
                userInput[len - 1] = '\0';
            }
            
            // Check if string is a valid number
            bool isValid = true;
            if ( userInput[0] == '\0' )
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
            
            if ( isValid == true )
            {
                algorithmChoice = atoi ( userInput );
            }
        }
    }

    // Setup maze datastructures for the user entered size.
    AllHorizontalWallsUp_Glob = new bool[MazeColumns_Glob];
    for ( int i = 0; i < MazeColumns_Glob; i++ )
    {
        AllHorizontalWallsUp_Glob[i] = true;
    }
    
    InteriorWallCount_Glob = MazeRows_Glob * ( MazeColumns_Glob - 1 ) + ( MazeRows_Glob - 1 ) * MazeColumns_Glob;
    
    WallsUp_Glob = new bool[InteriorWallCount_Glob];
    for ( int i = 0; i < InteriorWallCount_Glob; i++ )
    {
        WallsUp_Glob[i] = true;
    }

    // Initialize algorithm state globals
    CellVisited_Glob = new bool[MazeRows_Glob * MazeColumns_Glob];
    for ( int i = 0; i < MazeRows_Glob * MazeColumns_Glob; i++ )
    {
        CellVisited_Glob[i] = false;
    }
    
    FrontierWalls_Glob = new int[InteriorWallCount_Glob];
    
    CellInMaze_Glob = new bool[MazeRows_Glob * MazeColumns_Glob];
    for ( int i = 0; i < MazeRows_Glob * MazeColumns_Glob; i++ )
    {
        CellInMaze_Glob[i] = false;
    }

    // Initialize lookup tables based on chosen algorithm
    WallToCells_Glob = new int*[InteriorWallCount_Glob];
    for ( int i = 0; i < InteriorWallCount_Glob; i++ )
    {
        WallToCells_Glob[i] = new int[2];
    }
    
    CellToWalls_Glob = new int*[MazeRows_Glob * MazeColumns_Glob];
    for ( int i = 0; i < MazeRows_Glob * MazeColumns_Glob; i++ )
    {
        CellToWalls_Glob[i] = new int[4];
    }
    
    initializeLookupTables ( algorithmChoice );

    // Execute the chosen algorithm
    if ( algorithmChoice == KRUSKAL_ALGORITHM )
    {
        buildMazeKruskal();
    }
    else if ( algorithmChoice == PRIM_ALGORITHM )
    {
        buildMazePrim();
    }
    else if ( algorithmChoice == DEPTH_FIRST_ALGORITHM )
    {
        buildMazeDepthFirst();
    }
    else if ( algorithmChoice == BINARY_TREE_ALGORITHM )
    {
        buildMazeBinaryTree();
    }
    else if ( algorithmChoice == RECURSIVE_DIVISION_ALGORITHM )
    {
        buildMazeRecursiveDivision();
    }

    // Clean up global memory
    delete[] AllHorizontalWallsUp_Glob;
    delete[] WallsUp_Glob;
    delete[] CellVisited_Glob;
    delete[] FrontierWalls_Glob;
    delete[] CellInMaze_Glob;
    
    for ( int i = 0; i < InteriorWallCount_Glob; i++ )
    {
        delete[] WallToCells_Glob[i];
    }
    delete[] WallToCells_Glob;
    
    for ( int i = 0; i < MazeRows_Glob * MazeColumns_Glob; i++ )
    {
        delete[] CellToWalls_Glob[i];
    }
    delete[] CellToWalls_Glob;

    return 0;
}
