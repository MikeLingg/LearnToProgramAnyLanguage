with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Numerics.Discrete_Random;
with Ada.Calendar; use Ada.Calendar;

procedure Maze_Generator is

    -- Constants for directions
    LEFT  : constant Integer := 0;
    UP    : constant Integer := 1;
    RIGHT : constant Integer := 2;
    DOWN  : constant Integer := 3;

    FIRST_CELL  : constant Integer := 0;
    SECOND_CELL : constant Integer := 1;
    NO_CELL     : constant Integer := -1;

    -- Define algorithm types
    KRUSKAL_ALGORITHM : constant Integer := 1;
    PRIM_ALGORITHM : constant Integer := 2;
    DEPTH_FIRST_ALGORITHM : constant Integer := 3;
    BINARY_TREE_ALGORITHM : constant Integer := 4;
    RECURSIVE_DIVISION_ALGORITHM : constant Integer := 5;

    -- Define array types
    type Boolean_Array is array (Integer range <>) of Boolean;
    type Integer_Array is array (Integer range <>) of Integer;

    -- Access types for dynamic allocation
    type Boolean_Array_Access is access Boolean_Array;
    type Integer_Array_Access is access Integer_Array;
    type Wall_Cell_Array is array (Integer range <>, Integer range <>) of Integer;
    type Wall_Cell_Array_Access is access Wall_Cell_Array;

    -- Maze dimensions and counts
    MazeRows_Glob          : Integer := 0;
    MazeColumns_Glob       : Integer := 0;
    InteriorWallCount_Glob : Integer := 0;

    -- Global arrays (allocated at runtime once MazeRows/Cols known)
    WallsUp_Glob         : Boolean_Array_Access;
    CellVisited_Glob     : Boolean_Array_Access;
    CellInMaze_Glob      : Boolean_Array_Access;
    FrontierWalls_Glob   : Integer_Array_Access;
    AllHorizontalWallsUp_Glob : Boolean_Array_Access;

    WallToCells_Glob     : Wall_Cell_Array_Access;
    CellToWalls_Glob     : Wall_Cell_Array_Access;

    FrontierWallCount_Glob : Integer := 0;

    -- Random number generator
    subtype Random_Range is Integer range 0 .. Integer'Last;
    package Random_Int is new Ada.Numerics.Discrete_Random ( Random_Range );
    Generator : Random_Int.Generator;



    -- +-+-+-+
    -- Prints a horizontal wall, similar to the example above, 
    -- with walls down based on the provided parameters.
    -- HorizontalWallsUp_Par: Boolean array of size MazeColumns_Glob, 
    --     True indicates the wall should be printed as up.
    procedure printHorizontalWalls ( HorizontalWallsUp_Par : in out Boolean_Array ) is
    begin
        Put ( "+" );
        for horizontalWallIndex in 0 .. MazeColumns_Glob - 1 loop
            if ( HorizontalWallsUp_Par ( horizontalWallIndex ) = True ) then
                Put ( "-" );
            else
                Put ( " " );
            end if;
            Put ( "+" );
        end loop;
        New_Line;
    end printHorizontalWalls;



    -- +-+-+-+
    -- Prints a horizontal wall, similar to the example above, with all walls up.
    procedure printAllHorizontalWalls is
    begin
        printHorizontalWalls ( AllHorizontalWallsUp_Glob.all );
    end printAllHorizontalWalls;



    -- | | | |
    -- Prints a vertical wall, similar to the example above, 
    -- with walls down based on the provided parameters.
    -- VerticalWallsUp_Par: Boolean array of size MazeColumns_Glob - 1, 
    --     True indicates the wall should be printed as up.
    procedure printVerticalWalls ( VerticalWallsUp_Par : in out Boolean_Array ) is
    begin
        -- First wall is an exterior wall, always up.
        Put ( "|" );
        for verticalWallIndex in 0 .. MazeColumns_Glob - 2 loop
            Put ( " " );
            if ( VerticalWallsUp_Par ( verticalWallIndex ) = True ) then
                Put ( "|" );
            else
                Put ( " " );
            end if;
        end loop;
        -- Last wall exterior, always up.
        Put ( " " );
        Put ( "|" );
        New_Line;
    end printVerticalWalls;



    -- Loop through the rows of the maze and print the maze 
    -- based on WallsUp_Glob
    procedure printMaze is
        interiorWallIndex : Integer := 0;
        verticalWallsUp : Boolean_Array ( 0 .. 98 );
        horizontalWallsUp : Boolean_Array ( 0 .. 99 );
    begin
        -- First row is exterior walls
        printAllHorizontalWalls;
        for rowIndex in 0 .. MazeRows_Glob - 1 loop
            for columnIndex in 0 .. MazeColumns_Glob - 2 loop
                verticalWallsUp ( columnIndex ) := WallsUp_Glob ( interiorWallIndex );
                interiorWallIndex := interiorWallIndex + 1;
            end loop;

            printVerticalWalls ( verticalWallsUp );

            if ( rowIndex = MazeRows_Glob - 1 ) then
                printAllHorizontalWalls;
            else
                for columnIndex in 0 .. MazeColumns_Glob - 1 loop
                    horizontalWallsUp ( columnIndex ) := WallsUp_Glob ( interiorWallIndex );
                    interiorWallIndex := interiorWallIndex + 1;
                end loop;

                printHorizontalWalls ( horizontalWallsUp );
            end if;
        end loop;

        New_Line;
    end printMaze;



    -- Simple sleep function
    procedure sleepHalfSecond is
        Start_Time : Time;
        Current_Time : Time;
    begin
        Start_Time := Clock;
        loop
            Current_Time := Clock;
            exit when ( Current_Time - Start_Time >= 0.5 );
        end loop;
    end sleepHalfSecond;



    -- Initialize lookup tables for wall/cell relationships.
    -- AlgorithmType_Par: Algorithm constant to determine which tables to build
    -- Must be called after maze dimensions are set.
    procedure initializeLookupTables ( AlgorithmType_Par : Integer ) is
        wallIndex : Integer;
        firstCellInRow : Integer;
        leftCell : Integer;
        rightCell : Integer;
        upperCell : Integer;
        lowerCell : Integer;
        cellRow : Integer;
        cellCol : Integer;
        firstCellIndex : Integer;
        secondCellIndex : Integer;
        secondRow : Integer;
        secondCol : Integer;
        firstRow : Integer;
        firstCol : Integer;
    begin
        -- Build WallToCells_Glob for algorithms that need wall-to-cell lookups
        if ( AlgorithmType_Par = KRUSKAL_ALGORITHM or AlgorithmType_Par = PRIM_ALGORITHM or 
             AlgorithmType_Par = DEPTH_FIRST_ALGORITHM or AlgorithmType_Par = BINARY_TREE_ALGORITHM or 
             AlgorithmType_Par = RECURSIVE_DIVISION_ALGORITHM ) then
            
            wallIndex := 0;
            for rowIndex in 0 .. MazeRows_Glob - 1 loop
                -- Track the first cell in the current row
                firstCellInRow := rowIndex * MazeColumns_Glob;

                -- Note the 0..MazeColumns_Glob minus 2, one less vertical wall
                -- than the number of columns.
                for verticalWallIndex in 0 .. MazeColumns_Glob - 2 loop
                    leftCell := firstCellInRow + verticalWallIndex;
                    rightCell := leftCell + 1;
                    WallToCells_Glob ( wallIndex, FIRST_CELL ) := leftCell;
                    WallToCells_Glob ( wallIndex, SECOND_CELL ) := rightCell;
                    wallIndex := wallIndex + 1;
                end loop;

                -- The last row will have no interior horizontal walls below
                -- it, so will be skipped.
                if ( wallIndex < InteriorWallCount_Glob ) then
                    for horizontalWallIndex in 0 .. MazeColumns_Glob - 1 loop
                        upperCell := firstCellInRow + horizontalWallIndex;
                        lowerCell := upperCell + MazeColumns_Glob;
                        WallToCells_Glob ( wallIndex, FIRST_CELL ) := upperCell;
                        WallToCells_Glob ( wallIndex, SECOND_CELL ) := lowerCell;
                        wallIndex := wallIndex + 1;
                    end loop;
                end if;
            end loop;
        end if;

        -- Build CellToWalls_Glob for algorithms that need cell-to-wall lookups
        if ( AlgorithmType_Par = PRIM_ALGORITHM or AlgorithmType_Par = DEPTH_FIRST_ALGORITHM or 
             AlgorithmType_Par = BINARY_TREE_ALGORITHM or AlgorithmType_Par = RECURSIVE_DIVISION_ALGORITHM ) then
            
            for cellIndex in 0 .. MazeRows_Glob * MazeColumns_Glob - 1 loop
                cellRow := cellIndex / MazeColumns_Glob;
                cellCol := cellIndex mod MazeColumns_Glob;
                
                -- Initialize all directions to NO_CELL (invalid wall)
                CellToWalls_Glob ( cellIndex, LEFT ) := NO_CELL;
                CellToWalls_Glob ( cellIndex, UP ) := NO_CELL;
                CellToWalls_Glob ( cellIndex, RIGHT ) := NO_CELL;
                CellToWalls_Glob ( cellIndex, DOWN ) := NO_CELL;
                
                -- Find walls by checking which walls connect to this cell
                for wallIndex in 0 .. InteriorWallCount_Glob - 1 loop
                    firstCellIndex := WallToCells_Glob ( wallIndex, FIRST_CELL );
                    secondCellIndex := WallToCells_Glob ( wallIndex, SECOND_CELL );
                    
                    if ( firstCellIndex = cellIndex ) then
                        -- This wall connects from our cell to secondCell
                        secondRow := secondCellIndex / MazeColumns_Glob;
                        secondCol := secondCellIndex mod MazeColumns_Glob;
                        
                        if ( secondRow = cellRow and secondCol = cellCol + 1 ) then
                            -- Wall goes RIGHT
                            CellToWalls_Glob ( cellIndex, RIGHT ) := wallIndex;
                        elsif ( secondRow = cellRow + 1 and secondCol = cellCol ) then
                            -- Wall goes DOWN
                            CellToWalls_Glob ( cellIndex, DOWN ) := wallIndex;
                        end if;
                    elsif ( secondCellIndex = cellIndex ) then
                        -- This wall connects from firstCellIndex to our cell
                        firstRow := firstCellIndex / MazeColumns_Glob;
                        firstCol := firstCellIndex mod MazeColumns_Glob;
                        
                        if ( firstRow = cellRow and firstCol = cellCol - 1 ) then
                            -- Wall comes from LEFT
                            CellToWalls_Glob ( cellIndex, LEFT ) := wallIndex;
                        elsif ( firstRow = cellRow - 1 and firstCol = cellCol ) then
                            -- Wall comes from UP
                            CellToWalls_Glob ( cellIndex, UP ) := wallIndex;
                        end if;
                    end if;
                end loop;
            end loop;
        end if;
    end initializeLookupTables;



    -- Add a wall to frontier if not already there.
    -- WallIndex_Par: Index of wall to add to frontier, must be 0 to InteriorWallCount_Glob-1
    procedure addWallToFrontier ( WallIndex_Par : Integer ) is
        alreadyInFrontier : Boolean := False;
    begin
        -- Check if wall already in frontier
        for wallIndex in 0 .. FrontierWallCount_Glob - 1 loop
            if ( FrontierWalls_Glob ( wallIndex ) = WallIndex_Par ) then
                alreadyInFrontier := True;
                exit;
            end if;
        end loop;

        if ( alreadyInFrontier = False ) then
            FrontierWalls_Glob ( FrontierWallCount_Glob ) := WallIndex_Par;
            FrontierWallCount_Glob := FrontierWallCount_Glob + 1;
        end if;
    end addWallToFrontier;



    -- Add all walls adjacent to a cell to the frontier list.
    -- CellIndex_Par: Index of the cell whose adjacent walls should be added to frontier: 0 .. ( MazeRows_Glob * MazeColumns_Glob - 1 )
    procedure addCellWallsToFrontier ( CellIndex_Par : Integer ) is
    begin
        -- Check all four directions
        if ( CellToWalls_Glob ( CellIndex_Par, UP ) /= NO_CELL ) then
            addWallToFrontier ( CellToWalls_Glob ( CellIndex_Par, UP ) );
        end if;
        if ( CellToWalls_Glob ( CellIndex_Par, DOWN ) /= NO_CELL ) then
            addWallToFrontier ( CellToWalls_Glob ( CellIndex_Par, DOWN ) );
        end if;
        if ( CellToWalls_Glob ( CellIndex_Par, LEFT ) /= NO_CELL ) then
            addWallToFrontier ( CellToWalls_Glob ( CellIndex_Par, LEFT ) );
        end if;
        if ( CellToWalls_Glob ( CellIndex_Par, RIGHT ) /= NO_CELL ) then
            addWallToFrontier ( CellToWalls_Glob ( CellIndex_Par, RIGHT ) );
        end if;
    end addCellWallsToFrontier;



    -- Recursively divide an area with walls.
    -- StartRow_Par: Starting row of area to divide, must be 0 to MazeRows_Glob-1
    -- EndRow_Par: Ending row of area to divide, must be StartRow_Par to MazeRows_Glob-1
    -- StartCol_Par: Starting column of area to divide, must be 0 to MazeColumns_Glob-1
    -- EndCol_Par: Ending column of area to divide, must be StartCol_Par to MazeColumns_Glob-1
    procedure divideArea ( StartRow_Par : Integer; EndRow_Par : Integer; StartCol_Par : Integer; EndCol_Par : Integer ) is
        height : Integer;
        width : Integer;
        divideHorizontally : Boolean := False;
        divideRow : Integer;
        divideCol : Integer;
        cellIndex : Integer;
        wallIndex : Integer;
        gapCol : Integer;
        gapRow : Integer;
        gapCellIndex : Integer;
        gapWallIndex : Integer;
        randomValue : Integer;
    begin
        height := EndRow_Par - StartRow_Par + 1;
        width := EndCol_Par - StartCol_Par + 1;

        -- Base case - area too small to divide
        if ( height >= 2 or width >= 2 ) then
            -- Choose whether to divide horizontally or vertically
            if ( height > width ) then
                divideHorizontally := True;
            elsif ( width > height ) then
                divideHorizontally := False;
            else
                -- Square area - choose randomly
                randomValue := Random_Int.Random ( Generator );
                divideHorizontally := ( randomValue mod 2 = 0 );
            end if;

            if ( divideHorizontally = True and height > 1 ) then
                -- Choose random row to divide on
                randomValue := Random_Int.Random ( Generator );
                divideRow := StartRow_Par + ( randomValue mod ( EndRow_Par - StartRow_Par ) );

                -- Add horizontal wall
                for wallCol in StartCol_Par .. EndCol_Par loop
                    cellIndex := divideRow * MazeColumns_Glob + wallCol;
                    if ( cellIndex < MazeRows_Glob * MazeColumns_Glob ) then
                        wallIndex := CellToWalls_Glob ( cellIndex, DOWN );
                        if ( wallIndex /= NO_CELL ) then
                            WallsUp_Glob ( wallIndex ) := True;
                        end if;
                    end if;
                end loop;

                -- Choose random gap in the wall
                randomValue := Random_Int.Random ( Generator );
                gapCol := StartCol_Par + ( randomValue mod ( EndCol_Par - StartCol_Par + 1 ) );
                gapCellIndex := divideRow * MazeColumns_Glob + gapCol;
                if ( gapCellIndex < MazeRows_Glob * MazeColumns_Glob ) then
                    gapWallIndex := CellToWalls_Glob ( gapCellIndex, DOWN );
                    if ( gapWallIndex /= NO_CELL ) then
                        WallsUp_Glob ( gapWallIndex ) := False;
                    end if;
                end if;

                sleepHalfSecond;
                printMaze;

                -- Recursively divide the two areas
                divideArea ( StartRow_Par, divideRow, StartCol_Par, EndCol_Par );
                divideArea ( divideRow + 1, EndRow_Par, StartCol_Par, EndCol_Par );
            elsif ( divideHorizontally = False and width > 1 ) then
                -- Choose random column to divide on
                randomValue := Random_Int.Random ( Generator );
                divideCol := StartCol_Par + ( randomValue mod ( EndCol_Par - StartCol_Par ) );

                -- Add vertical wall
                for cellRow in StartRow_Par .. EndRow_Par loop
                    cellIndex := cellRow * MazeColumns_Glob + divideCol;
                    if ( cellIndex < MazeRows_Glob * MazeColumns_Glob ) then
                        wallIndex := CellToWalls_Glob ( cellIndex, RIGHT );
                        if ( wallIndex /= NO_CELL ) then
                            WallsUp_Glob ( wallIndex ) := True;
                        end if;
                    end if;
                end loop;

                -- Choose random gap in the wall
                randomValue := Random_Int.Random ( Generator );
                gapRow := StartRow_Par + ( randomValue mod ( EndRow_Par - StartRow_Par + 1 ) );
                gapCellIndex := gapRow * MazeColumns_Glob + divideCol;
                if ( gapCellIndex < MazeRows_Glob * MazeColumns_Glob ) then
                    gapWallIndex := CellToWalls_Glob ( gapCellIndex, RIGHT );
                    if ( gapWallIndex /= NO_CELL ) then
                        WallsUp_Glob ( gapWallIndex ) := False;
                    end if;
                end if;

                sleepHalfSecond;
                printMaze;

                -- Recursively divide the two areas
                divideArea ( StartRow_Par, EndRow_Par, StartCol_Par, divideCol );
                divideArea ( StartRow_Par, EndRow_Par, divideCol + 1, EndCol_Par );
            end if;
        end if;
    end divideArea;



    -- Kruskal's algorithm.
    -- The simple description of the algorithm is first place each 
    -- cell in its own group.  Then process all walls in random order,
    -- if the cells on either side of the wall are in separate groups, 
    -- remove the wall and merge the groups.  Repeat until all 
    -- cells are now in the same group.
    procedure buildMazeKruskal is
        -- Identify which cells are a part of each group
        -- Note each array is large enough to fit all cells,
        -- NO_CELL indicates no cell is assigned to this index.
        cellToGroup : Integer_Array ( 0 .. MazeRows_Glob * MazeColumns_Glob - 1 );
        groupCells : array ( 0 .. MazeRows_Glob * MazeColumns_Glob - 1, 0 .. MazeRows_Glob * MazeColumns_Glob - 1 ) of Integer;
        mazeComplete : Boolean := False;
        wallRemoveList : Integer_Array ( 0 .. InteriorWallCount_Glob - 1 );
        shuffleIndex : Integer;
        randomIndex : Integer;
        temp : Integer;
        nextWallToCheck : Integer;
        firstCellIndex : Integer;
        firstCellGroupIndex : Integer;
        secondCellIndex : Integer;
        secondCellGroupIndex : Integer;
        nextEmptyFirstGroupIndex : Integer;
        cellToMove : Integer;
        randomValue : Integer;
    begin
        for cellIndex in 0 .. MazeRows_Glob * MazeColumns_Glob - 1 loop
            cellToGroup ( cellIndex ) := cellIndex;

            for groupCellIndex in 0 .. MazeRows_Glob * MazeColumns_Glob - 1 loop
                if ( groupCellIndex = 0 ) then
                    groupCells ( cellIndex, groupCellIndex ) := cellIndex;
                else
                    groupCells ( cellIndex, groupCellIndex ) := NO_CELL;
                end if;
            end loop;
        end loop;

        for wallIndex in 0 .. InteriorWallCount_Glob - 1 loop
            wallRemoveList ( wallIndex ) := wallIndex;
        end loop;
        
        -- Fisher-Yates shuffle
        shuffleIndex := InteriorWallCount_Glob - 1;
        while ( shuffleIndex > 0 ) loop
            randomValue := Random_Int.Random ( Generator );
            randomIndex := randomValue mod ( shuffleIndex + 1 );
            temp := wallRemoveList ( shuffleIndex );
            wallRemoveList ( shuffleIndex ) := wallRemoveList ( randomIndex );
            wallRemoveList ( randomIndex ) := temp;
            shuffleIndex := shuffleIndex - 1;
        end loop;

        -- Perform Kruskal's algorithm.
        for removeWallIndex in 0 .. InteriorWallCount_Glob - 1 loop
            nextWallToCheck := wallRemoveList ( removeWallIndex );

            -- If the two cells connected to this wall are not part 
            -- of the same group, remove the wall and merge the 
            -- groups.
            firstCellIndex := WallToCells_Glob ( nextWallToCheck, FIRST_CELL );
            firstCellGroupIndex := cellToGroup ( firstCellIndex );
            secondCellIndex := WallToCells_Glob ( nextWallToCheck, SECOND_CELL );
            secondCellGroupIndex := cellToGroup ( secondCellIndex );

            if ( ( firstCellGroupIndex < 0 or firstCellGroupIndex >= MazeRows_Glob * MazeColumns_Glob ) and
                 ( secondCellGroupIndex < 0 or secondCellGroupIndex >= MazeRows_Glob * MazeColumns_Glob ) ) then
                Put_Line("Error: Invalid group indices");
                return;
            end if;

            if ( firstCellGroupIndex /= secondCellGroupIndex ) then
                WallsUp_Glob ( nextWallToCheck ) := False;

                -- Loop through the indices of all cells in the first 
                -- group until we find a NO_CELL indicating no cell here.
                nextEmptyFirstGroupIndex := 0;
                while ( nextEmptyFirstGroupIndex < MazeColumns_Glob * MazeRows_Glob and then
                        groupCells ( firstCellGroupIndex, nextEmptyFirstGroupIndex ) /= NO_CELL ) loop
                    nextEmptyFirstGroupIndex := nextEmptyFirstGroupIndex + 1;
                end loop;

                -- Loop through the indices of all cells in the second group,
                -- move each cell to the first group, and set that cell's 
                -- group to the first group index.
                for groupCellIndex in reverse 0 .. MazeColumns_Glob * MazeRows_Glob - 1 loop
                    -- Skip until we reach valid cells
                    if ( groupCells ( secondCellGroupIndex, groupCellIndex ) /= NO_CELL ) then
                        -- Get the id number of the cell to move from 
                        -- the second group to the first group
                        cellToMove := groupCells ( secondCellGroupIndex, groupCellIndex );

                        -- Move the cell number from the second group 
                        -- to the first group
                        groupCells ( firstCellGroupIndex, nextEmptyFirstGroupIndex ) := cellToMove;
                        -- Move our empty index to the next cell in this array.
                        nextEmptyFirstGroupIndex := nextEmptyFirstGroupIndex + 1;
                        -- Mark this cell as part of the first group.
                        cellToGroup ( cellToMove ) := firstCellGroupIndex;
                        -- Remove the cell from the second group (set the
                        -- array entry to NO_CELL)
                        groupCells ( secondCellGroupIndex, groupCellIndex ) := NO_CELL;

                        if ( nextEmptyFirstGroupIndex >= MazeColumns_Glob * MazeRows_Glob ) then
                            mazeComplete := True;
                        end if;
                    end if;
                end loop;

                sleepHalfSecond;
                printMaze;

                if ( mazeComplete = True ) then
                    exit;
                end if;
            end if;
        end loop;
    end buildMazeKruskal;



    -- Prim's algorithm for maze generation.
    -- Start with a random cell, mark it as part of the maze.
    -- Repeatedly pick a random wall from cells in the maze that 
    -- connects to a cell not in the maze, remove the wall and 
    -- add the new cell to the maze.
    procedure buildMazePrim is
        cellsInMaze : Integer := 0;
        startCell : Integer;
        randomWallIndex : Integer;
        wallToCheck : Integer;
        firstCellIndex : Integer;
        secondCellIndex : Integer;
        outerCellIndex : Integer := -1;
        randomValue : Integer;
    begin
        -- Initialize algorithm state
        FrontierWallCount_Glob := 0;

        -- Start with a random cell
        randomValue := Random_Int.Random ( Generator );
        startCell := randomValue mod ( MazeRows_Glob * MazeColumns_Glob );
        CellInMaze_Glob ( startCell ) := True;
        cellsInMaze := cellsInMaze + 1;

        -- Add all walls adjacent to the start cell to frontier
        addCellWallsToFrontier ( startCell );

        -- Continue until all cells are in the maze
        while ( cellsInMaze < MazeRows_Glob * MazeColumns_Glob ) loop
            -- Pick a random wall from frontier
            randomValue := Random_Int.Random ( Generator );
            randomWallIndex := randomValue mod FrontierWallCount_Glob;
            wallToCheck := FrontierWalls_Glob ( randomWallIndex );

            -- Remove this wall from frontier list by replacing 
            -- it with the last wall in the list
            FrontierWalls_Glob ( randomWallIndex ) := FrontierWalls_Glob ( FrontierWallCount_Glob - 1 );
            FrontierWallCount_Glob := FrontierWallCount_Glob - 1;

            -- Get the two cells this wall connects
            firstCellIndex := WallToCells_Glob ( wallToCheck, FIRST_CELL );
            secondCellIndex := WallToCells_Glob ( wallToCheck, SECOND_CELL );

            -- If one cell is already in the maze and the 
            -- other is not, remove the wall to connect the 
            -- outside cell to the maze
            if ( CellInMaze_Glob ( firstCellIndex ) /= CellInMaze_Glob ( secondCellIndex ) ) then
                WallsUp_Glob ( wallToCheck ) := False;

                -- Add the outside cell to the maze
                if ( CellInMaze_Glob ( firstCellIndex ) = False ) then
                    outerCellIndex := firstCellIndex;
                else
                    outerCellIndex := secondCellIndex;
                end if;

                CellInMaze_Glob ( outerCellIndex ) := True;
                cellsInMaze := cellsInMaze + 1;
                addCellWallsToFrontier ( outerCellIndex );

                sleepHalfSecond;
                printMaze;
            end if;
        end loop;
    end buildMazePrim;



    -- Depth-first search maze generation using recursive backtracking.
    -- Start at a random cell, randomly walk to neighbors outside 
    -- the maze,removing walls as you go. 
    -- When all neighbors are in the maze, backtrack to a cell 
    -- with neighbors outside the maze.
    procedure buildMazeDepthFirst is
        cellStack : Integer_Array ( 0 .. MazeRows_Glob * MazeColumns_Glob - 1 );
        stackSize : Integer := 0;
        currentCellIndex : Integer;
        randomizedDirections : array ( 0 .. 3 ) of Integer := ( LEFT, UP, RIGHT, DOWN );
        shuffleIndex : Integer;
        randomIndex : Integer;
        temp : Integer;
        foundNeighbor : Boolean := False;
        nextCellIndex : Integer := NO_CELL;
        wallIndex : Integer := NO_CELL;
        firstCellIndex : Integer;
        secondCellIndex : Integer;
        wallCandidate : Integer;
        randomValue : Integer;
    begin
        -- Start with random cell
        randomValue := Random_Int.Random ( Generator );
        currentCellIndex := randomValue mod ( MazeRows_Glob * MazeColumns_Glob );
        CellVisited_Glob ( currentCellIndex ) := True;

        -- Push starting cell onto stack
        cellStack ( stackSize ) := currentCellIndex;
        stackSize := stackSize + 1;

        while ( stackSize > 0 ) loop
            -- Create randomized direction list
            randomizedDirections := ( LEFT, UP, RIGHT, DOWN );
            
            -- Fisher-Yates shuffle the directions
            shuffleIndex := 3;
            while ( shuffleIndex >= 1 ) loop
                randomValue := Random_Int.Random ( Generator );
                randomIndex := randomValue mod ( shuffleIndex + 1 );
                temp := randomizedDirections ( shuffleIndex );
                randomizedDirections ( shuffleIndex ) := randomizedDirections ( randomIndex );
                randomizedDirections ( randomIndex ) := temp;
                shuffleIndex := shuffleIndex - 1;
            end loop;

            foundNeighbor := False;

            -- Check directions in random order until we find an unvisited neighbor
            for directionIndex in 0 .. 3 loop
                wallIndex := CellToWalls_Glob ( currentCellIndex, randomizedDirections ( directionIndex ) );
                if ( wallIndex /= NO_CELL ) then
                    -- Find the cell on the other side of this wall
                    firstCellIndex := WallToCells_Glob ( wallIndex, FIRST_CELL );
                    secondCellIndex := WallToCells_Glob ( wallIndex, SECOND_CELL );
                    
                    if ( firstCellIndex = currentCellIndex ) then
                        nextCellIndex := secondCellIndex;
                    else
                        nextCellIndex := firstCellIndex;
                    end if;
                    
                    -- Check if neighbor is unvisited
                    if ( CellVisited_Glob ( nextCellIndex ) = False ) then
                        foundNeighbor := True;
                        exit;
                    end if;
                end if;
            end loop;

            if ( foundNeighbor = True ) then
                -- Find wall between current and next cell using lookup tables
                wallIndex := NO_CELL;
                for direction in 0 .. 3 loop
                    wallCandidate := CellToWalls_Glob ( currentCellIndex, direction );
                    if ( wallCandidate /= NO_CELL ) then
                        firstCellIndex := WallToCells_Glob ( wallCandidate, FIRST_CELL );
                        secondCellIndex := WallToCells_Glob ( wallCandidate, SECOND_CELL );
                        if ( ( firstCellIndex = currentCellIndex and secondCellIndex = nextCellIndex ) or 
                             ( firstCellIndex = nextCellIndex and secondCellIndex = currentCellIndex ) ) then
                            wallIndex := wallCandidate;
                            exit;
                        end if;
                    end if;
                end loop;
                WallsUp_Glob ( wallIndex ) := False;

                -- Mark next cell as visited
                CellVisited_Glob ( nextCellIndex ) := True;

                -- Push next cell onto stack
                cellStack ( stackSize ) := nextCellIndex;
                stackSize := stackSize + 1;

                currentCellIndex := nextCellIndex;

                sleepHalfSecond;
                printMaze;
            else
                -- Backtrack - pop from stack
                stackSize := stackSize - 1;
                if ( stackSize > 0 ) then
                    currentCellIndex := cellStack ( stackSize - 1 );
                end if;
            end if;
        end loop;
    end buildMazeDepthFirst;



    -- Binary Tree maze generation algorithm.
    -- For each cell, randomly choose to either remove the wall 
    -- to the north or the wall to the east (if they exist).
    -- This creates a maze with a distinctive bias.
    procedure buildMazeBinaryTree is
        currentCellIndex : Integer;
        validWalls : array ( 0 .. 1 ) of Integer;
        validWallCount : Integer := 0;
        wallIndex : Integer;
        randomWallIndex : Integer;
        wallToRemove : Integer;
        randomValue : Integer;
    begin
        for cellRow in 0 .. MazeRows_Glob - 1 loop
            for cellCol in 0 .. MazeColumns_Glob - 1 loop
                currentCellIndex := cellRow * MazeColumns_Glob + cellCol;
                validWallCount := 0;

                -- Check if we can go north (UP) - only if not in top row
                if ( cellRow > 0 ) then
                    wallIndex := CellToWalls_Glob ( currentCellIndex, UP );
                    if ( wallIndex /= NO_CELL ) then
                        validWalls ( validWallCount ) := wallIndex;
                        validWallCount := validWallCount + 1;
                    end if;
                end if;

                -- Check if we can go east (RIGHT) - only if not in rightmost column
                if ( cellCol < MazeColumns_Glob - 1 ) then
                    wallIndex := CellToWalls_Glob ( currentCellIndex, RIGHT );
                    if ( wallIndex /= NO_CELL ) then
                        validWalls ( validWallCount ) := wallIndex;
                        validWallCount := validWallCount + 1;
                    end if;
                end if;

                -- If we have at least one valid wall, pick one randomly and remove it
                if ( validWallCount > 0 ) then
                    randomValue := Random_Int.Random ( Generator );
                    randomWallIndex := randomValue mod validWallCount;
                    wallToRemove := validWalls ( randomWallIndex );
                    WallsUp_Glob ( wallToRemove ) := False;
                end if;

                sleepHalfSecond;
                printMaze;
            end loop;
        end loop;
    end buildMazeBinaryTree;



    -- Recursive Division maze generation algorithm.
    -- Start with an empty area (all walls down), then recursively
    -- divide the area with walls, leaving random gaps.
    procedure buildMazeRecursiveDivision is
    begin
        -- Start with all interior walls down
        for wallIndex in 0 .. InteriorWallCount_Glob - 1 loop
            WallsUp_Glob ( wallIndex ) := False;
        end loop;

        printMaze;
        sleepHalfSecond;

        -- Recursively divide the entire maze area
        divideArea ( 0, MazeRows_Glob - 1, 0, MazeColumns_Glob - 1 );
    end buildMazeRecursiveDivision;



    -- Main program variables
    userInput : String ( 1 .. 100 );
    inputLength : Natural;
    isValid : Boolean;
    algorithmChoice : Integer := 0;

begin
    -- Seed random number generator
    Random_Int.Reset ( Generator );

    -- Prompt the user for maze size
    MazeColumns_Glob := 0;
    while ( MazeColumns_Glob <= 0 ) loop
        Put ( "Please enter number of columns for maze, must be greater than 1: " );
        Get_Line ( userInput, inputLength );
        
        -- Check if string is a valid number
        isValid := True;
        if ( inputLength = 0 ) then
            isValid := False;
        else
            for characterIndex in 1 .. inputLength loop
                if ( userInput ( characterIndex ) < '0' or userInput ( characterIndex ) > '9' ) then
                    isValid := False;
                    exit;
                end if;
            end loop;
        end if;
        
        if ( isValid = True ) then
            MazeColumns_Glob := Integer'Value ( userInput ( 1 .. inputLength ) );
        end if;
    end loop;

    MazeRows_Glob := 0;
    while ( MazeRows_Glob <= 0 ) loop
        Put ( "Please enter number of rows for maze, must be greater than 1: " );
        Get_Line ( userInput, inputLength );
        
        -- Check if string is a valid number
        isValid := True;
        if ( inputLength = 0 ) then
            isValid := False;
        else
            for characterIndex in 1 .. inputLength loop
                if ( userInput ( characterIndex ) < '0' or userInput ( characterIndex ) > '9' ) then
                    isValid := False;
                    exit;
                end if;
            end loop;
        end if;
        
        if ( isValid = True ) then
            MazeRows_Glob := Integer'Value ( userInput ( 1 .. inputLength ) );
        end if;
    end loop;

    -- Prompt the user for algorithm choice
    algorithmChoice := 0;
    while ( algorithmChoice < 1 or algorithmChoice > 5 ) loop
        Put_Line ( "Please choose maze generation algorithm:" );
        Put_Line ( "1 - Kruskal's Algorithm" );
        Put_Line ( "2 - Prim's Algorithm" );
        Put_Line ( "3 - Depth-First Search" );
        Put_Line ( "4 - Binary Tree Algorithm" );
        Put_Line ( "5 - Recursive Division Algorithm" );
        
        Get_Line ( userInput, inputLength );
        
        -- Check if string is a valid number
        isValid := True;
        if ( inputLength = 0 ) then
            isValid := False;
        else
            for characterIndex in 1 .. inputLength loop
                if ( userInput ( characterIndex ) < '0' or userInput ( characterIndex ) > '9' ) then
                    isValid := False;
                    exit;
                end if;
            end loop;
        end if;
        
        if ( isValid = True ) then
            algorithmChoice := Integer'Value ( userInput ( 1 .. inputLength ) );
        end if;
    end loop;

    InteriorWallCount_Glob := MazeRows_Glob * ( MazeColumns_Glob - 1 ) + ( MazeRows_Glob - 1 ) * MazeColumns_Glob;
    
    WallsUp_Glob       := new Boolean_Array ( 0 .. InteriorWallCount_Glob - 1 );
    CellVisited_Glob   := new Boolean_Array ( 0 .. MazeColumns_Glob * MazeRows_Glob - 1 );
    CellInMaze_Glob    := new Boolean_Array ( 0 .. MazeColumns_Glob * MazeRows_Glob - 1 );
    FrontierWalls_Glob := new Integer_Array ( 0 .. InteriorWallCount_Glob - 1 );

    WallToCells_Glob := new Wall_Cell_Array ( 0 .. InteriorWallCount_Glob - 1, 0 .. 1 );
    CellToWalls_Glob := new Wall_Cell_Array ( 0 .. MazeColumns_Glob * MazeRows_Glob - 1, 0 .. 3 );

    AllHorizontalWallsUp_Glob := new Boolean_Array ( 0 .. MazeRows_Glob - 1 );

    -- Setup maze datastructures for the user entered size.
    for columnIndex in 0 .. MazeColumns_Glob - 1 loop
        AllHorizontalWallsUp_Glob ( columnIndex ) := True;
    end loop;
    
    for wallIndex in 0 .. InteriorWallCount_Glob - 1 loop
        WallsUp_Glob ( wallIndex ) := True;
    end loop;

    -- Initialize algorithm state globals
    for cellIndex in 0 .. MazeRows_Glob * MazeColumns_Glob - 1 loop
        CellVisited_Glob ( cellIndex ) := False;
    end loop;
    
    for cellIndex in 0 .. MazeRows_Glob * MazeColumns_Glob - 1 loop
        CellInMaze_Glob ( cellIndex ) := False;
    end loop;

    -- Initialize lookup tables based on chosen algorithm
    initializeLookupTables ( algorithmChoice );

    -- Execute the chosen algorithm
    if ( algorithmChoice = KRUSKAL_ALGORITHM ) then
        buildMazeKruskal;
    elsif ( algorithmChoice = PRIM_ALGORITHM ) then
        buildMazePrim;
    elsif ( algorithmChoice = DEPTH_FIRST_ALGORITHM ) then
        buildMazeDepthFirst;
    elsif ( algorithmChoice = BINARY_TREE_ALGORITHM ) then
        buildMazeBinaryTree;
    elsif ( algorithmChoice = RECURSIVE_DIVISION_ALGORITHM ) then
        buildMazeRecursiveDivision;
    end if;

    Put_Line ( "Press Enter to exit..." );
    Get_Line ( userInput, inputLength );

end Maze_Generator;
