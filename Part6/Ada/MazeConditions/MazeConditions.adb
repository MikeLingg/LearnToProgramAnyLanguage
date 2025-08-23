with Ada.Text_IO; use Ada.Text_IO;
with Ada.Calendar; use Ada.Calendar;

procedure MazeConditions is
    -- Initialization code:
    -- Cell indexes:
    -- +-+-+-+
    -- |0|1|2|
    -- +-+-+-+
    -- |3|4|5|
    -- +-+-+-+
    -- |6|7|8|
    -- +-+-+-+
    
    -- Interior wall indexes:
    -- +-+-+-+
    -- | 0 1 |
    -- +2+3+4+
    -- | 5 6 |
    -- +7+8+9+
    -- | 10 11 |
    -- +-+-+-+
    
    -- Define directions
    Left  : constant Integer := 0;
    Up    : constant Integer := 1;
    Right : constant Integer := 2;
    Down  : constant Integer := 3;
    
    -- For each cell 0-8, indicate if a wall is exterior and cannot be removed (-1) or its interior index for each of
    -- the four directions, LEFT, UP, RIGHT, DOWN.
    type Wall_Array is array (0 .. 3) of Integer;
    type Cell_To_Wall_Array is array (0 .. 8) of Wall_Array;
    
    cellToWallLUT : constant Cell_To_Wall_Array :=
        (0 => (-1, -1,  0,  2),   1 => ( 0, -1,  1,  3),   2 => ( 1, -1, -1,  4),
         3 => (-1,  2,  5,  7),   4 => ( 5,  3,  6,  8),   5 => ( 6,  4, -1,  9),
         6 => (-1,  7, 10, -1),   7 => (10,  8, 11, -1),   8 => (11,  9, -1, -1));
    
    -- 12 interior walls in a 3x3 maze. Start with all the walls up.
    type Wall_List_Array is array (0 .. 11) of Boolean;
    wallList : Wall_List_Array := (others => True);
    
    -- Variables for maze processing
    currentInteriorWall : Integer;
    cellIndex : Integer;
    wallToRemove : Integer;
    wallRemoved : Boolean;
    direction : Integer;
    startTime : Time;
    
begin
    -- Print initial maze
    Put_Line ( "Initial Maze:" );
    
    -- Print out the maze, this is a rather painful copy/paste job without loops and functions.
    -- Note Put is meant to print a single character with no newlines.
    -- Horizontal walls above row 1 - All are exterior walls, no conditions.
    currentInteriorWall := 0;
    
    -- +-+-+-+
    Put ( '+' );
    Put ( '-' );
    Put ( '+' );
    Put ( '-' );
    Put ( '+' );
    Put ( '-' );
    Put ( '+' );
    New_Line;
    
    -- Vertical walls and cells row 1.
    -- The left and right vertical walls are exterior, always up.
    -- | | | |
    Put ( '|' );
    Put ( ' ' );
    if wallList (currentInteriorWall) = True then
        Put ( '|' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( ' ' );
    if wallList (currentInteriorWall) = True then
        Put ( '|' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( ' ' );
    Put ( '|' );
    New_Line;
    
    -- Horizontal walls above row 2
    -- +-+-+-+
    Put ( '+' );
    if wallList (currentInteriorWall) = True then
        Put ( '-' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( '+' );
    if wallList (currentInteriorWall) = True then
        Put ( '-' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( '+' );
    if wallList (currentInteriorWall) = True then
        Put ( '-' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( '+' );
    New_Line;
    
    -- Vertical walls and cells row 2.
    -- The left and right vertical walls are exterior, always up.
    -- | | | |
    Put ( '|' );
    Put ( ' ' );
    if wallList (currentInteriorWall) = True then
        Put ( '|' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( ' ' );
    if wallList (currentInteriorWall) = True then
        Put ( '|' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( ' ' );
    Put ( '|' );
    New_Line;
    
    -- Horizontal walls above row 3
    -- +-+-+-+
    Put ( '+' );
    if wallList (currentInteriorWall) = True then
        Put ( '-' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( '+' );
    if wallList (currentInteriorWall) = True then
        Put ( '-' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( '+' );
    if wallList (currentInteriorWall) = True then
        Put ( '-' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( '+' );
    New_Line;
    
    -- Vertical walls and cells row 3.
    -- The left and right vertical walls are exterior, always up.
    -- | | | |
    Put ( '|' );
    Put ( ' ' );
    if wallList (currentInteriorWall) = True then
        Put ( '|' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( ' ' );
    if wallList (currentInteriorWall) = True then
        Put ( '|' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( ' ' );
    Put ( '|' );
    New_Line;
    
    -- Horizontal walls below row 3 - All are exterior walls, no conditions.
    -- +-+-+-+
    Put ( '+' );
    Put ( '-' );
    Put ( '+' );
    Put ( '-' );
    Put ( '+' );
    Put ( '-' );
    Put ( '+' );
    New_Line;
    
    -- Now process each cell 0-8
    -- Cell 0
    New_Line;
    Put_Line ( "Removing wall from cell 0:" );
    
    -- Remove wall code:
    -- Remove a cell wall if possible for cell 0
    cellIndex := 0;
    
    wallRemoved := False;
    direction := Left;
    
    -- If the wall in this direction is NOT an exterior wall
    if cellToWallLUT (cellIndex)(direction) >= 0 then
        wallToRemove := cellToWallLUT (cellIndex)(direction);
        -- If this wall has not been already removed
        if wallList (wallToRemove) = True then
            -- Remove the wall and indicate a wall was successfully removed
            wallList (wallToRemove) := False;
            wallRemoved := True;
        end if;
    end if;
    
    -- If we haven't removed a wall yet, try to remove one in the next direction
    -- Check the guard, have we removed one wall
    if wallRemoved = False then
        direction := direction + 1;
        -- If the wall in this direction is NOT an exterior wall
        if cellToWallLUT (cellIndex)(direction) >= 0 then
            wallToRemove := cellToWallLUT (cellIndex)(direction);
            -- If this wall has not been already removed
            if wallList (wallToRemove) = True then
                -- Remove the wall and indicate a wall was successfully removed
                wallList (wallToRemove) := False;
                wallRemoved := True;
            end if;
        end if;
    end if;
    
    -- If we haven't removed a wall yet, try to remove one in the next direction
    -- Check the guard, have we removed one wall
    if wallRemoved = False then
        direction := direction + 1;
        -- If the wall in this direction is NOT an exterior wall
        if cellToWallLUT (cellIndex)(direction) >= 0 then
            wallToRemove := cellToWallLUT (cellIndex)(direction);
            -- If this wall has not been already removed
            if wallList (wallToRemove) = True then
                -- Remove the wall and indicate a wall was successfully removed
                wallList (wallToRemove) := False;
                wallRemoved := True;
            end if;
        end if;
    end if;
    
    -- If we haven't removed a wall yet, try to remove one in the next direction
    -- Check the guard, have we removed one wall
    if wallRemoved = False then
        direction := direction + 1;
        -- If the wall in this direction is NOT an exterior wall
        if cellToWallLUT (cellIndex)(direction) >= 0 then
            wallToRemove := cellToWallLUT (cellIndex)(direction);
            -- If this wall has not been already removed
            if wallList (wallToRemove) = True then
                -- Remove the wall and indicate a wall was successfully removed
                wallList (wallToRemove) := False;
                wallRemoved := True;
            end if;
        end if;
    end if;
    
    -- Print maze after cell 0
    -- Simple delay using busy wait (Ada's delay is simpler but this mimics the original)
    startTime := Clock;
    while Clock - startTime < 0.5 loop
        null;
    end loop;
    
    -- Print maze (duplicate of above print code)
    currentInteriorWall := 0;
    
    -- +-+-+-+
    Put ( '+' );
    Put ( '-' );
    Put ( '+' );
    Put ( '-' );
    Put ( '+' );
    Put ( '-' );
    Put ( '+' );
    New_Line;
    
    -- Row 1 vertical walls and cells
    Put ( '|' );
    Put ( ' ' );
    if wallList (currentInteriorWall) = True then
        Put ( '|' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( ' ' );
    if wallList (currentInteriorWall) = True then
        Put ( '|' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( ' ' );
    Put ( '|' );
    New_Line;
    
    -- Row 2 horizontal walls
    Put ( '+' );
    if wallList (currentInteriorWall) = True then
        Put ( '-' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( '+' );
    if wallList (currentInteriorWall) = True then
        Put ( '-' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( '+' );
    if wallList (currentInteriorWall) = True then
        Put ( '-' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( '+' );
    New_Line;
    
    -- Row 2 vertical walls and cells
    Put ( '|' );
    Put ( ' ' );
    if wallList (currentInteriorWall) = True then
        Put ( '|' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( ' ' );
    if wallList (currentInteriorWall) = True then
        Put ( '|' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( ' ' );
    Put ( '|' );
    New_Line;
    
    -- Row 3 horizontal walls
    Put ( '+' );
    if wallList (currentInteriorWall) = True then
        Put ( '-' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( '+' );
    if wallList (currentInteriorWall) = True then
        Put ( '-' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( '+' );
    if wallList (currentInteriorWall) = True then
        Put ( '-' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( '+' );
    New_Line;
    
    -- Row 3 vertical walls and cells
    Put ( '|' );
    Put ( ' ' );
    if wallList (currentInteriorWall) = True then
        Put ( '|' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( ' ' );
    if wallList (currentInteriorWall) = True then
        Put ( '|' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( ' ' );
    Put ( '|' );
    New_Line;
    
    -- Bottom border
    Put ( '+' );
    Put ( '-' );
    Put ( '+' );
    Put ( '-' );
    Put ( '+' );
    Put ( '-' );
    Put ( '+' );
    New_Line;
    
    -- Cell 1
    New_Line;
    Put_Line ( "Removing wall from cell 1:" );
    
    -- Remove wall code:
    -- Remove a cell wall if possible for cell 1
    cellIndex := 1;
    
    wallRemoved := False;
    direction := Left;
    
    -- If the wall in this direction is NOT an exterior wall
    if cellToWallLUT (cellIndex)(direction) >= 0 then
        wallToRemove := cellToWallLUT (cellIndex)(direction);
        -- If this wall has not been already removed
        if wallList (wallToRemove) = True then
            -- Remove the wall and indicate a wall was successfully removed
            wallList (wallToRemove) := False;
            wallRemoved := True;
        end if;
    end if;
    
    -- If we haven't removed a wall yet, try to remove one in the next direction
    -- Check the guard, have we removed one wall
    if wallRemoved = False then
        direction := direction + 1;
        -- If the wall in this direction is NOT an exterior wall
        if cellToWallLUT (cellIndex)(direction) >= 0 then
            wallToRemove := cellToWallLUT (cellIndex)(direction);
            -- If this wall has not been already removed
            if wallList (wallToRemove) = True then
                -- Remove the wall and indicate a wall was successfully removed
                wallList (wallToRemove) := False;
                wallRemoved := True;
            end if;
        end if;
    end if;
    
    -- If we haven't removed a wall yet, try to remove one in the next direction
    -- Check the guard, have we removed one wall
    if wallRemoved = False then
        direction := direction + 1;
        -- If the wall in this direction is NOT an exterior wall
        if cellToWallLUT (cellIndex)(direction) >= 0 then
            wallToRemove := cellToWallLUT (cellIndex)(direction);
            -- If this wall has not been already removed
            if wallList (wallToRemove) = True then
                -- Remove the wall and indicate a wall was successfully removed
                wallList (wallToRemove) := False;
                wallRemoved := True;
            end if;
        end if;
    end if;
    
    -- If we haven't removed a wall yet, try to remove one in the next direction
    -- Check the guard, have we removed one wall
    if wallRemoved = False then
        direction := direction + 1;
        -- If the wall in this direction is NOT an exterior wall
        if cellToWallLUT (cellIndex)(direction) >= 0 then
            wallToRemove := cellToWallLUT (cellIndex)(direction);
            -- If this wall has not been already removed
            if wallList (wallToRemove) = True then
                -- Remove the wall and indicate a wall was successfully removed
                wallList (wallToRemove) := False;
                wallRemoved := True;
            end if;
        end if;
    end if;
    
    -- Print maze after cell 1
    -- Simple delay using busy wait (Ada's delay is simpler but this mimics the original)
    startTime := Clock;
    while Clock - startTime < 0.5 loop
        null;
    end loop;
    
    -- Print maze (duplicate of above print code)
    currentInteriorWall := 0;
    
    -- +-+-+-+
    Put ( '+' );
    Put ( '-' );
    Put ( '+' );
    Put ( '-' );
    Put ( '+' );
    Put ( '-' );
    Put ( '+' );
    New_Line;
    
    -- Row 1 vertical walls and cells
    Put ( '|' );
    Put ( ' ' );
    if wallList (currentInteriorWall) = True then
        Put ( '|' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( ' ' );
    if wallList (currentInteriorWall) = True then
        Put ( '|' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( ' ' );
    Put ( '|' );
    New_Line;
    
    -- Row 2 horizontal walls
    Put ( '+' );
    if wallList (currentInteriorWall) = True then
        Put ( '-' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( '+' );
    if wallList (currentInteriorWall) = True then
        Put ( '-' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( '+' );
    if wallList (currentInteriorWall) = True then
        Put ( '-' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( '+' );
    New_Line;
    
    -- Row 2 vertical walls and cells
    Put ( '|' );
    Put ( ' ' );
    if wallList (currentInteriorWall) = True then
        Put ( '|' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( ' ' );
    if wallList (currentInteriorWall) = True then
        Put ( '|' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( ' ' );
    Put ( '|' );
    New_Line;
    
    -- Row 3 horizontal walls
    Put ( '+' );
    if wallList (currentInteriorWall) = True then
        Put ( '-' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( '+' );
    if wallList (currentInteriorWall) = True then
        Put ( '-' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( '+' );
    if wallList (currentInteriorWall) = True then
        Put ( '-' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( '+' );
    New_Line;
    
    -- Row 3 vertical walls and cells
    Put ( '|' );
    Put ( ' ' );
    if wallList (currentInteriorWall) = True then
        Put ( '|' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( ' ' );
    if wallList (currentInteriorWall) = True then
        Put ( '|' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( ' ' );
    Put ( '|' );
    New_Line;
    
    -- Bottom border
    Put ( '+' );
    Put ( '-' );
    Put ( '+' );
    Put ( '-' );
    Put ( '+' );
    Put ( '-' );
    Put ( '+' );
    New_Line;
    
    -- Cell 2
    New_Line;
    Put_Line ( "Removing wall from cell 2:" );
    
    -- Remove wall code:
    -- Remove a cell wall if possible for cell 2
    cellIndex := 2;
    
    wallRemoved := False;
    direction := Left;
    
    -- If the wall in this direction is NOT an exterior wall
    if cellToWallLUT (cellIndex)(direction) >= 0 then
        wallToRemove := cellToWallLUT (cellIndex)(direction);
        -- If this wall has not been already removed
        if wallList (wallToRemove) = True then
            -- Remove the wall and indicate a wall was successfully removed
            wallList (wallToRemove) := False;
            wallRemoved := True;
        end if;
    end if;
    
    -- If we haven't removed a wall yet, try to remove one in the next direction
    -- Check the guard, have we removed one wall
    if wallRemoved = False then
        direction := direction + 1;
        -- If the wall in this direction is NOT an exterior wall
        if cellToWallLUT (cellIndex)(direction) >= 0 then
            wallToRemove := cellToWallLUT (cellIndex)(direction);
            -- If this wall has not been already removed
            if wallList (wallToRemove) = True then
                -- Remove the wall and indicate a wall was successfully removed
                wallList (wallToRemove) := False;
                wallRemoved := True;
            end if;
        end if;
    end if;
    
    -- If we haven't removed a wall yet, try to remove one in the next direction
    -- Check the guard, have we removed one wall
    if wallRemoved = False then
        direction := direction + 1;
        -- If the wall in this direction is NOT an exterior wall
        if cellToWallLUT (cellIndex)(direction) >= 0 then
            wallToRemove := cellToWallLUT (cellIndex)(direction);
            -- If this wall has not been already removed
            if wallList (wallToRemove) = True then
                -- Remove the wall and indicate a wall was successfully removed
                wallList (wallToRemove) := False;
                wallRemoved := True;
            end if;
        end if;
    end if;
    
    -- If we haven't removed a wall yet, try to remove one in the next direction
    -- Check the guard, have we removed one wall
    if wallRemoved = False then
        direction := direction + 1;
        -- If the wall in this direction is NOT an exterior wall
        if cellToWallLUT (cellIndex)(direction) >= 0 then
            wallToRemove := cellToWallLUT (cellIndex)(direction);
            -- If this wall has not been already removed
            if wallList (wallToRemove) = True then
                -- Remove the wall and indicate a wall was successfully removed
                wallList (wallToRemove) := False;
                wallRemoved := True;
            end if;
        end if;
    end if;
    
    -- Print maze after cell 2
    -- Simple delay using busy wait (Ada's delay is simpler but this mimics the original)
    startTime := Clock;
    while Clock - startTime < 0.5 loop
        null;
    end loop;
    
    -- Print maze (duplicate of above print code)
    currentInteriorWall := 0;
    
    -- +-+-+-+
    Put ( '+' );
    Put ( '-' );
    Put ( '+' );
    Put ( '-' );
    Put ( '+' );
    Put ( '-' );
    Put ( '+' );
    New_Line;
    
    -- Row 1 vertical walls and cells
    Put ( '|' );
    Put ( ' ' );
    if wallList (currentInteriorWall) = True then
        Put ( '|' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( ' ' );
    if wallList (currentInteriorWall) = True then
        Put ( '|' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( ' ' );
    Put ( '|' );
    New_Line;
    
    -- Row 2 horizontal walls
    Put ( '+' );
    if wallList (currentInteriorWall) = True then
        Put ( '-' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( '+' );
    if wallList (currentInteriorWall) = True then
        Put ( '-' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( '+' );
    if wallList (currentInteriorWall) = True then
        Put ( '-' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( '+' );
    New_Line;
    
    -- Row 2 vertical walls and cells
    Put ( '|' );
    Put ( ' ' );
    if wallList (currentInteriorWall) = True then
        Put ( '|' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( ' ' );
    if wallList (currentInteriorWall) = True then
        Put ( '|' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( ' ' );
    Put ( '|' );
    New_Line;
    
    -- Row 3 horizontal walls
    Put ( '+' );
    if wallList (currentInteriorWall) = True then
        Put ( '-' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( '+' );
    if wallList (currentInteriorWall) = True then
        Put ( '-' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( '+' );
    if wallList (currentInteriorWall) = True then
        Put ( '-' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( '+' );
    New_Line;
    
    -- Row 3 vertical walls and cells
    Put ( '|' );
    Put ( ' ' );
    if wallList (currentInteriorWall) = True then
        Put ( '|' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( ' ' );
    if wallList (currentInteriorWall) = True then
        Put ( '|' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( ' ' );
    Put ( '|' );
    New_Line;
    
    -- Bottom border
    Put ( '+' );
    Put ( '-' );
    Put ( '+' );
    Put ( '-' );
    Put ( '+' );
    Put ( '-' );
    Put ( '+' );
    New_Line;
    
    -- Cell 3
    New_Line;
    Put_Line ( "Removing wall from cell 3:" );
    
    -- Remove wall code:
    -- Remove a cell wall if possible for cell 3
    cellIndex := 3;
    
    wallRemoved := False;
    direction := Left;
    
    -- If the wall in this direction is NOT an exterior wall
    if cellToWallLUT (cellIndex)(direction) >= 0 then
        wallToRemove := cellToWallLUT (cellIndex)(direction);
        -- If this wall has not been already removed
        if wallList (wallToRemove) = True then
            -- Remove the wall and indicate a wall was successfully removed
            wallList (wallToRemove) := False;
            wallRemoved := True;
        end if;
    end if;
    
    -- If we haven't removed a wall yet, try to remove one in the next direction
    -- Check the guard, have we removed one wall
    if wallRemoved = False then
        direction := direction + 1;
        -- If the wall in this direction is NOT an exterior wall
        if cellToWallLUT (cellIndex)(direction) >= 0 then
            wallToRemove := cellToWallLUT (cellIndex)(direction);
            -- If this wall has not been already removed
            if wallList (wallToRemove) = True then
                -- Remove the wall and indicate a wall was successfully removed
                wallList (wallToRemove) := False;
                wallRemoved := True;
            end if;
        end if;
    end if;
    
    -- If we haven't removed a wall yet, try to remove one in the next direction
    -- Check the guard, have we removed one wall
    if wallRemoved = False then
        direction := direction + 1;
        -- If the wall in this direction is NOT an exterior wall
        if cellToWallLUT (cellIndex)(direction) >= 0 then
            wallToRemove := cellToWallLUT (cellIndex)(direction);
            -- If this wall has not been already removed
            if wallList (wallToRemove) = True then
                -- Remove the wall and indicate a wall was successfully removed
                wallList (wallToRemove) := False;
                wallRemoved := True;
            end if;
        end if;
    end if;
    
    -- If we haven't removed a wall yet, try to remove one in the next direction
    -- Check the guard, have we removed one wall
    if wallRemoved = False then
        direction := direction + 1;
        -- If the wall in this direction is NOT an exterior wall
        if cellToWallLUT (cellIndex)(direction) >= 0 then
            wallToRemove := cellToWallLUT (cellIndex)(direction);
            -- If this wall has not been already removed
            if wallList (wallToRemove) = True then
                -- Remove the wall and indicate a wall was successfully removed
                wallList (wallToRemove) := False;
                wallRemoved := True;
            end if;
        end if;
    end if;
    
    -- Print maze after cell 3
    -- Simple delay using busy wait (Ada's delay is simpler but this mimics the original)
    startTime := Clock;
    while Clock - startTime < 0.5 loop
        null;
    end loop;
    
    -- Print maze (duplicate of above print code)
    currentInteriorWall := 0;
    
    -- +-+-+-+
    Put ( '+' );
    Put ( '-' );
    Put ( '+' );
    Put ( '-' );
    Put ( '+' );
    Put ( '-' );
    Put ( '+' );
    New_Line;
    
    -- Row 1 vertical walls and cells
    Put ( '|' );
    Put ( ' ' );
    if wallList (currentInteriorWall) = True then
        Put ( '|' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( ' ' );
    if wallList (currentInteriorWall) = True then
        Put ( '|' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( ' ' );
    Put ( '|' );
    New_Line;
    
    -- Row 2 horizontal walls
    Put ( '+' );
    if wallList (currentInteriorWall) = True then
        Put ( '-' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( '+' );
    if wallList (currentInteriorWall) = True then
        Put ( '-' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( '+' );
    if wallList (currentInteriorWall) = True then
        Put ( '-' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( '+' );
    New_Line;
    
    -- Row 2 vertical walls and cells
    Put ( '|' );
    Put ( ' ' );
    if wallList (currentInteriorWall) = True then
        Put ( '|' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( ' ' );
    if wallList (currentInteriorWall) = True then
        Put ( '|' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( ' ' );
    Put ( '|' );
    New_Line;
    
    -- Row 3 horizontal walls
    Put ( '+' );
    if wallList (currentInteriorWall) = True then
        Put ( '-' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( '+' );
    if wallList (currentInteriorWall) = True then
        Put ( '-' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( '+' );
    if wallList (currentInteriorWall) = True then
        Put ( '-' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( '+' );
    New_Line;
    
    -- Row 3 vertical walls and cells
    Put ( '|' );
    Put ( ' ' );
    if wallList (currentInteriorWall) = True then
        Put ( '|' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( ' ' );
    if wallList (currentInteriorWall) = True then
        Put ( '|' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( ' ' );
    Put ( '|' );
    New_Line;
    
    -- Bottom border
    Put ( '+' );
    Put ( '-' );
    Put ( '+' );
    Put ( '-' );
    Put ( '+' );
    Put ( '-' );
    Put ( '+' );
    New_Line;
    
    -- Cell 4
    New_Line;
    Put_Line ( "Removing wall from cell 4:" );
    
    -- Remove wall code:
    -- Remove a cell wall if possible for cell 4
    cellIndex := 4;
    
    wallRemoved := False;
    direction := Left;
    
    -- If the wall in this direction is NOT an exterior wall
    if cellToWallLUT (cellIndex)(direction) >= 0 then
        wallToRemove := cellToWallLUT (cellIndex)(direction);
        -- If this wall has not been already removed
        if wallList (wallToRemove) = True then
            -- Remove the wall and indicate a wall was successfully removed
            wallList (wallToRemove) := False;
            wallRemoved := True;
        end if;
    end if;
    
    -- If we haven't removed a wall yet, try to remove one in the next direction
    -- Check the guard, have we removed one wall
    if wallRemoved = False then
        direction := direction + 1;
        -- If the wall in this direction is NOT an exterior wall
        if cellToWallLUT (cellIndex)(direction) >= 0 then
            wallToRemove := cellToWallLUT (cellIndex)(direction);
            -- If this wall has not been already removed
            if wallList (wallToRemove) = True then
                -- Remove the wall and indicate a wall was successfully removed
                wallList (wallToRemove) := False;
                wallRemoved := True;
            end if;
        end if;
    end if;
    
    -- If we haven't removed a wall yet, try to remove one in the next direction
    -- Check the guard, have we removed one wall
    if wallRemoved = False then
        direction := direction + 1;
        -- If the wall in this direction is NOT an exterior wall
        if cellToWallLUT (cellIndex)(direction) >= 0 then
            wallToRemove := cellToWallLUT (cellIndex)(direction);
            -- If this wall has not been already removed
            if wallList (wallToRemove) = True then
                -- Remove the wall and indicate a wall was successfully removed
                wallList (wallToRemove) := False;
                wallRemoved := True;
            end if;
        end if;
    end if;
    
    -- If we haven't removed a wall yet, try to remove one in the next direction
    -- Check the guard, have we removed one wall
    if wallRemoved = False then
        direction := direction + 1;
        -- If the wall in this direction is NOT an exterior wall
        if cellToWallLUT (cellIndex)(direction) >= 0 then
            wallToRemove := cellToWallLUT (cellIndex)(direction);
            -- If this wall has not been already removed
            if wallList (wallToRemove) = True then
                -- Remove the wall and indicate a wall was successfully removed
                wallList (wallToRemove) := False;
                wallRemoved := True;
            end if;
        end if;
    end if;
    
    -- Print maze after cell 4
    -- Simple delay using busy wait (Ada's delay is simpler but this mimics the original)
    startTime := Clock;
    while Clock - startTime < 0.5 loop
        null;
    end loop;
    
    -- Print maze (duplicate of above print code)
    currentInteriorWall := 0;
    
    -- +-+-+-+
    Put ( '+' );
    Put ( '-' );
    Put ( '+' );
    Put ( '-' );
    Put ( '+' );
    Put ( '-' );
    Put ( '+' );
    New_Line;
    
    -- Row 1 vertical walls and cells
    Put ( '|' );
    Put ( ' ' );
    if wallList (currentInteriorWall) = True then
        Put ( '|' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( ' ' );
    if wallList (currentInteriorWall) = True then
        Put ( '|' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( ' ' );
    Put ( '|' );
    New_Line;
    
    -- Row 2 horizontal walls
    Put ( '+' );
    if wallList (currentInteriorWall) = True then
        Put ( '-' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( '+' );
    if wallList (currentInteriorWall) = True then
        Put ( '-' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( '+' );
    if wallList (currentInteriorWall) = True then
        Put ( '-' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( '+' );
    New_Line;
    
    -- Row 2 vertical walls and cells
    Put ( '|' );
    Put ( ' ' );
    if wallList (currentInteriorWall) = True then
        Put ( '|' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( ' ' );
    if wallList (currentInteriorWall) = True then
        Put ( '|' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( ' ' );
    Put ( '|' );
    New_Line;
    
    -- Row 3 horizontal walls
    Put ( '+' );
    if wallList (currentInteriorWall) = True then
        Put ( '-' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( '+' );
    if wallList (currentInteriorWall) = True then
        Put ( '-' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( '+' );
    if wallList (currentInteriorWall) = True then
        Put ( '-' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( '+' );
    New_Line;
    
    -- Row 3 vertical walls and cells
    Put ( '|' );
    Put ( ' ' );
    if wallList (currentInteriorWall) = True then
        Put ( '|' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( ' ' );
    if wallList (currentInteriorWall) = True then
        Put ( '|' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( ' ' );
    Put ( '|' );
    New_Line;
    
    -- Bottom border
    Put ( '+' );
    Put ( '-' );
    Put ( '+' );
    Put ( '-' );
    Put ( '+' );
    Put ( '-' );
    Put ( '+' );
    New_Line;
    
    -- Cell 5
    New_Line;
    Put_Line ( "Removing wall from cell 5:" );
    
    -- Remove wall code:
    -- Remove a cell wall if possible for cell 5
    cellIndex := 5;
    
    wallRemoved := False;
    direction := Left;
    
    -- If the wall in this direction is NOT an exterior wall
    if cellToWallLUT (cellIndex)(direction) >= 0 then
        wallToRemove := cellToWallLUT (cellIndex)(direction);
        -- If this wall has not been already removed
        if wallList (wallToRemove) = True then
            -- Remove the wall and indicate a wall was successfully removed
            wallList (wallToRemove) := False;
            wallRemoved := True;
        end if;
    end if;
    
    -- If we haven't removed a wall yet, try to remove one in the next direction
    -- Check the guard, have we removed one wall
    if wallRemoved = False then
        direction := direction + 1;
        -- If the wall in this direction is NOT an exterior wall
        if cellToWallLUT (cellIndex)(direction) >= 0 then
            wallToRemove := cellToWallLUT (cellIndex)(direction);
            -- If this wall has not been already removed
            if wallList (wallToRemove) = True then
                -- Remove the wall and indicate a wall was successfully removed
                wallList (wallToRemove) := False;
                wallRemoved := True;
            end if;
        end if;
    end if;
    
    -- If we haven't removed a wall yet, try to remove one in the next direction
    -- Check the guard, have we removed one wall
    if wallRemoved = False then
        direction := direction + 1;
        -- If the wall in this direction is NOT an exterior wall
        if cellToWallLUT (cellIndex)(direction) >= 0 then
            wallToRemove := cellToWallLUT (cellIndex)(direction);
            -- If this wall has not been already removed
            if wallList (wallToRemove) = True then
                -- Remove the wall and indicate a wall was successfully removed
                wallList (wallToRemove) := False;
                wallRemoved := True;
            end if;
        end if;
    end if;
    
    -- If we haven't removed a wall yet, try to remove one in the next direction
    -- Check the guard, have we removed one wall
    if wallRemoved = False then
        direction := direction + 1;
        -- If the wall in this direction is NOT an exterior wall
        if cellToWallLUT (cellIndex)(direction) >= 0 then
            wallToRemove := cellToWallLUT (cellIndex)(direction);
            -- If this wall has not been already removed
            if wallList (wallToRemove) = True then
                -- Remove the wall and indicate a wall was successfully removed
                wallList (wallToRemove) := False;
                wallRemoved := True;
            end if;
        end if;
    end if;
    
    -- Print maze after cell 5
    -- Simple delay using busy wait (Ada's delay is simpler but this mimics the original)
    startTime := Clock;
    while Clock - startTime < 0.5 loop
        null;
    end loop;
    
    -- Print maze (duplicate of above print code)
    currentInteriorWall := 0;
    
    -- +-+-+-+
    Put ( '+' );
    Put ( '-' );
    Put ( '+' );
    Put ( '-' );
    Put ( '+' );
    Put ( '-' );
    Put ( '+' );
    New_Line;
    
    -- Row 1 vertical walls and cells
    Put ( '|' );
    Put ( ' ' );
    if wallList (currentInteriorWall) = True then
        Put ( '|' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( ' ' );
    if wallList (currentInteriorWall) = True then
        Put ( '|' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( ' ' );
    Put ( '|' );
    New_Line;
    
    -- Row 2 horizontal walls
    Put ( '+' );
    if wallList (currentInteriorWall) = True then
        Put ( '-' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( '+' );
    if wallList (currentInteriorWall) = True then
        Put ( '-' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( '+' );
    if wallList (currentInteriorWall) = True then
        Put ( '-' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( '+' );
    New_Line;
    
    -- Row 2 vertical walls and cells
    Put ( '|' );
    Put ( ' ' );
    if wallList (currentInteriorWall) = True then
        Put ( '|' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( ' ' );
    if wallList (currentInteriorWall) = True then
        Put ( '|' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( ' ' );
    Put ( '|' );
    New_Line;
    
    -- Row 3 horizontal walls
    Put ( '+' );
    if wallList (currentInteriorWall) = True then
        Put ( '-' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( '+' );
    if wallList (currentInteriorWall) = True then
        Put ( '-' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( '+' );
    if wallList (currentInteriorWall) = True then
        Put ( '-' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( '+' );
    New_Line;
    
    -- Row 3 vertical walls and cells
    Put ( '|' );
    Put ( ' ' );
    if wallList (currentInteriorWall) = True then
        Put ( '|' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( ' ' );
    if wallList (currentInteriorWall) = True then
        Put ( '|' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( ' ' );
    Put ( '|' );
    New_Line;
    
    -- Bottom border
    Put ( '+' );
    Put ( '-' );
    Put ( '+' );
    Put ( '-' );
    Put ( '+' );
    Put ( '-' );
    Put ( '+' );
    New_Line;
    
    -- Cell 6
    New_Line;
    Put_Line ( "Removing wall from cell 6:" );
    
    -- Remove wall code:
    -- Remove a cell wall if possible for cell 6
    cellIndex := 6;
    
    wallRemoved := False;
    direction := Left;
    
    -- If the wall in this direction is NOT an exterior wall
    if cellToWallLUT (cellIndex)(direction) >= 0 then
        wallToRemove := cellToWallLUT (cellIndex)(direction);
        -- If this wall has not been already removed
        if wallList (wallToRemove) = True then
            -- Remove the wall and indicate a wall was successfully removed
            wallList (wallToRemove) := False;
            wallRemoved := True;
        end if;
    end if;
    
    -- If we haven't removed a wall yet, try to remove one in the next direction
    -- Check the guard, have we removed one wall
    if wallRemoved = False then
        direction := direction + 1;
        -- If the wall in this direction is NOT an exterior wall
        if cellToWallLUT (cellIndex)(direction) >= 0 then
            wallToRemove := cellToWallLUT (cellIndex)(direction);
            -- If this wall has not been already removed
            if wallList (wallToRemove) = True then
                -- Remove the wall and indicate a wall was successfully removed
                wallList (wallToRemove) := False;
                wallRemoved := True;
            end if;
        end if;
    end if;
    
    -- If we haven't removed a wall yet, try to remove one in the next direction
    -- Check the guard, have we removed one wall
    if wallRemoved = False then
        direction := direction + 1;
        -- If the wall in this direction is NOT an exterior wall
        if cellToWallLUT (cellIndex)(direction) >= 0 then
            wallToRemove := cellToWallLUT (cellIndex)(direction);
            -- If this wall has not been already removed
            if wallList (wallToRemove) = True then
                -- Remove the wall and indicate a wall was successfully removed
                wallList (wallToRemove) := False;
                wallRemoved := True;
            end if;
        end if;
    end if;
    
    -- If we haven't removed a wall yet, try to remove one in the next direction
    -- Check the guard, have we removed one wall
    if wallRemoved = False then
        direction := direction + 1;
        -- If the wall in this direction is NOT an exterior wall
        if cellToWallLUT (cellIndex)(direction) >= 0 then
            wallToRemove := cellToWallLUT (cellIndex)(direction);
            -- If this wall has not been already removed
            if wallList (wallToRemove) = True then
                -- Remove the wall and indicate a wall was successfully removed
                wallList (wallToRemove) := False;
                wallRemoved := True;
            end if;
        end if;
    end if;
    
    -- Print maze after cell 6
    -- Simple delay using busy wait (Ada's delay is simpler but this mimics the original)
    startTime := Clock;
    while Clock - startTime < 0.5 loop
        null;
    end loop;
    
    -- Print maze (duplicate of above print code)
    currentInteriorWall := 0;
    
    -- +-+-+-+
    Put ( '+' );
    Put ( '-' );
    Put ( '+' );
    Put ( '-' );
    Put ( '+' );
    Put ( '-' );
    Put ( '+' );
    New_Line;
    
    -- Row 1 vertical walls and cells
    Put ( '|' );
    Put ( ' ' );
    if wallList (currentInteriorWall) = True then
        Put ( '|' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( ' ' );
    if wallList (currentInteriorWall) = True then
        Put ( '|' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( ' ' );
    Put ( '|' );
    New_Line;
    
    -- Row 2 horizontal walls
    Put ( '+' );
    if wallList (currentInteriorWall) = True then
        Put ( '-' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( '+' );
    if wallList (currentInteriorWall) = True then
        Put ( '-' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( '+' );
    if wallList (currentInteriorWall) = True then
        Put ( '-' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( '+' );
    New_Line;
    
    -- Row 2 vertical walls and cells
    Put ( '|' );
    Put ( ' ' );
    if wallList (currentInteriorWall) = True then
        Put ( '|' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( ' ' );
    if wallList (currentInteriorWall) = True then
        Put ( '|' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( ' ' );
    Put ( '|' );
    New_Line;
    
    -- Row 3 horizontal walls
    Put ( '+' );
    if wallList (currentInteriorWall) = True then
        Put ( '-' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( '+' );
    if wallList (currentInteriorWall) = True then
        Put ( '-' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( '+' );
    if wallList (currentInteriorWall) = True then
        Put ( '-' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( '+' );
    New_Line;
    
    -- Row 3 vertical walls and cells
    Put ( '|' );
    Put ( ' ' );
    if wallList (currentInteriorWall) = True then
        Put ( '|' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( ' ' );
    if wallList (currentInteriorWall) = True then
        Put ( '|' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( ' ' );
    Put ( '|' );
    New_Line;
    
    -- Bottom border
    Put ( '+' );
    Put ( '-' );
    Put ( '+' );
    Put ( '-' );
    Put ( '+' );
    Put ( '-' );
    Put ( '+' );
    New_Line;
    
    -- Cell 7
    New_Line;
    Put_Line ( "Removing wall from cell 7:" );
    
    -- Remove wall code:
    -- Remove a cell wall if possible for cell 7
    cellIndex := 7;
    
    wallRemoved := False;
    direction := Left;
    
    -- If the wall in this direction is NOT an exterior wall
    if cellToWallLUT (cellIndex)(direction) >= 0 then
        wallToRemove := cellToWallLUT (cellIndex)(direction);
        -- If this wall has not been already removed
        if wallList (wallToRemove) = True then
            -- Remove the wall and indicate a wall was successfully removed
            wallList (wallToRemove) := False;
            wallRemoved := True;
        end if;
    end if;
    
    -- If we haven't removed a wall yet, try to remove one in the next direction
    -- Check the guard, have we removed one wall
    if wallRemoved = False then
        direction := direction + 1;
        -- If the wall in this direction is NOT an exterior wall
        if cellToWallLUT (cellIndex)(direction) >= 0 then
            wallToRemove := cellToWallLUT (cellIndex)(direction);
            -- If this wall has not been already removed
            if wallList (wallToRemove) = True then
                -- Remove the wall and indicate a wall was successfully removed
                wallList (wallToRemove) := False;
                wallRemoved := True;
            end if;
        end if;
    end if;
    
    -- If we haven't removed a wall yet, try to remove one in the next direction
    -- Check the guard, have we removed one wall
    if wallRemoved = False then
        direction := direction + 1;
        -- If the wall in this direction is NOT an exterior wall
        if cellToWallLUT (cellIndex)(direction) >= 0 then
            wallToRemove := cellToWallLUT (cellIndex)(direction);
            -- If this wall has not been already removed
            if wallList (wallToRemove) = True then
                -- Remove the wall and indicate a wall was successfully removed
                wallList (wallToRemove) := False;
                wallRemoved := True;
            end if;
        end if;
    end if;
    
    -- If we haven't removed a wall yet, try to remove one in the next direction
    -- Check the guard, have we removed one wall
    if wallRemoved = False then
        direction := direction + 1;
        -- If the wall in this direction is NOT an exterior wall
        if cellToWallLUT (cellIndex)(direction) >= 0 then
            wallToRemove := cellToWallLUT (cellIndex)(direction);
            -- If this wall has not been already removed
            if wallList (wallToRemove) = True then
                -- Remove the wall and indicate a wall was successfully removed
                wallList (wallToRemove) := False;
                wallRemoved := True;
            end if;
        end if;
    end if;
    
    -- Print maze after cell 7
    -- Simple delay using busy wait (Ada's delay is simpler but this mimics the original)
    startTime := Clock;
    while Clock - startTime < 0.5 loop
        null;
    end loop;
    
    -- Print maze (duplicate of above print code)
    currentInteriorWall := 0;
    
    -- +-+-+-+
    Put ( '+' );
    Put ( '-' );
    Put ( '+' );
    Put ( '-' );
    Put ( '+' );
    Put ( '-' );
    Put ( '+' );
    New_Line;
    
    -- Row 1 vertical walls and cells
    Put ( '|' );
    Put ( ' ' );
    if wallList (currentInteriorWall) = True then
        Put ( '|' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( ' ' );
    if wallList (currentInteriorWall) = True then
        Put ( '|' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( ' ' );
    Put ( '|' );
    New_Line;
    
    -- Row 2 horizontal walls
    Put ( '+' );
    if wallList (currentInteriorWall) = True then
        Put ( '-' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( '+' );
    if wallList (currentInteriorWall) = True then
        Put ( '-' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( '+' );
    if wallList (currentInteriorWall) = True then
        Put ( '-' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( '+' );
    New_Line;
    
    -- Row 2 vertical walls and cells
    Put ( '|' );
    Put ( ' ' );
    if wallList (currentInteriorWall) = True then
        Put ( '|' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( ' ' );
    if wallList (currentInteriorWall) = True then
        Put ( '|' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( ' ' );
    Put ( '|' );
    New_Line;
    
    -- Row 3 horizontal walls
    Put ( '+' );
    if wallList (currentInteriorWall) = True then
        Put ( '-' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( '+' );
    if wallList (currentInteriorWall) = True then
        Put ( '-' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( '+' );
    if wallList (currentInteriorWall) = True then
        Put ( '-' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( '+' );
    New_Line;
    
    -- Row 3 vertical walls and cells
    Put ( '|' );
    Put ( ' ' );
    if wallList (currentInteriorWall) = True then
        Put ( '|' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( ' ' );
    if wallList (currentInteriorWall) = True then
        Put ( '|' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( ' ' );
    Put ( '|' );
    New_Line;
    
    -- Bottom border
    Put ( '+' );
    Put ( '-' );
    Put ( '+' );
    Put ( '-' );
    Put ( '+' );
    Put ( '-' );
    Put ( '+' );
    New_Line;
    
    -- Cell 8
    New_Line;
    Put_Line ( "Removing wall from cell 8:" );
    
    -- Remove wall code:
    -- Remove a cell wall if possible for cell 8
    cellIndex := 8;
    
    wallRemoved := False;
    direction := Left;
    
    -- If the wall in this direction is NOT an exterior wall
    if cellToWallLUT (cellIndex)(direction) >= 0 then
        wallToRemove := cellToWallLUT (cellIndex)(direction);
        -- If this wall has not been already removed
        if wallList (wallToRemove) = True then
            -- Remove the wall and indicate a wall was successfully removed
            wallList (wallToRemove) := False;
            wallRemoved := True;
        end if;
    end if;
    
    -- If we haven't removed a wall yet, try to remove one in the next direction
    -- Check the guard, have we removed one wall
    if wallRemoved = False then
        direction := direction + 1;
        -- If the wall in this direction is NOT an exterior wall
        if cellToWallLUT (cellIndex)(direction) >= 0 then
            wallToRemove := cellToWallLUT (cellIndex)(direction);
            -- If this wall has not been already removed
            if wallList (wallToRemove) = True then
                -- Remove the wall and indicate a wall was successfully removed
                wallList (wallToRemove) := False;
                wallRemoved := True;
            end if;
        end if;
    end if;
    
    -- If we haven't removed a wall yet, try to remove one in the next direction
    -- Check the guard, have we removed one wall
    if wallRemoved = False then
        direction := direction + 1;
        -- If the wall in this direction is NOT an exterior wall
        if cellToWallLUT (cellIndex)(direction) >= 0 then
            wallToRemove := cellToWallLUT (cellIndex)(direction);
            -- If this wall has not been already removed
            if wallList (wallToRemove) = True then
                -- Remove the wall and indicate a wall was successfully removed
                wallList (wallToRemove) := False;
                wallRemoved := True;
            end if;
        end if;
    end if;
    
    -- If we haven't removed a wall yet, try to remove one in the next direction
    -- Check the guard, have we removed one wall
    if wallRemoved = False then
        direction := direction + 1;
        -- If the wall in this direction is NOT an exterior wall
        if cellToWallLUT (cellIndex)(direction) >= 0 then
            wallToRemove := cellToWallLUT (cellIndex)(direction);
            -- If this wall has not been already removed
            if wallList (wallToRemove) = True then
                -- Remove the wall and indicate a wall was successfully removed
                wallList (wallToRemove) := False;
                wallRemoved := True;
            end if;
        end if;
    end if;
    
    -- Print maze after cell 8
    -- Simple delay using busy wait (Ada's delay is simpler but this mimics the original)
    startTime := Clock;
    while Clock - startTime < 0.5 loop
        null;
    end loop;
    
    -- Print maze (duplicate of above print code)
    currentInteriorWall := 0;
    
    -- +-+-+-+
    Put ( '+' );
    Put ( '-' );
    Put ( '+' );
    Put ( '-' );
    Put ( '+' );
    Put ( '-' );
    Put ( '+' );
    New_Line;
    
    -- Row 1 vertical walls and cells
    Put ( '|' );
    Put ( ' ' );
    if wallList (currentInteriorWall) = True then
        Put ( '|' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( ' ' );
    if wallList (currentInteriorWall) = True then
        Put ( '|' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( ' ' );
    Put ( '|' );
    New_Line;
    
    -- Row 2 horizontal walls
    Put ( '+' );
    if wallList (currentInteriorWall) = True then
        Put ( '-' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( '+' );
    if wallList (currentInteriorWall) = True then
        Put ( '-' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( '+' );
    if wallList (currentInteriorWall) = True then
        Put ( '-' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( '+' );
    New_Line;
    
    -- Row 2 vertical walls and cells
    Put ( '|' );
    Put ( ' ' );
    if wallList (currentInteriorWall) = True then
        Put ( '|' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( ' ' );
    if wallList (currentInteriorWall) = True then
        Put ( '|' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( ' ' );
    Put ( '|' );
    New_Line;
    
    -- Row 3 horizontal walls
    Put ( '+' );
    if wallList (currentInteriorWall) = True then
        Put ( '-' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( '+' );
    if wallList (currentInteriorWall) = True then
        Put ( '-' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( '+' );
    if wallList (currentInteriorWall) = True then
        Put ( '-' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( '+' );
    New_Line;
    
    -- Row 3 vertical walls and cells
    Put ( '|' );
    Put ( ' ' );
    if wallList (currentInteriorWall) = True then
        Put ( '|' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( ' ' );
    if wallList (currentInteriorWall) = True then
        Put ( '|' );
    else
        Put ( ' ' );
    end if;
    currentInteriorWall := currentInteriorWall + 1;
    Put ( ' ' );
    Put ( '|' );
    New_Line;
    
    -- Bottom border
    Put ( '+' );
    Put ( '-' );
    Put ( '+' );
    Put ( '-' );
    Put ( '+' );
    Put ( '-' );
    Put ( '+' );
    New_Line;
    
end MazeConditions;
