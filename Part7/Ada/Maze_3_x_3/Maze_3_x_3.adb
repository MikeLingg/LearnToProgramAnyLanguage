with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Discrete_Random;
with Ada.Calendar; use Ada.Calendar;

procedure Main is
   -- Define directions
   -- LEFT : constant := 0;
   -- UP : constant := 1;
   -- RIGHT : constant := 2;
   -- DOWN : constant := 3;

   FIRST_CELL : constant := 0;
   SECOND_CELL : constant := 1;
   
   NO_CELL : constant := -1;
   
   INTERIOR_WALL_COUNT : constant := 12;

   -- For each cell 0-8, indicate if a wall is exterior and cannot be removed ( -1 ) or its interior index for each of
   -- the four directions, LEFT, UP, RIGHT, DOWN.
   --cellToWallLUT : constant array ( 0..8, 0..3 ) of Integer := (
   --   ( -1, -1, 0, 2 ), ( 0, -1, 1, 3 ), ( 1, -1, -1, 4 ),
   --   ( -1, 2, 5, 7 ), ( 5, 3, 6, 8 ), ( 6, 4, -1, 9 ),
   --   ( -1, 7, 10, -1 ), ( 10, 8, 11, -1 ), ( 11, 9, -1, -1 )
   --);

   -- 12 interior walls in a 3x3 maze. Start with all the walls up.
   wallsUp : array ( 0..11 ) of Boolean := ( others => True );

   -- Identify the cells each wall connects.
   wallConnections : constant array ( 0..11, 0..1 ) of Integer := 
   (
      ( 0, 1 ), ( 1, 2 ), ( 0, 3 ), ( 1, 4 ), ( 2, 5 ), ( 3, 4 ),
      ( 4, 5 ), ( 3, 6 ), ( 4, 7 ), ( 5, 8 ), ( 6, 7 ), ( 7, 8 )
   );

   -- Identify which group each cell is a part of.
   cellToGroup : array ( 0..8 ) of Integer := ( 0, 1, 2, 3, 4, 5, 6, 7, 8 );

   -- Identify which cells are a part of each group
   groupCells : array ( 0..8, 0..8 ) of Integer := 
   (
      ( 0, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL ),
      ( 1, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL ),
      ( 2, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL ),
      ( 3, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL ),
      ( 4, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL ),
      ( 5, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL ),
      ( 6, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL ),
      ( 7, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL ),
      ( 8, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL )
   );

   -- Print maze code:
   -- Print out the maze, this is a less painful copy/paste job without functions, but better with loops.
   currentInteriorWall : Integer := 0;

   -- Random number generation
   subtype Wall_Range is Integer range 0..11;
   package Wall_Random is new Ada.Numerics.Discrete_Random ( Wall_Range );
   wallGenerator : Wall_Random.Generator;

   -- Create and randomize wall removal list
   wallRemoveList : array ( 0..11 ) of Integer := ( 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 );
   
   mazeComplete : Boolean := False;
   
   -- Variables for algorithm
   nextWallToCheck : Integer;
   firstCell : Integer;
   firstCellGroupIndex : Integer;
   secondCell : Integer;
   secondCellGroupIndex : Integer;
   nextEmptyFirstGroupIndex : Integer;
   cellToMove : Integer;

begin
   -- Print the horizontal walls above row 1 - All are exterior walls, no conditions.
   -- +-+-+-+
   -- One initial cell with + followed by 3 cells with -+
   Put ( "+" );
   for cellIndex in 0..2 loop
      Put ( "-" );
      Put ( "+" );
   end loop;
   New_Line;

   for rowIndex in 0..2 loop
      -- Vertical walls and cells row 1.
      -- The left and right vertical walls are exterior, always up.
      -- | | | |
      -- Or print one |, followed by 3 cells of <space>| where the |
      -- may be down ( <space> ).
      Put ( "|" );
      for cellIndex in 0..2 loop
         Put ( " " );

         -- Always print the right most vertical wall,
         -- if interior wall, print if the wall is up.
         if cellIndex = 2 or wallsUp ( currentInteriorWall ) = True then
            Put ( "|" );
         else
            Put ( " " );
         end if;
         if cellIndex < 2 then
            currentInteriorWall := currentInteriorWall + 1;
         end if;
      end loop;
      New_Line;

      -- One fewer horizontal wall than vertical
      if rowIndex < 2 then
         -- Horizontal walls above row rowIndex
         -- +-+-+-+
         Put ( "+" );
         for cellIndex in 0..2 loop
            if wallsUp ( currentInteriorWall ) = True then
               Put ( "-" );
            else
               Put ( " " );
            end if;
            Put ( "+" );
         end loop;
         New_Line;
      end if;

      currentInteriorWall := currentInteriorWall + 1;
   end loop;

   -- Horizontal walls below row 3 - All are exterior walls, no conditions.
   -- +-+-+-+
   Put ( "+" );
   for cellIndex in 0..2 loop
      Put ( "-" );
      Put ( "+" );
   end loop;
   New_Line;

   -- Initialize random number generator
   Wall_Random.Reset ( wallGenerator );
   
   -- Fisher-Yates shuffle algorithm
   for shuffleIndex in reverse 1..11 loop
      declare
         otherIndex : Integer := Wall_Random.Random ( wallGenerator ) mod ( shuffleIndex + 1 );
         temp : Integer;
      begin
         -- Swap wallRemoveList ( shuffleIndex ) with wallRemoveList ( otherIndex )
         temp := wallRemoveList ( shuffleIndex );
         wallRemoveList ( shuffleIndex ) := wallRemoveList ( otherIndex );
         wallRemoveList ( otherIndex ) := temp;
      end;
   end loop;

   -- Remove wall code:
   -- Now that we have loops we can implement Kruskal's algorithm.
   -- The simple description of the algorithm is first place each 
   -- cell in its own group.  Then process all walls in random order,
   -- if the cells on either side of the wall are in separate groups, 
   -- remove the wall and merge the groups.  Repeat until all 
   -- cells are now in the same group.
   for removeWallIndex in 0..INTERIOR_WALL_COUNT - 1 loop
      nextWallToCheck := wallRemoveList ( removeWallIndex );

      -- If the two cells connected to this wall are not part 
      -- of the same group, remove the wall and merge the 
      -- groups.
      firstCell := wallConnections ( nextWallToCheck, FIRST_CELL );
      firstCellGroupIndex := cellToGroup ( firstCell );
      secondCell := wallConnections ( nextWallToCheck, SECOND_CELL );
      secondCellGroupIndex := cellToGroup ( secondCell );
      
      if firstCellGroupIndex /= secondCellGroupIndex then
         wallsUp ( nextWallToCheck ) := False;

         -- Loop through the indices of all cells in the first 
         -- group until we find a NO_CELL indicating no cell here.
         nextEmptyFirstGroupIndex := 0;
         for cellIndex in 0..8 loop
            if groupCells ( firstCellGroupIndex, cellIndex ) = NO_CELL then
               nextEmptyFirstGroupIndex := cellIndex;
               exit;
            end if;
         end loop;

         -- Loop through the indices of all cells in the second group,
         -- move each cell to the first group, and set that cell's 
         -- group to the first group index.
         for groupCellIndex in reverse 0..8 loop
            -- Skip until we reach valid cells
            if groupCells ( secondCellGroupIndex, groupCellIndex ) /= NO_CELL then
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
               -- Remove the cell from the second group ( set the
               -- array entry to NO_CELL )
               groupCells ( secondCellGroupIndex, groupCellIndex ) := NO_CELL;
               
               if nextEmptyFirstGroupIndex >= 9 then
                  mazeComplete := True;
               end if;
            end if;
         end loop;

         -- Print maze code ( copied from above ):
         delay 0.5; -- Sleep for 500 milliseconds
         currentInteriorWall := 0;

         New_Line;
         Put ( "+" );
         for cellIndex in 0..2 loop
            Put ( "-" );
            Put ( "+" );
         end loop;
         New_Line;

         for rowIndex in 0..2 loop
            Put ( "|" );
            for cellIndex in 0..2 loop
               Put ( " " );

               if cellIndex = 2 or else wallsUp ( currentInteriorWall ) = True then
                  Put ( "|" );
               else
                  Put ( " " );
               end if;
               if cellIndex < 2 then
                  currentInteriorWall := currentInteriorWall + 1;
               end if;
            end loop;
            New_Line;

            if rowIndex < 2 then
               Put ( "+" );
               for cellIndex in 0..2 loop
                  if wallsUp ( currentInteriorWall ) = True then
                     Put ( "-" );
                  else
                     Put ( " " );
                  end if;
                  Put ( "+" );

                  currentInteriorWall := currentInteriorWall + 1;
               end loop;
               New_Line;
            end if;
         end loop;

         Put ( "+" );
         for cellIndex in 0..2 loop
            Put ( "-" );
            Put ( "+" );
         end loop;
         New_Line;
      end if;

      if mazeComplete = True then
         exit;
      end if;
   end loop;

end Main;
