with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Calendar; use Ada.Calendar;

procedure Maze_N_x_N is
   FIRST_CELL : constant Integer := 0;
   SECOND_CELL : constant Integer := 1;
   NO_CELL : constant Integer := -1;
   
   type booleanArray is array ( Integer range <> ) of Boolean;
   type integerArray is array ( Integer range <> ) of Integer;
   type integerMatrix is array ( Integer range <>, Integer range <> ) of Integer;
   
   mazeColumns : Integer := 0;
   mazeRows : Integer := 0;
   interiorWallCount : Integer;
   
begin
   -- Get maze dimensions
   while ( mazeColumns <= 0 ) loop
      Put ( "Please enter number of columns for maze, must be greater than 1: " );
      declare
         inputLine : String ( 1..100 );
         lastChar : Natural;
      begin
         Get_Line ( inputLine, lastChar );
         begin
            mazeColumns := Integer'Value ( inputLine ( 1..lastChar ) );
         exception
            when others =>
               mazeColumns := 0;
         end;
      end;
   end loop;
   
   while ( mazeRows <= 0 ) loop
      Put ( "Please enter number of rows for maze, must be greater than 1: " );
      declare
         inputLine : String ( 1..100 );
         lastChar : Natural;
      begin
         Get_Line ( inputLine, lastChar );
         begin
            mazeRows := Integer'Value ( inputLine ( 1..lastChar ) );
         exception
            when others =>
               mazeRows := 0;
         end;
      end;
   end loop;
   
   interiorWallCount := mazeRows * ( mazeColumns - 1 ) + ( mazeRows - 1 ) * mazeColumns;
   
   declare
      wallsUp : booleanArray ( 0 .. interiorWallCount - 1 ) := ( others => True );
      wallConnections : integerMatrix ( 0 .. interiorWallCount - 1, 0 .. 1 );
      cellToGroup : integerArray ( 0 .. mazeRows * mazeColumns - 1 );
      groupCells : integerMatrix ( 0 .. mazeColumns * mazeRows - 1, 0 .. mazeColumns * mazeRows - 1 );
      wallRemoveList : integerArray ( 0 .. interiorWallCount - 1 );
      
      wallIndex : Integer := 0;
      mazeComplete : Boolean := False;
      
      -- Simple random number generator using time
      randomSeed : Integer := Integer ( Seconds ( Clock ) );
      
   begin
      -- Build wall connections
      for rowIndex in 0 .. mazeRows - 1 loop
         declare
            First_Cell_In_Row : constant Integer := rowIndex * mazeColumns;
         begin
            -- Vertical walls
            for Vertical_wallIndex in 0 .. mazeColumns - 2 loop
               declare
                  Left_Cell : constant Integer := First_Cell_In_Row + Vertical_wallIndex;
                  Right_Cell : constant Integer := Left_Cell + 1;
               begin
                  wallConnections ( wallIndex, FIRST_CELL ) := Left_Cell;
                  wallConnections ( wallIndex, SECOND_CELL ) := Right_Cell;
                  wallIndex := wallIndex + 1;
               end;
            end loop;
            
            -- Horizontal walls
            if ( wallIndex < interiorWallCount ) then
               for Horizontal_wallIndex in 0 .. mazeColumns - 1 loop
                  declare
                     Upper_Cell : constant Integer := First_Cell_In_Row + Horizontal_wallIndex;
                     Lower_Cell : constant Integer := Upper_Cell + mazeColumns;
                  begin
                     wallConnections ( wallIndex, FIRST_CELL ) := Upper_Cell;
                     wallConnections ( wallIndex, SECOND_CELL ) := Lower_Cell;
                     wallIndex := wallIndex + 1;
                  end;
               end loop;
            end if;
         end;
      end loop;
      
      -- Initialize cell groups
      for Cell_Index in cellToGroup'Range loop
         cellToGroup ( Cell_Index ) := Cell_Index;
      end loop;
      
      -- Initialize group cells
      for Cell_Index in 0 .. mazeRows * mazeColumns - 1 loop
         for Inner_Cell_Index in 0 .. mazeRows * mazeColumns - 1 loop
            if ( Inner_Cell_Index = 0 ) then
               groupCells ( Cell_Index, Inner_Cell_Index ) := Cell_Index;
            else
               groupCells ( Cell_Index, Inner_Cell_Index ) := NO_CELL;
            end if;
         end loop;
      end loop;
      
      -- Print initial maze
      declare
         currentInteriorWall : Integer := wallsUp'First;
      begin
         -- Print top border
         Put ( "+" );
         for I in 0 .. mazeColumns - 1 loop
            Put ( "-+" );
         end loop;
         New_Line;
         
         for rowIndex in 0 .. mazeRows - 1 loop
            -- Print vertical walls and cells
            Put ( "|" );
            for columnIndex in 0 .. mazeColumns - 1 loop
               Put ( " " );
               
               if ( columnIndex = mazeColumns - 1 or else wallsUp ( currentInteriorWall ) ) then
                  Put ( "|" );
               else
                  Put ( " " );
               end if;
               
               if ( columnIndex < mazeColumns - 1 ) then
                  currentInteriorWall := currentInteriorWall + 1;
               end if;
            end loop;
            New_Line;
            
            -- Print horizontal walls
            if ( rowIndex < mazeRows - 1 ) then
               Put ( "+" );
               for columnIndex in 0 .. mazeColumns - 1 loop
                  if ( wallsUp ( currentInteriorWall ) ) then
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
         
         -- Print bottom border
         Put ( "+" );
         for I in 0 .. mazeColumns - 1 loop
            Put ( "-+" );
         end loop;
         New_Line;
      end;
      
      -- Initialize wall removal list
      for I in wallRemoveList'Range loop
         wallRemoveList ( I ) := I;
      end loop;
      
      -- Fisher-Yates shuffle
      for Shuffle_Index in reverse 1 .. interiorWallCount - 1 loop
         -- Simple LCG for random number generation
         randomSeed := Integer(Seconds(Clock)) mod 1000000;
         declare
            otherIndex : constant Integer := abs ( randomSeed ) mod ( Shuffle_Index + 1 );
            swapTemp : constant Integer := wallRemoveList ( Shuffle_Index );
         begin
            wallRemoveList ( Shuffle_Index ) := wallRemoveList ( otherIndex );
            wallRemoveList ( otherIndex ) := swapTemp;
         end;
      end loop;
      
      -- Kruskal's algorithm
      for removeWallIndex in 0 .. interiorWallCount - 1 loop
         declare
            nextWallToCheck : constant Integer := wallRemoveList ( removeWallIndex );
            firstCell : constant Integer := wallConnections ( nextWallToCheck, FIRST_CELL );
            firstCellGroupIndex : constant Integer := cellToGroup ( firstCell );
            secondCell : constant Integer := wallConnections ( nextWallToCheck, SECOND_CELL );
            secondCellGroupIndex : constant Integer := cellToGroup ( secondCell );
         begin
            if ( firstCellGroupIndex /= secondCellGroupIndex ) then
               wallsUp ( nextWallToCheck ) := False;
               
               -- Find next empty position in first group
               declare
                  nextEmptyFirstGroupIndex : Integer := 0;
               begin
                  for cellIndex in 0 .. mazeColumns * mazeRows - 1 loop
                     if ( groupCells ( firstCellGroupIndex, cellIndex ) = NO_CELL ) then
                        nextEmptyFirstGroupIndex := cellIndex;
                        exit;
                     end if;
                  end loop;
                  
                  -- Move all cells from second group to first group
                  for groupCellIndex in reverse 0 .. mazeColumns * mazeRows - 1 loop
                     if ( groupCells ( secondCellGroupIndex, groupCellIndex ) /= NO_CELL ) then
                        declare
                           cellToMove : constant Integer := groupCells ( secondCellGroupIndex, groupCellIndex );
                        begin
                           groupCells ( firstCellGroupIndex, nextEmptyFirstGroupIndex ) := cellToMove;
                           nextEmptyFirstGroupIndex := nextEmptyFirstGroupIndex + 1;
                           cellToGroup ( cellToMove ) := firstCellGroupIndex;
                           groupCells ( secondCellGroupIndex, groupCellIndex ) := NO_CELL;
                           
                           if ( nextEmptyFirstGroupIndex >= mazeColumns * mazeRows ) then
                              mazeComplete := True;
                           end if;
                        end;
                     end if;
                  end loop;
               end;
               
               -- Sleep and print maze
               delay 0.5;
               
               -- Copy in Print maze code from above again here:
               declare
                  currentInteriorWall : Integer := wallsUp'First;
               begin
                  New_Line;
                  Put ( "+" );
                  for i in 0 .. mazeColumns - 1 loop
                     Put ( "-+" );
                  end loop;
                  New_Line;
                  
                  for rowIndex in 0 .. mazeRows - 1 loop
                     Put ( "|" );
                     for columnIndex in 0 .. mazeColumns - 1 loop
                        Put ( " " );
                        
                        if ( columnIndex = mazeColumns - 1 or else wallsUp ( currentInteriorWall ) ) then
                           Put ( "|" );
                        else
                           Put ( " " );
                        end if;
                        
                        if ( columnIndex < mazeColumns - 1 ) then
                           currentInteriorWall := currentInteriorWall + 1;
                        end if;
                     end loop;
                     New_Line;
                     
                     if ( rowIndex < mazeRows - 1 ) then
                        Put ( "+" );
                        for columnIndex in 0 .. mazeColumns - 1 loop
                           if ( wallsUp ( currentInteriorWall ) ) then
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
                  for i in 0 .. mazeColumns - 1 loop
                     Put ( "-+" );
                  end loop;
                  New_Line;
               end;
               
               if ( mazeComplete ) then
                  exit;
               end if;
            end if;
         end;
      end loop;
   end;
end Maze_N_x_N;
