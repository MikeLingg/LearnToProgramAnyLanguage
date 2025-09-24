with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Numerics.Discrete_Random;
with Ada.Calendar; use Ada.Calendar;

procedure Maze_Generator is

   -- Define directions
   LEFT : constant Integer := 0;
   UP : constant Integer := 1;
   RIGHT : constant Integer := 2;
   DOWN : constant Integer := 3;

   FIRST_CELL : constant Integer := 1;
   SECOND_CELL : constant Integer := 2;

   NO_CELL : constant Integer := -1;

   -- Global variables
   type Boolean_Array is array (Integer range <>) of Boolean;
   type Integer_Array is array (Integer range <>) of Integer;
   type Integer_Matrix is array (Integer range <>, Integer range <>) of Integer;

   type Boolean_Array_Access is access Boolean_Array;
   type Integer_Array_Access is access Integer_Array;
   type Integer_Matrix_Access is access Integer_Matrix;

   All_Horizontal_Walls_Up_Glob : Boolean_Array_Access;
   Walls_Up_Glob : Boolean_Array_Access;
   Interior_Wall_Count_Glob : Integer;
   Maze_Rows_Glob : Integer;
   Maze_Columns_Glob : Integer;

   -- Random number generator
   subtype Random_Range is Integer range 1 .. Integer'Last;
   package Random_Int is new Ada.Numerics.Discrete_Random (Random_Range);
   Gen : Random_Int.Generator;



   -- +-+-+-+
   -- Prints a horizontal wall, similar to the example above, 
   -- with walls down based on the provided parameters.
   -- Horizontal_Walls_Up_Par: Boolean array of size Maze_Columns_Glob, 
   --     True indicates the wall should be printed as up.
   procedure Print_Horizontal_Walls ( Horizontal_Walls_Up_Par : in Boolean_Array ) is
   begin
      Put ( "+" );
      for Horizontal_Wall_Index in 1 .. Maze_Columns_Glob loop
         if ( Horizontal_Walls_Up_Par(Horizontal_Wall_Index) = True ) then
            Put ( "-" );
         else
            Put ( " " );
         end if;
         Put ( "+" );
      end loop;
      New_Line;
   end Print_Horizontal_Walls;



   -- +-+-+-+
   -- Prints a horizontal wall, similar to the example above, with all walls up.
   procedure Print_Horizontal_Walls_All is
   begin
      Print_Horizontal_Walls ( All_Horizontal_Walls_Up_Glob.all );
   end Print_Horizontal_Walls_All;



   -- | | | |
   -- Prints a vertical wall, similar to the example above, 
   -- with walls down based on the provided parameters.
   -- Vertical_Walls_Up_Par: Boolean array of size Maze_Columns_Glob - 1, 
   --     True indicates the wall should be printed as up.
   procedure Print_Vertical_Walls ( Vertical_Walls_Up_Par : in Boolean_Array ) is
   begin
      -- First wall is an exterior wall, always up.
      Put ( "|" );
      for Vertical_Wall_Index in 1 .. Maze_Columns_Glob - 1 loop
         Put ( " " );
         if ( Vertical_Walls_Up_Par(Vertical_Wall_Index) = True ) then
            Put ( "|" );
         else
            Put ( " " );
         end if;
      end loop;
      -- Last wall exterior, always up.
      Put ( " " );
      Put ( "|" );
      New_Line;
   end Print_Vertical_Walls;



   -- Loop through the rows of the maze and print the maze 
   -- based on Walls_Up_Glob
   procedure Print_Maze is
      Interior_Wall_Index : Integer := 1;
      Vertical_Walls_Up : Boolean_Array_Access;
      Horizontal_Walls_Up : Boolean_Array_Access;
   begin
      -- First row is exterior walls
      Print_Horizontal_Walls_All;
      for Row_Index in 1 .. Maze_Rows_Glob loop
         Vertical_Walls_Up := new Boolean_Array(1 .. Maze_Columns_Glob - 1);
         for Column_Index in 1 .. Maze_Columns_Glob - 1 loop
            Vertical_Walls_Up(Column_Index) := Walls_Up_Glob(Interior_Wall_Index);
            Interior_Wall_Index := Interior_Wall_Index + 1;
         end loop;

         Print_Vertical_Walls ( Vertical_Walls_Up.all );

         if ( Row_Index = Maze_Rows_Glob ) then
            Print_Horizontal_Walls_All;
         else
            Horizontal_Walls_Up := new Boolean_Array(1 .. Maze_Columns_Glob);
            for Column_Index in 1 .. Maze_Columns_Glob loop
               Horizontal_Walls_Up(Column_Index) := Walls_Up_Glob(Interior_Wall_Index);
               Interior_Wall_Index := Interior_Wall_Index + 1;
            end loop;

            Print_Horizontal_Walls ( Horizontal_Walls_Up.all );
         end if;
      end loop;

      New_Line;
   end Print_Maze;



   -- Simple sleep function
   procedure Sleep_Half_Second is
      Start_Time : Time;
   begin
      Start_Time := Clock;
      while Clock - Start_Time < 1.0 loop
         null; -- Busy wait for 1 second
      end loop;
   end Sleep_Half_Second;



   -- Kruskal's algorithm.
   -- The simple description of the algorithm is first place each 
   -- cell in its own group.  Then process all walls in random order,
   -- if the cells on either side of the wall are in separate groups, 
   -- remove the wall and merge the groups.  Repeat until all 
   -- cells are now in the same group.
   procedure Build_Maze_Kruskal is
      Wall_Connections : Integer_Matrix_Access;
      Wall_Remove_List : Integer_Array_Access;
      Cell_To_Group : Integer_Array_Access;
      Group_Cells : Integer_Matrix_Access;
      Wall_Index : Integer := 1;
      First_Cell_In_Row : Integer;
      Left_Cell, Right_Cell : Integer;
      Upper_Cell, Lower_Cell : Integer;
      Maze_Complete : Boolean := False;
      Temp : Integer;
      Next_Wall_To_Check : Integer;
      First_Cell_Index : Integer;
      First_Cell_Group_Index : Integer;
      Second_Cell_Index : Integer;
      Second_Cell_Group_Index : Integer;
      Next_Empty_First_Group_Index : Integer;
      Cell_To_Move : Integer;
      Total_Cells : Integer;
   begin
      Total_Cells := Maze_Rows_Glob * Maze_Columns_Glob;
      
      -- Allocate arrays
      Wall_Connections := new Integer_Matrix(1 .. Interior_Wall_Count_Glob, 1 .. 2);
      Wall_Remove_List := new Integer_Array(1 .. Interior_Wall_Count_Glob);
      Cell_To_Group := new Integer_Array(1 .. Total_Cells);
      Group_Cells := new Integer_Matrix(1 .. Total_Cells, 1 .. Total_Cells);

      -- Identify the cells each wall connects.
      for Row_Index in 1 .. Maze_Rows_Glob loop
         -- Track the first cell in the current row
         First_Cell_In_Row := (Row_Index - 1) * Maze_Columns_Glob + 1;

         -- Note the 1..Maze_Columns_Glob - 1, one less vertical wall
         -- than the number of columns.
         for Vertical_Wall_Index in 1 .. Maze_Columns_Glob - 1 loop
            Left_Cell := First_Cell_In_Row + Vertical_Wall_Index - 1;
            Right_Cell := Left_Cell + 1;
            Wall_Connections(Wall_Index, FIRST_CELL) := Left_Cell;
            Wall_Connections(Wall_Index, SECOND_CELL) := Right_Cell;
            Wall_Index := Wall_Index + 1;
         end loop;

         -- The last row will have no interior horizontal walls below
         -- it, so will be skipped.
         if ( Wall_Index <= Interior_Wall_Count_Glob ) then
            for Horizontal_Wall_Index in 1 .. Maze_Columns_Glob loop
               Upper_Cell := First_Cell_In_Row + Horizontal_Wall_Index - 1;
               Lower_Cell := Upper_Cell + Maze_Columns_Glob;
               Wall_Connections(Wall_Index, FIRST_CELL) := Upper_Cell;
               Wall_Connections(Wall_Index, SECOND_CELL) := Lower_Cell;
               Wall_Index := Wall_Index + 1;
            end loop;
         end if;
      end loop;

      for Cell_Index in 1 .. Total_Cells loop
         Cell_To_Group(Cell_Index) := Cell_Index;

         for Inner_Cell_Index in 1 .. Total_Cells loop
            if ( Inner_Cell_Index = 1 ) then
               Group_Cells(Cell_Index, Inner_Cell_Index) := Cell_Index;
            else
               Group_Cells(Cell_Index, Inner_Cell_Index) := NO_CELL;
            end if;
         end loop;
      end loop;

      for Wall_Index_Loop in 1 .. Interior_Wall_Count_Glob loop
         Wall_Remove_List(Wall_Index_Loop) := Wall_Index_Loop;
      end loop;

      -- Fisher-Yates shuffle
      for I in reverse 2 .. Interior_Wall_Count_Glob loop
         declare
            J : Integer := Random_Int.Random(Gen) mod I + 1;
         begin
            Temp := Wall_Remove_List(I);
            Wall_Remove_List(I) := Wall_Remove_List(J);
            Wall_Remove_List(J) := Temp;
         end;
      end loop;

      -- Perform Kruskal's algorithm.
      for Remove_Wall_Index in 1 .. Interior_Wall_Count_Glob loop
         Next_Wall_To_Check := Wall_Remove_List(Remove_Wall_Index);

         -- If the two cells connected to this wall are not part 
         -- of the same group, remove the wall and merge the 
         -- groups.
         First_Cell_Index := Wall_Connections(Next_Wall_To_Check, FIRST_CELL);
         First_Cell_Group_Index := Cell_To_Group(First_Cell_Index);
         Second_Cell_Index := Wall_Connections(Next_Wall_To_Check, SECOND_CELL);
         Second_Cell_Group_Index := Cell_To_Group(Second_Cell_Index);
         if ( First_Cell_Group_Index /= Second_Cell_Group_Index ) then
            Walls_Up_Glob(Next_Wall_To_Check) := False;

            -- Loop through the indices of all cells in the first 
            -- group until we find a NO_CELL indicating no cell here.
            Next_Empty_First_Group_Index := 1;
            for Cell_Index in 1 .. Total_Cells loop
               if ( Group_Cells(First_Cell_Group_Index, Cell_Index) = NO_CELL ) then
                  Next_Empty_First_Group_Index := Cell_Index;
                  exit;
               end if;
            end loop;

            -- Loop through the indices of all cells in the second group,
            -- move each cell to the first group, and set that cell's 
            -- group to the first group index.
            for Group_Cell_Index in reverse 1 .. Total_Cells loop
               -- Skip until we reach valid cells
               if ( Group_Cells(Second_Cell_Group_Index, Group_Cell_Index) /= NO_CELL ) then
                  -- Get the id number of the cell to move from 
                  -- the second group to the first group
                  Cell_To_Move := Group_Cells(Second_Cell_Group_Index, Group_Cell_Index);

                  -- Move the cell number from the second group 
                  -- to the first group
                  Group_Cells(First_Cell_Group_Index, Next_Empty_First_Group_Index) := Cell_To_Move;
                  -- Move our empty index to the next cell in this array.
                  Next_Empty_First_Group_Index := Next_Empty_First_Group_Index + 1;
                  -- Mark this cell as part of the first group.
                  Cell_To_Group(Cell_To_Move) := First_Cell_Group_Index;
                  -- Remove the cell from the second group (set the
                  -- array entry to NO_CELL)
                  Group_Cells(Second_Cell_Group_Index, Group_Cell_Index) := NO_CELL;

                  if ( Next_Empty_First_Group_Index > Total_Cells ) then
                     Maze_Complete := True;
                  end if;
               end if;
            end loop;

            Sleep_Half_Second;

            Print_Maze;

            if ( Maze_Complete = True ) then
               exit;
            end if;
         end if;
      end loop;
   end Build_Maze_Kruskal;



   -- Main procedure
   procedure Main_Program is
      User_Input : Integer;
      Input_Valid : Boolean;
   begin
      -- Initialize random seed
      Random_Int.Reset(Gen);

      -- Prompt the user for maze size
      Maze_Columns_Glob := 0;
      while ( Maze_Columns_Glob <= 0 ) loop
         Put ( "Please enter number of columns for maze, must be greater than 1: " );
         begin
            Get ( User_Input );
            if ( User_Input > 1 ) then
               Maze_Columns_Glob := User_Input;
            end if;
         exception
            when others =>
               Skip_Line;
         end;
      end loop;

      Maze_Rows_Glob := 0;
      while ( Maze_Rows_Glob <= 0 ) loop
         Put ( "Please enter number of rows for maze, must be greater than 1: " );
         begin
            Get ( User_Input );
            if ( User_Input > 1 ) then
               Maze_Rows_Glob := User_Input;
            end if;
         exception
            when others =>
               Skip_Line;
         end;
      end loop;

      -- Setup maze datastructures for the user entered size.
      All_Horizontal_Walls_Up_Glob := new Boolean_Array(1 .. Maze_Columns_Glob);
      for I in 1 .. Maze_Columns_Glob loop
         All_Horizontal_Walls_Up_Glob(I) := True;
      end loop;

      Interior_Wall_Count_Glob := Maze_Rows_Glob * ( Maze_Columns_Glob - 1 ) + ( Maze_Rows_Glob - 1 ) * Maze_Columns_Glob;

      Walls_Up_Glob := new Boolean_Array(1 .. Interior_Wall_Count_Glob);
      for I in 1 .. Interior_Wall_Count_Glob loop
         Walls_Up_Glob(I) := True;
      end loop;

      Build_Maze_Kruskal;
   end Main_Program;

begin
   Main_Program;
end Maze_Generator;
