with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Main is
   -- This maze program does not necessarily progress the program, 
   -- but provides a construct we will use eventually in the program.
   
   type Maze_Wall_Array is array ( 1 .. 5, 1 .. 3 ) of Character;
   mazeWalls : Maze_Wall_Array := (
      ( '|', ' ', ASCII.NUL ),
      ( ' ', ' ', ' ' ),
      ( ' ', '|', ASCII.NUL ),
      ( ' ', '-', ' ' ),
      ( '|', ' ', ASCII.NUL )
   );
   
   LEFT : constant Integer := 1;
   UP : constant Integer := 2;
   RIGHT : constant Integer := 3;
   DOWN : constant Integer := 4;
   
   type Neighbor_Lookup_Array is array ( 1 .. 9, 1 .. 4 ) of Integer;
   neighborLookup : Neighbor_Lookup_Array := (
      ( -1, -1, -1, 3 ),  -- Cell 0
      ( -1, -1, 2, 4 ),   -- Cell 1
      ( 1, -1, -1, 5 ),   -- Cell 2
      ( -1, 0, 4, 6 ),    -- Cell 3
      ( 3, 1, -1, -1 ),   -- Cell 4
      ( -1, 2, -1, 8 ),   -- Cell 5
      ( -1, 3, -1, -1 ),  -- Cell 6
      ( -1, -1, 8, -1 ),  -- Cell 7
      ( 7, 5, -1, -1 )    -- Cell 8
   );
   
   type Top_Bottom_Array is array ( 1 .. 7 ) of Character;
   topBottomRow : Top_Bottom_Array := ( '+', '-', '+', '-', '+', '-', '+' );

begin
   -- This sort of code will look better when we have loops and functions
   -- We see how a 3x3 maze is created from the array of data, and using the 
   -- neighbor lookup provides -1 where a wall is in that direction, otherwise 
   -- provides the cell number in that direction.
   Put ( topBottomRow( 1 ) );
   Put ( topBottomRow( 2 ) );
   Put ( topBottomRow( 3 ) );
   Put ( topBottomRow( 4 ) );
   Put ( topBottomRow( 5 ) );
   Put ( topBottomRow( 6 ) );
   Put ( topBottomRow( 7 ) );
   New_Line;
   
   Put ( "|0" );
   Put ( mazeWalls( 1, 1 ) );
   Put ( "1" );
   Put ( mazeWalls( 1, 2 ) );
   Put ( "2|" );
   New_Line;
   
   Put ( "+" );
   Put ( mazeWalls( 2, 1 ) );
   Put ( "+" );
   Put ( mazeWalls( 2, 2 ) );
   Put ( "+" );
   Put ( mazeWalls( 2, 3 ) );
   Put ( "+" );
   New_Line;
   
   Put ( "|3" );
   Put ( mazeWalls( 3, 1 ) );
   Put ( "4" );
   Put ( mazeWalls( 3, 2 ) );
   Put ( "5|" );
   New_Line;
   
   Put ( "+" );
   Put ( mazeWalls( 4, 1 ) );
   Put ( "+" );
   Put ( mazeWalls( 4, 2 ) );
   Put ( "+" );
   Put ( mazeWalls( 4, 3 ) );
   Put ( "+" );
   New_Line;
   
   Put ( "|6" );
   Put ( mazeWalls( 5, 1 ) );
   Put ( "7" );
   Put ( mazeWalls( 5, 2 ) );
   Put ( "8|" );
   New_Line;
   
   Put ( topBottomRow( 1 ) );
   Put ( topBottomRow( 2 ) );
   Put ( topBottomRow( 3 ) );
   Put ( topBottomRow( 4 ) );
   Put ( topBottomRow( 5 ) );
   Put ( topBottomRow( 6 ) );
   Put ( topBottomRow( 7 ) );
   New_Line;
   
   -- So take cell number 4 and see what rooms are around it, 
   -- cell 3 is to the left and cell 1 is up, but walls are right and down.
   Put ( "Cell to left of cell 4 is: " );
   Put ( neighborLookup( 5, LEFT ), Width => 0 );
   New_Line;
   Put ( "Cell to up of cell 4 is: " );
   Put ( neighborLookup( 5, UP ), Width => 0 );
   New_Line;
   Put ( "Cell to right of cell 4 is: " );
   Put ( neighborLookup( 5, RIGHT ), Width => 0 );
   New_Line;
   Put ( "Cell to down of cell 4 is: " );
   Put ( neighborLookup( 5, DOWN ), Width => 0 );
   New_Line;

end Main;
