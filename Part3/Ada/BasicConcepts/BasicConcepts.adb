with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with System; use System;
with System.Address_To_Access_Conversions;
with System.Storage_Elements; use System.Storage_Elements;

procedure Main is
   -- Note the structured example is assuming zero based indexing.
   -- One based index languages will differ.
   -- Ada uses 1-based indexing by default, but we can specify 0-based
   -- Also note how some references are array location, with ranges from first to last,
   -- while indexes being zero based are 0 to size - 1.

   type Temperature_Array is array ( 0 .. 23 ) of Integer;
   type Test_Score_Array is array ( 0 .. 5 ) of Integer;
   type Book_Number_Array is array ( 0 .. 4 ) of Integer;
   
   temperatures : Temperature_Array := ( 55, 58, 60, 65, 70, 73, 76, 79, 81, 83, 84, 85, 85, 84, 83, 81, 78, 75, 72, 69, 65, 62, 59, 57 );
   testScores : Test_Score_Array := ( 95, 75, 86, 86, 78, 94 );
   bookNumber : Book_Number_Array := ( 12495, 35786, 15863, 84962, 42697 );

   -- Variables for indexing
   bookTwoIndex : Integer := 1;
   hourCount : Integer := 24;
   firstTemperatureIndex : Integer := 0;
   lastTemperatureIndex : Integer;
   bookIndex : Integer := 2;
   largeArraySize : Integer := 10000;

   -- Large arrays
   type Large_Bool_Array is array ( 0 .. 9999 ) of Boolean;
   type Large_Int_Array is array ( 0 .. 999 ) of Integer;
   type Large_Float_Array is array ( 0 .. 4999 ) of Float;
   
   largeArray : Large_Bool_Array;
   largeArray1 : Large_Int_Array;
   largeArray2 : Large_Float_Array;

   -- Character arrays
   myString : array ( 0 .. 99 ) of Character;
   myString1 : array ( 0 .. 99 ) of Character := ( 'H', 'e', 'l', 'l', 'o', ' ', 'W', 'o', 'r', 'l', 'd', '.', ASCII.NUL, others => ASCII.NUL );
   
   -- Note the null terminator is implicit in string literals
   myString2 : constant String := "Hello World.";

   -- Test struct for demonstrating bounds checking
   type Int_Array_Type is array ( 0 .. 9 ) of Integer;
   type testStruct is record
      intArray : Int_Array_Type;
      myInt : Integer;
   end record;

   testStructInstance : testStruct;

   -- 2D Array
   type Two_D_Array is array ( 0 .. 3, 0 .. 3 ) of Character;
   twoDArray : Two_D_Array;

   -- Color constants
   RED : constant Integer := 0;
   GREEN : constant Integer := 1;
   BLUE : constant Integer := 2;
   YELLOW : constant Integer := 3;
   CYAN : constant Integer := 4;
   MAGENTA : constant Integer := 5;
   WHITE : constant Integer := 6;

   -- Color table
   type Color_Table_Array is array ( 0 .. 6, 0 .. 2 ) of Integer;
   colorTable : Color_Table_Array := (
      ( 255, 0,   0   ),  -- Red
      ( 0,   255, 0   ),  -- Green
      ( 0,   0,   255 ),  -- Blue
      ( 255, 255, 0   ),  -- Yellow = Red + Green
      ( 0,   255, 255 ),  -- Cyan   = Green + Blue
      ( 255, 0,   255 ),  -- Magenta = Red + Blue
      ( 255, 255, 255 )   -- White = Red + Green + Blue
   );

   -- For unsafe operations (Ada equivalent of C pointers)
   package Int_Address_Conv is new System.Address_To_Access_Conversions ( Integer );
   use Int_Address_Conv;

begin
   lastTemperatureIndex := hourCount - 1;

   -- 10th entry (0-based index 9)
   Put ( "Temperature at tenth hour is " );
   Put ( temperatures ( 9 ) );
   New_Line;
   
   -- 4th entry (0-based index 3)
   Put ( "Fourth student grade is " );
   Put ( testScores ( 3 ) );
   New_Line;

   -- 2nd entry (0-based index 1)
   Put ( "Second book index is " );
   Put ( bookNumber ( bookTwoIndex ) );
   New_Line;

   -- First and last 0 based indexes, 0 and array size - 1.
   Put ( "First temperature is " );
   Put ( temperatures ( firstTemperatureIndex ) );
   New_Line;
   
   Put ( "Last temperature is " );
   Put ( temperatures ( lastTemperatureIndex ) );
   New_Line;

   -- set temperature first entry to 65
   temperatures ( 0 ) := 65;
   Put ( "First temperature is now " );
   Put ( temperatures ( 0 ) );
   New_Line;

   -- set testScores fourth entry to 99
   testScores ( 3 ) := 99;
   Put ( "Fourth test score is now " );
   Put ( testScores ( 3 ) );
   New_Line;

   -- set bookNumber at index third entry to 75681
   bookNumber ( bookIndex ) := 75681;
   Put ( "Third book number is now " );
   Put ( bookNumber ( bookIndex ) );
   New_Line;

   -- Large arrays are automatically initialized to default values
   Put ( "First large array first and last initial values: " );
   if largeArray ( 0 ) then
      Put ( "TRUE" );
   else
      Put ( "FALSE" );
   end if;
   Put ( " " );
   if largeArray ( largeArraySize - 1 ) then
      Put ( "TRUE" );
   else
      Put ( "FALSE" );
   end if;
   New_Line;

   Put ( "Second large array first and last initial values: " );
   Put ( largeArray1 ( 0 ) );
   Put ( " " );
   Put ( largeArray1 ( 999 ) );
   New_Line;

   Put ( "Third large array first and last initial values: " );
   Put ( largeArray2 ( 0 ), Fore => 1, Aft => 1, Exp => 0 );
   Put ( " " );
   Put ( largeArray2 ( 4999 ), Fore => 1, Aft => 2, Exp => 0 );
   New_Line;

   -- set largeArray first entry to True
   largeArray ( 0 ) := True;
   -- set largeArray last entry to False
   largeArray ( largeArraySize - 1 ) := False;
   Put ( "First large array first and last values: " );
   if largeArray ( 0 ) then
      Put ( "TRUE" );
   else
      Put ( "FALSE" );
   end if;
   Put ( " " );
   if largeArray ( largeArraySize - 1 ) then
      Put ( "TRUE" );
   else
      Put ( "FALSE" );
   end if;
   New_Line;

   -- set largeArray1 first entry to 25
   largeArray1 ( 0 ) := 25;
   -- set largeArray1 last entry to 55
   largeArray1 ( 999 ) := 55;
   Put ( "Second large array first and last values: " );
   Put ( largeArray1 ( 0 ) );
   Put ( " " );
   Put ( largeArray1 ( 999 ) );
   New_Line;

   -- set largeArray2 first entry to 27.5
   largeArray2 ( 0 ) := 27.5;
   -- set largeArray2 last entry to 58.25
   largeArray2 ( 4999 ) := 58.25;
   Put ( "Third large array first and last values: " );
   Put ( largeArray2 ( 0 ), Fore => 1, Aft => 1, Exp => 0 );
   Put ( " " );
   Put ( largeArray2 ( 4999 ), Fore => 1, Aft => 2, Exp => 0 );
   New_Line;

   -- Character array (Ada style)
   myString ( 0 ) := 'H';
   myString ( 1 ) := 'e';
   myString ( 2 ) := 'l';
   myString ( 3 ) := 'l';
   myString ( 4 ) := 'o';
   myString ( 5 ) := ' ';
   myString ( 6 ) := 'W';
   myString ( 7 ) := 'o';
   myString ( 8 ) := 'r';
   myString ( 9 ) := 'l';
   myString ( 10 ) := 'd';
   myString ( 11 ) := '.';
   myString ( 12 ) := ASCII.NUL;
   
   -- Print up to null terminator
   for I in myString'Range loop
      exit when myString ( I ) = ASCII.NUL;
      Put ( myString ( I ) );
   end loop;
   New_Line;

   -- Print second string
   for I in myString1'Range loop
      exit when myString1 ( I ) = ASCII.NUL;
      Put ( myString1 ( I ) );
   end loop;
   New_Line;

   -- Note the \0 is not needed in Ada strings
   Put_Line ( myString2 );

   -- Initialize test struct
   testStructInstance.intArray := ( others => 0 );
   testStructInstance.myInt := 0;

   -- In Ada, buffer overflows are prevented by bounds checking
   -- This code demonstrates what would happen
   -- The following would raise Constraint_Error in Ada:
   -- testStructInstance.intArray ( 10 ) := 55;  -- This would raise exception

   -- Using unsafe operations to demonstrate buffer overflow (very dangerous!)
   declare
      -- Get address of the array and treat it as if it has 11 elements
      arrayAddr : System.Address := testStructInstance.intArray'Address;
      unsafeArrayAccess : Object_Pointer;
   begin
      -- This is the dangerous operation - accessing beyond bounds using addresses
      -- In Ada, this requires very explicit unsafe operations
      unsafeArrayAccess := To_Pointer ( arrayAddr + Storage_Offset ( 10 * 4 ) ); -- 10 * sizeof(Integer)
      unsafeArrayAccess.all := 55; -- This writes to myInt's memory location!
   end;

   -- myInt should now be corrupted due to buffer overflow
   Put ( "myInt value: " );
   Put ( testStructInstance.myInt );
   New_Line;

   testStructInstance.myInt := testStructInstance.myInt + 1;

   -- Reading the out-of-bounds location shows the change
   declare
      arrayAddr : System.Address := testStructInstance.intArray'Address;
      unsafeArrayAccess : Object_Pointer;
   begin
      unsafeArrayAccess := To_Pointer ( arrayAddr + Storage_Offset ( 10 * 4 ) );
      Put ( "Out of bounds array value: " );
      Put ( unsafeArrayAccess.all );
      New_Line;
   end;

   -- 2D Array
   twoDArray ( 0, 0 ) := '0';
   twoDArray ( 0, 1 ) := '1';
   twoDArray ( 0, 2 ) := '2';
   twoDArray ( 0, 3 ) := '3';
   twoDArray ( 1, 0 ) := '4';
   twoDArray ( 1, 1 ) := '5';
   twoDArray ( 1, 2 ) := '6';
   twoDArray ( 1, 3 ) := '7';
   twoDArray ( 2, 0 ) := '8';
   twoDArray ( 2, 1 ) := '9';
   twoDArray ( 2, 2 ) := 'A';
   twoDArray ( 2, 3 ) := 'B';
   twoDArray ( 3, 0 ) := 'C';
   twoDArray ( 3, 1 ) := 'D';
   twoDArray ( 3, 2 ) := 'E';
   twoDArray ( 3, 3 ) := 'F';

   -- Note: the actual implementation of this code will use some advanced
   -- techniques that will not be described, only the results of the code observed.
   Put ( "twoDArray memory location as flat data: " );
   for I in 0 .. 3 loop
      for J in 0 .. 3 loop
         Put ( twoDArray ( I, J ) );
      end loop;
   end loop;
   New_Line;

   Put ( "CYAN color values: " );
   Put ( colorTable ( CYAN, 0 ) );
   Put ( " " );
   Put ( colorTable ( CYAN, 1 ) );
   Put ( " " );
   Put ( colorTable ( CYAN, 2 ) );
   New_Line;

end Main;
