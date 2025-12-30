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

   type Temperature_Array is array ( 1 .. 24 ) of Integer;
   type Test_Score_Array is array ( 1 .. 6 ) of Integer;
   type Book_Number_Array is array ( 1 .. 5 ) of Integer;
   
   temperatures : Temperature_Array := ( 55, 58, 60, 65, 70, 73, 76, 79, 81, 83, 84, 85, 85, 84, 83, 81, 78, 75, 72, 69, 65, 62, 59, 57 );
   testScores : Test_Score_Array := ( 95, 75, 86, 86, 78, 94 );
   bookNumber : Book_Number_Array := ( 12495, 35786, 15863, 84962, 42697 );

   -- Variables for indexing
   bookTwoIndex : Integer;
   firstTemperatureIndex : Integer;
   lastTemperatureIndex : Integer;
   bookIndex : Integer;

   -- Large arrays
   type Large_Bool_Array is array ( 1 .. 10000 ) of Boolean;
   type Large_Int_Array is array ( 1 .. 1000 ) of Integer;
   type Large_Float_Array is array ( 1 .. 5000 ) of Float;
   
   largeArray : Large_Bool_Array;
   largeArray1 : Large_Int_Array;
   largeArray2 : Large_Float_Array;

   -- Character arrays
   myString : array ( 1 .. 100 ) of Character;
   myString1 : array ( 1 .. 100 ) of Character := ( 'H', 'e', 'l', 'l', 'o', ' ', 'W', 'o', 'r', 'l', 'd', '.', ASCII.NUL, others => ASCII.NUL );
   
   -- Note the null terminator is implicit in string literals
   myString2 : constant String := "Hello World.";

   -- Test struct for demonstrating bounds checking
   type Int_Array_Type is array ( 1 .. 10 ) of Integer;
   type testStruct is record
      intArray : Int_Array_Type;
      myInt : Integer;
   end record;

   testStructInstance : testStruct;

   -- 2D Array
   type Two_D_Array is array ( 1 .. 4, 1 .. 4 ) of Character;
   twoDArray : Two_D_Array;

   Flat : array (1 .. 16) of Character;
   for Flat'Address use twoDArray'Address;
   pragma Import (Ada, Flat);

   -- Color constants
   RED : constant Integer := 1;
   GREEN : constant Integer := 2;
   BLUE : constant Integer := 3;
   YELLOW : constant Integer := 4;
   CYAN : constant Integer := 5;
   MAGENTA : constant Integer := 6;
   WHITE : constant Integer := 7;

   RGB_RED : constant Integer := 1;
   RGB_GREEN : constant Integer := 2;
   RGB_BLUE : constant Integer := 3;

   -- Color table
   type Color_Table_Array is array ( 1 .. 7, 1 .. 3 ) of Integer;
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
   -- 10th entry (1-based index 10)
   Put ( "Temperature at tenth hour is " );
   Put ( temperatures ( 10 ) );
   New_Line;
   
   -- 4th entry (1-based index 4)
   Put ( "Fourth student grade is " );
   Put ( testScores ( 4 ) );
   New_Line;

   -- 2nd entry (1-based index 2)
   bookTwoIndex := 2;
   Put ( "Second book index is " );
   Put ( bookNumber ( bookTwoIndex ) );
   New_Line;

   -- First and last 1 based indexes, 1 and array size.
   firstTemperatureIndex := Temperature_Array'First;
   Put ( "First temperature is " );
   Put ( temperatures ( firstTemperatureIndex ) );
   New_Line;
   
   lastTemperatureIndex := Temperature_Array'Last;
   Put ( "Last temperature is " );
   Put ( temperatures ( lastTemperatureIndex ) );
   New_Line;

   -- set temperature first entry to 65
   temperatures ( 1 ) := 65;
   Put ( "First temperature is now " );
   Put ( temperatures ( 1 ) );
   New_Line;

   -- set testScores fourth entry to 99
   testScores ( 4 ) := 99;
   Put ( "Fourth test score is now " );
   Put ( testScores ( 4 ) );
   New_Line;

   -- set bookNumber at third entry to 75681
   bookIndex := 3;
   bookNumber ( bookIndex ) := 75681;
   Put ( "Third book number is now " );
   Put ( bookNumber ( bookIndex ) );
   New_Line;

   -- Large arrays are automatically initialized to default values
   Put ( "First large array first and last initial values: " );
   if largeArray ( Large_Bool_Array'First ) then
      Put ( "TRUE" );
   else
      Put ( "FALSE" );
   end if;
   Put ( " " );
   if largeArray ( Large_Bool_Array'Last ) then
      Put ( "TRUE" );
   else
      Put ( "FALSE" );
   end if;
   New_Line;

   Put ( "Second large array first and last initial values: " );
   Put ( largeArray1 ( Large_Int_Array'First ) );
   Put ( " " );
   Put ( largeArray1 ( Large_Int_Array'Last ) );
   New_Line;

   Put ( "Third large array first and last initial values: " );
   Put ( largeArray2 ( Large_Float_Array'First ), Fore => 1, Aft => 1, Exp => 0 );
   Put ( " " );
   Put ( largeArray2 ( Large_Float_Array'Last ), Fore => 1, Aft => 2, Exp => 0 );
   New_Line;

   -- set largeArray first entry to True
   largeArray ( Large_Bool_Array'First ) := True;
   -- set largeArray last entry to False
   largeArray ( Large_Bool_Array'Last ) := False;
   Put ( "First large array first and last values: " );
   if largeArray ( Large_Bool_Array'First ) then
      Put ( "TRUE" );
   else
      Put ( "FALSE" );
   end if;
   Put ( " " );
   if largeArray ( Large_Bool_Array'Last ) then
      Put ( "TRUE" );
   else
      Put ( "FALSE" );
   end if;
   New_Line;

   -- set largeArray1 first entry to 25
   largeArray1 ( Large_Int_Array'First ) := 25;
   -- set largeArray1 last entry to 55
   largeArray1 ( Large_Int_Array'Last ) := 55;
   Put ( "Second large array first and last values: " );
   Put ( largeArray1 ( Large_Int_Array'First ) );
   Put ( " " );
   Put ( largeArray1 ( Large_Int_Array'Last ) );
   New_Line;

   -- set largeArray2 first entry to 27.5
   largeArray2 ( Large_Float_Array'First ) := 27.5;
   -- set largeArray2 last entry to 58.25
   largeArray2 ( Large_Float_Array'Last ) := 58.25;
   Put ( "Third large array first and last values: " );
   Put ( largeArray2 ( Large_Float_Array'First ), Fore => 1, Aft => 1, Exp => 0 );
   Put ( " " );
   Put ( largeArray2 ( Large_Float_Array'Last ), Fore => 1, Aft => 2, Exp => 0 );
   New_Line;

   -- Character array (Ada style)
   myString ( 1 ) := 'H';
   myString ( 2 ) := 'e';
   myString ( 3 ) := 'l';
   myString ( 4 ) := 'l';
   myString ( 5 ) := 'o';
   myString ( 6 ) := ' ';
   myString ( 7 ) := 'W';
   myString ( 8 ) := 'o';
   myString ( 9 ) := 'r';
   myString ( 10 ) := 'l';
   myString ( 11 ) := 'd';
   myString ( 12 ) := '.';
   myString ( 13 ) := ASCII.NUL;
   
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
   -- testStructInstance.intArray ( 11 ) := 55;  -- This would raise exception

   -- 2D Array
   twoDArray ( 1, 1 ) := '0';
   twoDArray ( 1, 2 ) := '1';
   twoDArray ( 1, 3 ) := '2';
   twoDArray ( 1, 4 ) := '3';
   twoDArray ( 2, 1 ) := '4';
   twoDArray ( 2, 2 ) := '5';
   twoDArray ( 2, 3 ) := '6';
   twoDArray ( 2, 4 ) := '7';
   twoDArray ( 3, 1 ) := '8';
   twoDArray ( 3, 2 ) := '9';
   twoDArray ( 3, 3 ) := 'A';
   twoDArray ( 3, 4 ) := 'B';
   twoDArray ( 4, 1 ) := 'C';
   twoDArray ( 4, 2 ) := 'D';
   twoDArray ( 4, 3 ) := 'E';
   twoDArray ( 4, 4 ) := 'F';

   -- Note: the actual implementation of this code will use some advanced
   -- techniques that will not be described, only the results of the code observed.
   Put ( "twoDArray memory location as flat data: " );
   for I in Flat'Range loop
      Put ( Flat(I) );
   end loop;
   New_Line;

   Put ( "CYAN color values: " );
   Put ( colorTable ( CYAN, RGB_RED ) );
   Put ( " " );
   Put ( colorTable ( CYAN, RGB_GREEN ) );
   Put ( " " );
   Put ( colorTable ( CYAN, RGB_BLUE ) );
   New_Line;

end Main;