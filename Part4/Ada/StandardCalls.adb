with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;

procedure IO_Math_Demo is
   -- Note: For this program to function as expected, the user will have to correctly enter the requested values.

   -- Boolean strings of true/false cannot be converted to a bool variable without conditions,
   -- so we will discuss how that works in the branches video coming soon.
   -- Some languages will not even allow for reading values of 0 or 1 from the terminal as
   -- booleans so we will identify which languages this fails with, and revisit how to make 
   -- this work in the branches video.

   User_Input : String ( 1 .. 100 );
   Input_Length : Natural;
   Entered_Boolean : Boolean;
   Entered_Integer : Integer;
   Entered_Float : Float;

begin
   -- Note: Ada has no standard way to read Boolean from terminal input
   -- We will cover how to handle this in the video on conditions 

   Put ( "Type 55 and press enter." );
   begin
      Get ( Entered_Integer );
      Put_Line ( "The user entered the integer" & Integer'Image ( Entered_Integer ) );
   exception
      when others =>
         Put_Line ( "The user entered the integer 0 (parse error occurred)" );
   end;

   Put ( "Type 55.5 and press enter." );
   begin
      Get ( Entered_Float );
      -- Note the skip line. If this is followed by a Get_Line, Get leaves a newline in the buffer.
      -- The same issue can happen if the Get parse fails.
      Skip_Line;
      Put_Line ( "The user entered the float" & Float'Image ( Entered_Float ) );

      Put("The user entered the float ");
      Put(Entered_Float, Fore => 2, Aft => 1, Exp => 0);
      New_Line;
   
      Put("The user entered the float ");
      Put(Entered_Float, Fore => 1, Aft => 1, Exp => 0);
      New_Line;
   
      Put("The user entered the float ");
      Put(Entered_Float, Fore => 3, Aft => 1, Exp => 0);
      New_Line;
   
      Put("The user entered the float ");
      Put(Entered_Float, Fore => 2, Aft => 0, Exp => 0);
      New_Line;
   
      Put("The user entered the float ");
      Put(Entered_Float, Fore => 2, Aft => 2, Exp => 0);
      New_Line;
   exception
      when others =>
         Put_Line ( "The user entered the float 0.0 (parse error occurred)" );
   end;

   Put ( "Type Hello World! and press enter." );
   Get_Line ( User_Input, Input_Length );
   Put_Line ( "The user entered the string " & User_Input ( 1 .. Input_Length ) );

   Put ( "Type 123abc and press enter." );
   begin
      Get ( Entered_Integer );
      Skip_Line;
      Put_Line ( "The user entered the integer" & Integer'Image ( Entered_Integer ) );
   exception
      when others =>
         Put_Line ( "The user entered the integer 0 (parse error occurred)" );
   end;

   Put ( "Type 123.45 and press enter." );
   begin
      Get ( Entered_Integer );
      Skip_Line;
      Put_Line ( "The user entered the integer" & Integer'Image ( Entered_Integer ) );
   exception
      when others =>
         Put_Line ( "The user entered the integer 0 (parse error occurred)" );
   end;

   Put ( "Type abc123 and press enter." );
   begin
      Get ( Entered_Integer );
      Skip_Line;
      Put_Line ( "The user entered the integer" & Integer'Image ( Entered_Integer ) );
   exception
      when others =>
         Put_Line ( "The user entered the integer 0 (parse error occurred)" );
         Skip_Line;
   end;

   Put ( "Type  567 and press enter." );
   begin
      Get ( Entered_Integer );
      Skip_Line;
      Put_Line ( "The user entered the integer" & Integer'Image ( Entered_Integer ) );
   exception
      when others =>
         Put_Line ( "The user entered the integer 0 (parse error occurred)" );
   end;

   Put ( "Type +567 and press enter." );
   begin
      Get ( Entered_Integer );
      Skip_Line;
      Put_Line ( "The user entered the integer" & Integer'Image ( Entered_Integer ) );
   exception
      when others =>
         Put_Line ( "The user entered the integer 0 (parse error occurred)" );
   end;

   Put ( "Type -567 and press enter." );
   begin
      Get ( Entered_Integer );
      Skip_Line;
      Put_Line ( "The user entered the integer" & Integer'Image ( Entered_Integer ) );
   exception
      when others =>
         Put_Line ( "The user entered the integer 0 (parse error occurred)" );
   end;

   -- Ada has abs operator for all numeric types
   Put_Line ( "Abs of -5 is" & Integer'Image ( abs ( -5 ) ) );
   Put_Line ( "Abs of -5.5 is" & Float'Image ( abs ( -5.5 ) ) );
   Put_Line ( "Abs of a is" & Integer'Image ( abs ( Character'Pos ( 'a' ) ) ) );

   Put_Line ( "Fabs of -5 is" & Float'Image ( abs ( -5.0 ) ) );
   Put_Line ( "Fabs of -5.5 is" & Float'Image ( abs ( -5.5 ) ) );
   Put_Line ( "Fabs of a is" & Float'Image ( abs ( Float ( Character'Pos ( 'a' ) ) ) ) );

   Put_Line ( "Pow of 2^5 is" & Float'Image ( 2.0 ** 5 ) );
   Put_Line ( "Pow of 2.2^5.2 is" & Float'Image ( 2.2 ** 5.2 ) );
   Put_Line ( "Pow of a^b is" & Float'Image ( Float ( Character'Pos ( 'a' ) ) ** Float ( Character'Pos ( 'b' ) ) ) );

   -- Note trig functions are almost always in radians, not degrees
   Put_Line ( "Sin of 90 is" & Float'Image ( Sin ( 90.0 ) ) );
   Put_Line ( "Sin of pi/2 is" & Float'Image ( Sin ( Ada.Numerics.Pi / 2.0 ) ) );

   Put_Line ( "Cos of 180 is" & Float'Image ( Cos ( 180.0 ) ) );
   Put_Line ( "Cos of pi is" & Float'Image ( Cos ( Ada.Numerics.Pi ) ) );

   -- Rounding type functions are very useful for explicit float to int conversions
   Put_Line ( "Floor of 5.5 is" & Float'Image ( Float'Floor ( 5.5 ) ) );
   Put_Line ( "Floor of -5.5 is" & Float'Image ( Float'Floor ( -5.5 ) ) );

   Put_Line ( "Ceil of 5.5 is" & Float'Image ( Float'Ceiling ( 5.5 ) ) );
   Put_Line ( "Ceil of -5.5 is" & Float'Image ( Float'Ceiling ( -5.5 ) ) );

   Put_Line ( "Round of 5.5 is" & Float'Image ( Float'Rounding ( 5.5 ) ) );
   Put_Line ( "Round of -5.5 is" & Float'Image ( Float'Rounding ( -5.5 ) ) );

   Put_Line ( "Trunc of 5.5 is" & Float'Image ( Float'Truncation ( 5.5 ) ) );
   Put_Line ( "Trunc of -5.5 is" & Float'Image ( Float'Truncation ( -5.5 ) ) );

   -- This will NOT crash in Ada (exception handling catches errors)
   Put ( "Type Hello World! and press enter." );
   begin
      Get ( Entered_Integer );
      Skip_Line;
      Put_Line ( "The user entered the integer" & Integer'Image ( Entered_Integer ) );
   exception
      when others =>
         Put_Line ( "The user entered the integer 0 (parse error occurred)" );
         Skip_Line;
   end;

   -- This will NOT crash in Ada (exception handling catches errors)
   Put ( "Type abc123 and press enter." );
   begin
      Get ( Entered_Integer );
      Put_Line ( "The user entered the integer" & Integer'Image ( Entered_Integer ) );
   exception
      when others =>
         Put_Line ( "The user entered the integer 0 (parse error occurred)" );
   end;

end IO_Math_Demo;
