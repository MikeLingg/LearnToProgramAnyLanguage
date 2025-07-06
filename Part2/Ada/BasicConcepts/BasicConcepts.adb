with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;

procedure BasicConcepts is
   -- This signifies a number of basic concepts that would be included in one program, 
   -- except some of these concepts will prevent compilation or crash the program. 
   -- So this program will be broken up as specific languages require it.

   False_Boolean : Boolean := True;
   True_Boolean : Boolean := False;
   
   Min_Signed8 : Integer range -128 .. 127 := -128;
   Max_Signed8 : Integer range -128 .. 127 := 127;
   Min_Unsigned8 : Integer range 0 .. 255 := 0;
   Max_Unsigned8 : Integer range 0 .. 255 := 255;
   
   Min_Signed16 : Integer range -32768 .. 32767 := -32768;
   Max_Signed16 : Integer range -32768 .. 32767 := 32767;
   Min_Unsigned16 : Integer range 0 .. 65535 := 0;
   Max_Unsigned16 : Integer range 0 .. 65535 := 65535;
   
   Min_Signed32 : Integer := -2147483648;
   Max_Signed32 : Integer := 2147483647;
   Min_Unsigned32 : Long_Integer range 0 .. 4294967295 := 0;
   Max_Unsigned32 : Long_Integer range 0 .. 4294967295 := 4294967295;
   
   Min_Signed64 : Long_Long_Integer := -9223372036854775808;
   Max_Signed64 : Long_Long_Integer := 9223372036854775807;
   Min_Unsigned64 : Long_Long_Integer := 0;
   Max_Unsigned64 : Long_Long_Integer := 9223372036854775807;  -- Ada doesn't have unsigned long long
   
   Float_Max : Float := Float'Last;
   Float_Min : Float := Float'Small;
   
   Zero_Point_One : Float := 0.1;
   Zero_Point_Two : Float := 0.2;
   Zero_Point_Three : Float := 0.3;
   
   Double_Max : Long_Float := Long_Float'Last;
   Double_Min : Long_Float := Long_Float'Small;
   
   -- Shows the basics of ASCII characters, including special ones.
   -- This should print 1 followed by a tab followed by 2, then on the next line print 3.
   Char_One : Character := '1';
   Char_Tab : Character := ASCII.HT;
   Char_Two : Character := '2';
   Char_New_Line : Character := ASCII.LF;
   Char_Three : Character := '3';
   
   -- Note some languages may not compile this code so it will be in a separate file, others may throw warnings.
   -- Ada doesn't allow implicit conversion from integer to boolean
   Out_Of_Range_Boolean : Boolean := True;
   
   -- Note some languages may not compile this code so it will be in a separate file, others may throw warnings.
   -- Ada will check ranges at runtime and raise Constraint_Error if out of bounds
   Out_Of_Range : Integer range -32768 .. 32767 := 32767;
   
   -- Ada will throw a compiler error to prevent this.
   Out_Of_Range_Float : Float := Float'Last * 2.0;  -- This might become infinity
   Out_Of_Range_Double : Long_Float := Long_Float'Last * 2.0;  -- This might become infinity

   -- Ada character can handle ASCII range
   Out_Of_Range_Char : Character := Character'Val(65);  -- 'A' character

begin
   Put_Line ("Boolean range: " & Boolean'Image(False_Boolean) & " " & Boolean'Image(True_Boolean));
   
   Put_Line ("8 bit signed int range: " & Integer'Image(Min_Signed8) & " " & Integer'Image(Max_Signed8));
   Put_Line ("8 bit unsigned int range: " & Integer'Image(Min_Unsigned8) & " " & Integer'Image(Max_Unsigned8));
   
   Put_Line ("16 bit signed int range: " & Integer'Image(Min_Signed16) & " " & Integer'Image(Max_Signed16));
   Put_Line ("16 bit unsigned int range: " & Integer'Image(Min_Unsigned16) & " " & Integer'Image(Max_Unsigned16));
   
   Put_Line ("32 bit signed int range: " & Integer'Image(Min_Signed32) & " " & Integer'Image(Max_Signed32));
   Put_Line ("32 bit unsigned int range: " & Long_Integer'Image(Min_Unsigned32) & " " & Long_Integer'Image(Max_Unsigned32));
   
   Put_Line ("Note: Ada uses range constraints and strong typing for safety.");
   Put_Line ("64 bit signed int range: " & Long_Long_Integer'Image(Min_Signed64) & " " & Long_Long_Integer'Image(Max_Signed64));
   Put_Line ("64 bit unsigned int range: " & Long_Long_Integer'Image(Min_Unsigned64) & " " & Long_Long_Integer'Image(Max_Unsigned64));
   
   Put_Line ("Note that scientific notation must be used to print such a small number.");
   Put_Line ("32 bit float: " & Float'Image(Float_Min) & " " & Float'Image(Float_Max));
   
   -- So let's look at how far off the actual floating point value is from the value it was set to.
   Put_Line ("Floating point 0.1, 0.2, 0.3 -> " & Float'Image(Zero_Point_One) & 
             " and " & Float'Image(Zero_Point_Two) & " and " & Float'Image(Zero_Point_Three));
   
   Put_Line ("Note that scientific notation must be used to print such a small number.");
   Put_Line ("64 bit float range: " & Long_Float'Image(Double_Min) & " " & Long_Float'Image(Double_Max));
   
   Put ("Characters: ");
   Put (Char_One);
   Put (Char_Tab);
   Put (Char_Two);
   Put (Char_New_Line);
   Put (Char_Three);
   New_Line;
   
   -- Show how printing as an integer, not a character, can be confusing
   Put_Line ("charOne as an integer: " & Integer'Image(Character'Pos(Char_One)));
   
   Put_Line ("Out of range Boolean: " & Boolean'Image(Out_Of_Range_Boolean));
   
   Put_Line ("Out of range value: " & Integer'Image(Out_Of_Range));
   
   Put_Line ("Note that adding a small amount to float max is lost in the precision, so multiplying by 2.");
   Put_Line ("Out of range float and double: " & Float'Image(Out_Of_Range_Float) & " " & Long_Float'Image(Out_Of_Range_Double));

   Put_Line ("Out of range char: " & Out_Of_Range_Char);

end BasicConcepts;
