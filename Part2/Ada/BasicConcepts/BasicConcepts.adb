with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;

procedure BasicConcepts is
   -- This signifies a number of basic concepts that would be included in one program, 
   -- except some of these concepts will prevent compilation or crash the program. 
   -- So this program will be broken up as specific languages require it.

   falseBoolean : Boolean := True;
   trueBoolean : Boolean := False;
   
   minSigned8 : Integer range -128 .. 127 := -128;
   maxSigned8 : Integer range -128 .. 127 := 127;
   minUnsigned8 : Integer range 0 .. 255 := 0;
   maxUnsigned8 : Integer range 0 .. 255 := 255;
   
   minSigned16 : Integer range -32768 .. 32767 := -32768;
   maxSigned16 : Integer range -32768 .. 32767 := 32767;
   minUnsigned16 : Integer range 0 .. 65535 := 0;
   maxUnsigned16 : Integer range 0 .. 65535 := 65535;
   
   minSigned32 : Integer := -2147483648;
   maxSigned32 : Integer := 2147483647;
   minUnsigned32 : Long_Integer range 0 .. 4294967295 := 0;
   maxUnsigned32 : Long_Integer range 0 .. 4294967295 := 4294967295;
   
   minSigned64 : Long_Long_Integer := -9223372036854775808;
   maxSigned64 : Long_Long_Integer := 9223372036854775807;
   minUnsigned64 : Long_Long_Integer := 0;
   maxUnsigned64 : Long_Long_Integer := 9223372036854775807;  -- Ada doesn't have unsigned long long
   
   floatMax : Float := Float'Last;
   floatMin : Float := Float'Small;
   
   zeroPointOne : Float := 0.1;
   zeroPointTwo : Float := 0.2;
   zeroPointThree : Float := 0.3;
   
   doubleMax : Long_Float := Long_Float'Last;
   doubleMin : Long_Float := Long_Float'Small;
   
   -- Shows the basics of ASCII characters, including special ones.
   -- This should print 1 followed by a tab followed by 2, then on the next line print 3.
   charOne : Character := '1';
   charTab : Character := ASCII.HT;
   singleQuote : Character := ''';
   charNewLine : Character := ASCII.LF;
   doubleQuotes : Character := '"';
   
   -- Note some languages may not compile this code so it will be in a separate file, others may throw warnings.
   -- Ada doesn't allow implicit conversion from integer to boolean
   outOfRangeBoolean : Boolean := True;
   
   -- Note some languages may not compile this code so it will be in a separate file, others may throw warnings.
   -- Ada will check ranges at runtime and raise Constraint_Error if out of bounds
   outOfRange : Integer range -32768 .. 32767 := 32767;
   
   -- Ada will throw a compiler error to prevent this.
   --outOfRangeFloat : Float := Float'Last * 2.0;  -- This might become infinity
   --outOfRangeDouble : Long_Float := Long_Float'Last * 2.0;  -- This might become infinity

   -- Ada character can handle ASCII range
   outOfRangeChar : Character := Character'Val ( 65 );  -- 'A' character

begin
   Put_Line ( "Boolean range: " & Boolean'Image ( falseBoolean ) & " " & Boolean'Image ( trueBoolean ));
   
   Put_Line ( "8 bit signed int range: " & Integer'Image ( minSigned8 ) & " " & Integer'Image ( maxSigned8 ));
   Put_Line ( "8 bit unsigned int range: " & Integer'Image ( minUnsigned8 ) & " " & Integer'Image ( maxUnsigned8 ));
   
   Put_Line ( "16 bit signed int range: " & Integer'Image ( minSigned16 ) & " " & Integer'Image ( maxSigned16 ));
   Put_Line ( "16 bit unsigned int range: " & Integer'Image ( minUnsigned16 ) & " " & Integer'Image ( maxUnsigned16 ));
   
   Put_Line ( "32 bit signed int range: " & Integer'Image ( minSigned32 ) & " " & Integer'Image ( maxSigned32 ));
   Put_Line ( "32 bit unsigned int range: " & Long_Integer'Image ( minUnsigned32 ) & " " & Long_Integer'Image ( maxUnsigned32 ));
   
   Put_Line ( "Note: Ada uses range constraints and strong typing for safety." );
   Put_Line ( "64 bit signed int range: " & Long_Long_Integer'Image ( minSigned64 ) & " " & Long_Long_Integer'Image ( maxSigned64 ));
   Put_Line ( "64 bit unsigned int range: " & Long_Long_Integer'Image ( minUnsigned64 ) & " " & Long_Long_Integer'Image ( maxUnsigned64 ));
   
   Put_Line ( "Note that scientific notation must be used to print such a small number." );
   Put_Line ( "32 bit float: " & Float'Image ( floatMin ) & " " & Float'Image ( floatMax ));
   
   -- So let's look at how far off the actual floating point value is from the value it was set to.
   Put_Line ( "Floating point 0.1, 0.2, 0.3 -> " & Float'Image ( zeroPointOne ) & 
             " and " & Float'Image ( zeroPointTwo ) & " and " & Float'Image ( zeroPointThree ));

   Put(zeroPointOne, Fore => 1, Aft => 20, Exp => 0);
   New_Line;  -- Add newline separately
   
   Put(zeroPointOne, Fore => 1, Aft => 20, Exp => 0);
   New_Line;  -- Add newline separately
   
   Put(zeroPointOne, Fore => 1, Aft => 20, Exp => 0);
   New_Line;  -- Add newline separately
   
   Put_Line ( "Note that scientific notation must be used to print such a small number." );
   Put_Line ( "64 bit float range: " & Long_Float'Image ( doubleMin ) & " " & Long_Float'Image ( doubleMax ));
   
   Put ( "Characters: " );
   Put ( charOne );
   Put ( charTab );
   Put ( singleQuote );
   Put ( charNewLine );
   Put ( doubleQuotes );
   New_Line;
   
   -- Show how printing as an integer, not a character, can be confusing
   Put_Line ( "charOne as an integer: " & Integer'Image ( Character'Pos ( charOne )));
   
   Put_Line ( "Out of range Boolean: " & Boolean'Image ( outOfRangeBoolean ));
   
   Put_Line ( "Out of range value: " & Integer'Image ( outOfRange ));
   
   --Put_Line ( "Note that adding a small amount to float max is lost in the precision, so multiplying by 2." );
   --Put_Line ( "Out of range float and double: " & Float'Image ( outOfRangeFloat ) & " " & Long_Float'Image ( outOfRangeDouble ));

   Put_Line ( "Out of range char: " & outOfRangeChar );

end BasicConcepts;
