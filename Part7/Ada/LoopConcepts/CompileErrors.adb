with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure AdaErrors is

begin
   -- ERROR: Missing 'loop' keyword after for statement
   Put_Line("Missing loop keyword");
   for loopIndex in 0 .. 10
      Put_Line(Integer'Image(loopIndex));
   end loop;

   -- ERROR: Missing 'end loop' terminator
   Put_Line("Missing end loop");
   for loopIndex in 1 .. 5 loop
      Put_Line(Integer'Image(loopIndex));

   -- ERROR: Using 'end' instead of 'end loop'
   Put_Line("Using end instead of end loop");
   for loopIndex in 1 .. 5 loop
      Put_Line(Integer'Image(loopIndex));
   end;

   -- ERROR: Wrong order in range (higher before lower without reverse)
   Put_Line("Range in wrong order");
   for loopIndex in 10 .. 1 loop
      Put_Line(Integer'Image(loopIndex));
   end loop;

   -- ERROR: Using step/by for increment (not valid Ada syntax)
   Put_Line("Attempting to use step");
   for loopIndex in 0 .. 10 step 2 loop
      Put_Line(Integer'Image(loopIndex));
   end loop;

   -- ERROR: Using 'by' for increment (not valid Ada syntax)
   for loopIndex in 0 .. 10 by 2 loop
      Put_Line(Integer'Image(loopIndex));
   end loop;

   -- ERROR: Using ++ operator (C-style, not valid in Ada)
   declare
      counter : Integer := 0;
   begin
      while counter < 10 loop
         Put_Line(Integer'Image(counter));
         counter++;
      end loop;
   end;

   -- ERROR: Using += operator (not valid in Ada)
   declare
      counter : Integer := 0;
   begin
      while counter < 10 loop
         Put_Line(Integer'Image(counter));
         counter += 1;
      end loop;
   end;

   -- ERROR: Missing 'in' keyword in for loop
   for loopIndex 0 .. 10 loop
      Put_Line(Integer'Image(loopIndex));
   end loop;

   -- ERROR: Using semicolon instead of 'loop' keyword
   for loopIndex in 0 .. 10;
      Put_Line(Integer'Image(loopIndex));
   end loop;

   -- ERROR: Using 'do' keyword instead of 'loop' (common from other languages)
   for loopIndex in 0 .. 10 do
      Put_Line(Integer'Image(loopIndex));
   end do;

   -- ERROR: Missing 'while' keyword
   declare
      counter : Integer := 0;
   begin
      counter < 10 loop
         Put_Line(Integer'Image(counter));
         counter := counter + 1;
      end loop;
   end;

   -- ERROR: Using parentheses around while condition (not required, but error if used wrong)
   declare
      counter : Integer := 0;
   begin
      while (counter < 10) loop
         Put_Line(Integer'Image(counter));
         counter := counter + 1;
      end loop;
   end;

   -- ERROR: Using 'break' instead of 'exit'
   for loopIndex in 1 .. 10 loop
      if loopIndex = 5 then
         break;
      end if;
      Put_Line(Integer'Image(loopIndex));
   end loop;

   -- ERROR: Using 'continue' instead of Ada's loop control
   for loopIndex in 1 .. 10 loop
      if loopIndex mod 2 = 0 then
         continue;
      end if;
      Put_Line(Integer'Image(loopIndex));
   end loop;

   -- ERROR: Missing 'exit when' - using just 'when'
   declare
      counter : Integer := 0;
   begin
      loop
         Put_Line(Integer'Image(counter));
         counter := counter + 1;
         when counter >= 10;
      end loop;
   end;

   -- ERROR: Using 'if' for exit condition instead of 'exit when'
   declare
      counter : Integer := 0;
   begin
      loop
         Put_Line(Integer'Image(counter));
         counter := counter + 1;
         if counter >= 10;
      end loop;
   end;

   -- ERROR: Missing colon after loop variable declaration in range
   for Integer loopIndex in 0 .. 10 loop
      Put_Line(Integer'Image(loopIndex));
   end loop;

   -- ERROR: Declaring type before loop variable (wrong syntax)
   for Integer : loopIndex in 0 .. 10 loop
      Put_Line(Integer'Image(loopIndex));
   end loop;

   -- ERROR: Using 'to' instead of '..' in range
   for loopIndex in 0 to 10 loop
      Put_Line(Integer'Image(loopIndex));
   end loop;

   -- ERROR: Using 'until' instead of range
   for loopIndex from 0 until 10 loop
      Put_Line(Integer'Image(loopIndex));
   end loop;

   -- ERROR: Trying to modify loop variable inside loop
   for loopIndex in 1 .. 10 loop
      Put_Line(Integer'Image(loopIndex));
      loopIndex := loopIndex + 1;
   end loop;

   -- ERROR: Using floating point in range (not allowed in Ada)
   declare
      startVal : Float := 0.0;
      endVal : Float := 10.0;
   begin
      for loopIndex in startVal .. endVal loop
         Put_Line(Float'Image(loopIndex));
      end loop;
   end;

   -- ERROR: Direct floating point literal in range
   for loopIndex in 0.0 .. 10.0 loop
      Put_Line(Float'Image(loopIndex));
   end loop;

   -- ERROR: Missing 'reverse' keyword but using descending range
   for loopIndex in 10 .. 1 loop
      Put_Line(Integer'Image(loopIndex));
   end loop;

   -- ERROR: Using 'downto' instead of 'reverse' (Pascal syntax)
   for loopIndex in 10 downto 1 loop
      Put_Line(Integer'Image(loopIndex));
   end loop;

   -- ERROR: Placing 'reverse' after range instead of before
   for loopIndex in 10 .. 1 reverse loop
      Put_Line(Integer'Image(loopIndex));
   end loop;

   -- ERROR: Using array as range instead of array'Range
   declare
      myArray : array(1 .. 5) of Integer := (1, 2, 3, 4, 5);
   begin
      for loopIndex in myArray loop
         Put_Line(Integer'Image(loopIndex));
      end loop;
   end;

   -- ERROR: Missing semicolon after 'exit'
   for loopIndex in 1 .. 10 loop
      if loopIndex = 5 then
         exit
      end if;
      Put_Line(Integer'Image(loopIndex));
   end loop;

   -- ERROR: Using comparison operator = instead of equality test in exit when
   declare
      counter : Integer := 0;
   begin
      loop
         counter := counter + 1;
         exit when counter = 10
      end loop;
   end;

   -- ERROR: Missing 'then' in if statement inside loop
   for loopIndex in 1 .. 10 loop
      if loopIndex mod 2 = 0
         Put_Line("Even");
      end if;
   end loop;

   -- ERROR: Using curly braces instead of 'loop'/'end loop'
   for loopIndex in 0 .. 10 {
      Put_Line(Integer'Image(loopIndex));
   }

   -- ERROR: Using C-style for loop syntax
   for (int loopIndex = 0; loopIndex < 10; loopIndex++) loop
      Put_Line(Integer'Image(loopIndex));
   end loop;

   -- ERROR: Missing 'declare' block for local variables in loop body
   for loopIndex in 1 .. 10 loop
      tempValue : Integer := loopIndex * 2;
      Put_Line(Integer'Image(tempValue));
   end loop;

   -- ERROR: Using assignment := instead of comparison = in while condition
   declare
      counter : Integer := 0;
   begin
      while counter := 10 loop
         counter := counter + 1;
      end loop;
   end;

   -- ERROR: Using single equals for comparison in while (C-style)
   declare
      counter : Integer := 0;
   begin
      while counter == 10 loop
         counter := counter + 1;
      end loop;
   end;

   -- ERROR: Missing 'begin' in declare block with loop
   declare
      counter : Integer := 0;
      while counter < 10 loop
         Put_Line(Integer'Image(counter));
         counter := counter + 1;
      end loop;
   end;

   -- ERROR: Nested loop with same variable name without separate scope
   for loopIndex in 1 .. 5 loop
      Put_Line("Outer: " & Integer'Image(loopIndex));
      for loopIndex in 1 .. 3 loop
         Put_Line("Inner: " & Integer'Image(loopIndex));
      end loop;
   end loop;

   -- ERROR: Using 'each' keyword (Python/Ruby style)
   declare
      myArray : array(1 .. 5) of Integer := (1, 2, 3, 4, 5);
   begin
      for each element in myArray loop
         Put_Line(Integer'Image(element));
      end loop;
   end;

   -- ERROR: Missing 'end if' inside loop
   for loopIndex in 1 .. 10 loop
      if loopIndex mod 2 = 0 then
         Put_Line("Even");
      Put_Line(Integer'Image(loopIndex));
   end loop;

   -- ERROR: Using 'elsif' as separate word 'else if'
   for loopIndex in 1 .. 10 loop
      if loopIndex < 5 then
         Put_Line("Less than 5");
      else if loopIndex < 8 then
         Put_Line("Less than 8");
      else
         Put_Line("8 or greater");
      end if;
   end loop;

   -- ERROR: Using 'loop' keyword with infinite loop but forgetting basic loop syntax
   infinite loop
      Put_Line("Forever");
      exit when True;
   end loop;

   -- ERROR: Wrong terminator name after 'end loop'
   myLoop:
   for loopIndex in 1 .. 10 loop
      Put_Line(Integer'Image(loopIndex));
   end loop wrongName;

end AdaErrors;
