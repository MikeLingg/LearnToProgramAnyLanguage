-- Ada does not pass commmand line arguments to main, have to use built in functions.
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;

procedure Main is
begin
   -- Note Argument_Count is a parameterless funciton.
   Put_Line ( "Number of arguments:" & Integer'Image ( Argument_Count ) );
   Put_Line ( "Arguments:" );
   for I in 0 .. Argument_Count loop
      if I = 0 then
         Put_Line ( ASCII.HT & "Argument" & Integer'Image ( I ) & ": " & Command_Name );
      else
         Put_Line ( ASCII.HT & "Argument" & Integer'Image ( I ) & ": " & Argument ( I ) );
      end if;
   end loop;
end Main;
