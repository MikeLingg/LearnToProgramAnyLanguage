-- This example will not compile as we cannot call a variable like a function
with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
   procedure My_Function is
   begin
      Put_Line ( "Called My_Function" );
   end My_Function;
   
   My_Variable : Integer := 5;
begin
   My_Variable ();
end Main;
