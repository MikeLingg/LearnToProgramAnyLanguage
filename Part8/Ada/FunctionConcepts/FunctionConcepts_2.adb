-- An example of function variable scope
with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
   Global_Variable : Integer := 15;
   Global_To_Be_Shadowed : Integer := 5;
   
   procedure My_Function is
      My_Variable : Integer := 55;
      Global_To_Be_Shadowed : Integer := 15;
   begin
      Global_Variable := 42;
   end My_Function;
   
begin
   Put_Line ( "Global variable:" & Integer'Image ( Global_Variable ) );
   Put_Line ( "Global shadowed:" & Integer'Image ( Global_To_Be_Shadowed ) );
   My_Function;
   Put_Line ( "Function variable:" & Integer'Image ( My_Variable ) );
   Put_Line ( "Global variable:" & Integer'Image ( Global_Variable ) );
   Put_Line ( "Global shadowed:" & Integer'Image ( Global_To_Be_Shadowed ) );
end Main;
