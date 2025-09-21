-- Basic function with no parameters or return value
with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
   procedure Print_Hello is
   begin
      Put_Line ( "Hello" );
   end Print_Hello;
begin
   Print_Hello;
end Main;
