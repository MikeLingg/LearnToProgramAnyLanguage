with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Error5_Missing_Parameter is
   User_Input : String( 1..100 );
   Last : Natural;
   Entered_Integer : Integer;
begin
   Put_Line( "Type 1 and press enter." );
   Get_Line( User_Input, Last );
   
   -- ERROR: Attribute requires 1 parameter, but we're passing 0
   Entered_Integer := Integer'Value;
   
   Put_Line( "The user entered the integer" & Integer'Image( Entered_Integer ) );
   
end Error5_Missing_Parameter;
