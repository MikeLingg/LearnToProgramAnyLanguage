with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Error4_Extra_Parameter is
   Extra_Input : Integer := 5;
   User_Input : String( 1..100 );
   Last : Natural;
   Entered_Integer : Integer;
begin
   Put_Line( "Type 1 and press enter." );
   Get_Line( User_Input, Last );
   
   -- Attribute only takes 1 parameter, but we're passing 2
   Entered_Integer := Integer'Value( User_Input( 1..Last ), Extra_Input );
   
   Put_Line( "The user entered the integer" & Integer'Image( Entered_Integer ) );
   
end Error4_Extra_Parameter;
