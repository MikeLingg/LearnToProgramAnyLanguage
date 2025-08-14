with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Error3_Wrong_Function_Name is
   User_Input : String( 1..100 );
   Last : Natural;
   Entered_Integer : Integer;
begin
   Put_Line( "Type 1 and press enter." );
   Get_Line( User_Input, Last );
   
   -- Function name is wrong - should be Integer'Value
   Entered_Integer := Integer'String_To_Number( User_Input( 1..Last ) );
   
   Put_Line( "The user entered the integer" & Integer'Image( Entered_Integer ) );
   
end Error3_Wrong_Function_N
