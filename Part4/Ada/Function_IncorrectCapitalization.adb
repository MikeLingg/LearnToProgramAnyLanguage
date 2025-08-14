with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Error2_Wrong_Capitalization is
   User_Input : String( 1..100 );
   Last : Natural;
   Entered_Integer : Integer;
begin
   Put_Line( "Type 1 and press enter." );
   Get_Line( User_Input, Last );
   
   -- Function name has wrong capitalization
   Entered_Integer := Integer'value( User_Input( 1..Last ) );
   
   Put_Line( "The user entered the integer" & Integer'Image( Entered_Integer ) );
   
end Error2_Wrong_Capitalization;
