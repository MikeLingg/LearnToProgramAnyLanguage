with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Error7_Assignment_Wrong_Place is
   User_Input : String( 1..100 );
   Last : Natural;
begin
   Put_Line( "Type 1 and press enter." );
   Get_Line( User_Input, Last );
   
   -- Assignment operator placed incorrectly in declaration
   Entered_Integer Integer'Value := ( User_Input( 1..Last ) );
   
   Put_Line( "The user entered the integer" & Integer'Image( Entered_Integer ) );
   
end Error7_Assignment_Wrong_Place;
