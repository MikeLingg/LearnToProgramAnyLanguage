with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Function_Ignored_Return is
   User_Input : String( 1..100 );
   Last : Natural;
   Entered_Integer : Integer; -- This will be uninitialized!
begin
   Put_Line( "Type 1 and press enter." );
   Get_Line( User_Input, Last );
   
   -- ERROR: Function called but return value ignored
   Integer'Value( User_Input( 1..Last ) );
   
   Put_Line( "The user entered the integer" & Integer'Image( Entered_Integer ) );
   
end Function_Ignored_Return;
