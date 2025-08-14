with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Error6_Function_Variable_Reversed is
   User_Input : String( 1..100 );
   Last : Natural;
   Entered_Integer : Integer;
begin
   Put_Line( "Type 1 and press enter." );
   Get_Line( User_Input, Last );
   
   -- Trying to assign to attribute instead of calling attribute
   Integer'Value := Entered_Integer'( User_Input( 1..Last ) );
   
   Put_Line( "The user entered the integer" & Integer'Image( Entered_Integer ) );
   
end Error6_Function_Variable_Reversed;
