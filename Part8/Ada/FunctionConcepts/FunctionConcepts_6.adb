-- Example with three default parameters
with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
   procedure Three_Default_Parameters ( First_Parameter_Par : Integer := 5; Second_Parameter_Par : Integer := 10; Third_Parameter_Par : Integer := 15 ) is
   begin
      Put_Line ( "Parameters:" & Integer'Image ( First_Parameter_Par ) & Integer'Image ( Second_Parameter_Par ) & Integer'Image ( Third_Parameter_Par ) );
   end Three_Default_Parameters;
   
begin
   Three_Default_Parameters ( First_Parameter_Par => 20, Second_Parameter_Par => 25, Third_Parameter_Par => 30 );
   Three_Default_Parameters ( Second_Parameter_Par => 25 );
end Main;
