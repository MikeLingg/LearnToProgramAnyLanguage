-- Example of function overloading
with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
   procedure My_Function ( Int_Parameter_Par : Integer ) is
   begin
      Put_Line ( "Int version of my function called" & Integer'Image ( Int_Parameter_Par ) );
   end My_Function;
   
   procedure My_Function ( Float_Parameter_Par : Float ) is
   begin
      Put_Line ( "Float version of my function called" & Float'Image ( Float_Parameter_Par ) );
   end My_Function;
   
begin
   My_Function ( Int_Parameter_Par => 5 );
   My_Function ( Float_Parameter_Par => 5.5 );
end Main;
