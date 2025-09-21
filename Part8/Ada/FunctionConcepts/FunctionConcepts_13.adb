-- Computing factorial with loop
with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
   function Factorial ( Factorial_Number_Par : Integer ) return Integer is
      Total_Factorial : Integer := 1;
   begin
      for Factorial_Number in 1 .. Factorial_Number_Par loop
         Total_Factorial := Total_Factorial * Factorial_Number;
      end loop;
      
      return Total_Factorial;
   end Factorial;
   
   Factorial_Result : Integer;
begin
   Factorial_Result := Factorial ( Factorial_Number_Par => 10 );
   Put_Line ( "Factorial of 10 is:" & Integer'Image ( Factorial_Result ) );
end Main;
