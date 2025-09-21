-- Recursive function call to compute factorial
with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
   function Factorial ( Factorial_Number_Par : Integer ) return Integer is
   begin
      if Factorial_Number_Par <= 1 then
         return 1;
      end if;
      
      return Factorial_Number_Par * Factorial ( Factorial_Number_Par - 1 );
   end Factorial;
   
   Factorial_Result : Integer;
begin
   Factorial_Result := Factorial ( Factorial_Number_Par => 10 );
   Put_Line ( "Factorial of 10 is:" & Integer'Image ( Factorial_Result ) );
end Main;
