-- Infinite recursive function
procedure Main is
   procedure Recursive_Function is
   begin
      Recursive_Function;
   end Recursive_Function;
   
begin
   Recursive_Function;
end Main;
