with Ada.Text_IO; use Ada.Text_IO;

procedure Statement_Errors is
   package Int_IO is new Ada.Text_IO.Integer_IO(Integer);
   package Float_IO is new Ada.Text_IO.Float_IO(Float);
   use Int_IO, Float_IO;

   -- Variable declarations
   a : Integer := 5;
   b : Integer := 10;
   c : Float := 3.14;
   result : Integer;
   stringOne : String := "Hello";
   flag : Boolean := True;

begin
   -- ERROR: Type mismatch in assignment - cannot assign Float to Integer
   result := c;
   
   -- ERROR: Type mismatch in arithmetic - mixing Integer and Float without conversion
   result := a + c;
   
   -- ERROR: Type mismatch in comparison - comparing Integer with Float
   flag := (a > c);
   
   -- ERROR: Cannot assign to a literal/constant
   5 := a;
   
   -- ERROR: Using assignment operator instead of comparison in Boolean expression
   flag := (a := b);
   
   -- ERROR: Missing conversion in mixed-type arithmetic
   c := a / b;
   
   -- ERROR: Invalid string comparison with different types
   flag := (stringOne = a);
   
   -- ERROR: Missing semicolon after statement
   result := a + b
   
   -- ERROR: Invalid Boolean operation on non-Boolean types
   result := a and b;
   
   -- ERROR: Incorrect case sensitivity in Boolean literals
   flag := true;
   
   -- ERROR: Type mismatch in logical operations
   flag := (a and True);
   
   -- ERROR: Using wrong operator for string concatenation in comparison
   flag := (stringOne + "World" = "HelloWorld");
   
   -- ERROR: Attempting arithmetic on Boolean values
   result := flag + 1;

end Statement_Errors;
