procedure CompileErrors is
   
   -- Error 1: Missing "array" keyword
   type Temperature_Array is ( 0 .. 23 ) of Integer;
   
   -- Error 2: Wrong parentheses for range  
   type Pressure_Array is array [ 0 .. 10 ] of Integer;
   
   -- Error 3: Missing "of" keyword
   type Humidity_Array is array ( 0 .. 15 ) Integer;
   
   -- Error 4: Using wrong operator for range
   type Wind_Array is array ( 0 to 5 ) of Integer;
   
   -- Error 5: Inconsistent range bounds (null range)
   type Rain_Array is array ( 10 .. 0 ) of Integer;
   
   -- Error 6: Missing parentheses around range
   type Snow_Array is array 0 .. 8 of Integer;
   
   temperatures : Temperature_Array;
   pressures : Pressure_Array;
   humidity : Humidity_Array;
   
   -- Error 7: Using curly braces instead of parentheses
   values1 : array ( 0 .. 2 ) of Integer := { 10, 20, 30 };
   
   -- Error 8: Missing parentheses around aggregate
   values2 : array ( 0 .. 2 ) of Integer := 40, 50, 60;
   
   -- Error 9: Wrong separator in aggregate
   values3 : array ( 0 .. 2 ) of Integer := ( 70; 80; 90 );
   
   -- Error 10: Wrong syntax for 2D array aggregate
   type Matrix_Type is array ( 0 .. 1, 0 .. 1 ) of Integer;
   matrix1 : Matrix_Type := {
      { 1, 2 },
      { 3, 4 }
   };
   
   -- Error 11: Missing parentheses for inner arrays
   matrix2 : Matrix_Type := (
      1, 2,
      3, 4
   );
   
   -- Error 12: Using double quotes for single character
   myChar : Character := "H";
   
   -- Error 13: Missing quotes around character
   myChar2 : Character := H;
   
   -- Error 14: Wrong null terminator syntax
   nullChar : Character := '\0';
   
   -- Error 15: Using assignment operator in declaration
   values4 : array ( 0 .. 1 ) of Integer = ( 100, 200 );
   
   size : Integer := 5;
   
   -- Error 16: Trying to declare array size with variable (not static)
   dynamicArray : array ( 0 .. size ) of Integer;

begin
   
   -- Error 17: Using square brackets instead of parentheses
   temperatures [ 0 ] := 25;
   
   -- Error 18: Missing parentheses for array access
   temperatures 1 := 30;
   
   -- Error 19: Wrong syntax for 2D array access
   matrix1 [ 0, 1 ] := 99;
   
   -- Error 20: Using declaration syntax in assignment
   values1 : array ( 0 .. 2 ) of Integer := ( 1, 2, 3 );
   
   -- Error 21: Trying to use Put without proper with/use clauses
   Put ( "Hello World" );
   
   -- Error 22: Using C-style null terminator
   myChar := '\0';
   
   -- Error 23: Using NUL without ASCII prefix
   myChar2 := NUL;

end CompileErrors;
