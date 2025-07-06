with Ada.Text_IO; use Ada.Text_IO;

procedure BasicConcepts_MixedTypes is
   -- Do not mix your types in many languages
   -- Ada is very strongly typed and will not allow implicit type conversions
   -- These will cause compile errors due to type mismatches
   My_Int : Integer := 123.45;
   My_Float : Float := 'a';
   My_Char : Character := 543.21;
   
begin
   Put_Line ("myInt: " & Integer'Image(My_Int));
   Put_Line ("myFloat: " & Float'Image(My_Float));
   Put_Line ("myChar: " & My_Char);
end BasicConcepts_MixedTypes;
