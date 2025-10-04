with Ada.Text_IO; use Ada.Text_IO;

procedure BasicConcepts_MixedTypes is
   -- Do not mix your types in many languages
   -- Ada is very strongly typed and will not allow implicit type conversions
   -- These will cause compile errors due to type mismatches
   myInt : Integer := 123.45;
   myFloat : Float := 'a';
   myChar : Character := 543.21;
   
begin
   Put_Line ( "myInt: " & Integer'Image ( myInt ) );
   Put_Line ( "myFloat: " & Float'Image ( myFloat ) );
   Put_Line ( "myChar: " & myChar );
end BasicConcepts_MixedTypes;
