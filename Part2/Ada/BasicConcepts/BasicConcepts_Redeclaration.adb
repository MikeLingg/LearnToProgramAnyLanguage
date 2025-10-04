with Ada.Text_IO; use Ada.Text_IO;

procedure BasicConcepts_Redeclaration is
   -- Do not redeclare variable names in most languages:
   duplicateCharacter : Character := 'a';
   -- This will cause compile error - variable already declared
   duplicateCharacter : Character := 'b';
   
begin
   Put_Line ( "duplicateCharacter: " & duplicateCharacter );
end BasicConcepts_Redeclaration;
