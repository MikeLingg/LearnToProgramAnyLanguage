with Ada.Text_IO; use Ada.Text_IO;

procedure BasicConcepts_Redeclaration is
   -- Do not redeclare variable names in most languages:
   Duplicate_Character : Character := 'a';
   -- This will cause compile error - variable already declared
   Duplicate_Character : Character := 'b';
   
begin
   Put_Line ("duplicateCharacter: " & Duplicate_Character);
end BasicConcepts_Redeclaration;
