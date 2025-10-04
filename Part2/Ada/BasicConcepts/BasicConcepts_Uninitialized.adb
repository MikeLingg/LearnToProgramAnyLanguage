with Ada.Text_IO; use Ada.Text_IO;

procedure BasicConcepts_Uninitialized is
   -- I think in some programs this will cause a crash or failure to compile, so it will be in a separate program.
   -- Ada requires initialization or will have undefined values - this may cause runtime issues
   myCharacter : Character;
begin
   Put_Line ( "myCharacter: " & myCharacter & " as int: " & Integer'Image ( Character'Pos ( myCharacter ) ) );
end BasicConcepts_Uninitialized;
