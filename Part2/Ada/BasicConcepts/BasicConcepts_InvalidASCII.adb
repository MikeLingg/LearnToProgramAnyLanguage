with Ada.Text_IO; use Ada.Text_IO;

procedure BasicConcepts_InvalidASCII is
   -- So the ASCII table shows the tab symbol as TAB, but this doesn't work in programming.
   -- I think in some programs this will crash, so it will be in a separate program.
   -- Ada doesn't allow multi-character literals - this will cause compile error
   Char_Invalid : Character := 'TAB';
begin
   Put_Line ("Invalid char: " & Char_Invalid);
end BasicConcepts_InvalidASCII;
