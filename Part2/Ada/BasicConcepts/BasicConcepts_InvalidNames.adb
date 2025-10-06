with Ada.Text_IO; use Ada.Text_IO;

procedure BasicConcepts_InvalidNames is
   -- Don't forget to declare your variables as appropriate to the language, some languages will fail to compile with this program
   validName : Character := 'a';
   wrongCase : Character := 'a';
   wrOngLetter : Character := 'a';
   
   -- Also avoid using keywords already reserved by the programming language
   -- This will cause compile error - reserved keyword
   Procedure : Integer := 1;
   -- This will cause compile error - reserved keyword
   Package : Integer := 2;
   
   -- Don't start your variables with numbers or use hyphens
   -- This will cause compile error - invalid identifier
   2NameInvalid : Integer := 5;
   -- This will cause compile error - invalid identifier (hyphens not allowed)
   Invalid-Name : Character := 'a';
   
begin
   -- This will cause compile error - undefined variable
   Put_Line ( invalidName & "" );
   -- This will cause compile error - undefined variable
   Put_Line ( validname & "" );
   -- This will cause compile error - undefined variable
   Put_Line ( wrongcase & "" );
   -- This will cause compile error - undefined variable
   Put_Line ( wr0ngLetter & "" );
 
   Put_Line (Integer'Image(2NameInvalid));
   Put_Line (Invalid-Name & "");
   
   Put_Line (Integer'Image(Procedure));
   Put_Line (Integer'Image(Package));

end BasicConcepts_InvalidNames;
