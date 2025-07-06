with Ada.Text_IO; use Ada.Text_IO;

procedure BasicConcepts_InvalidNames is
   -- Don't forget to declare your variables as appropriate to the language, some languages will fail to compile with this program
   Valid_Name : Character := 'a';
   Wrong_Case : Character := 'a';
   Wr_Ong_Letter : Character := 'a';
   
begin
   -- This will cause compile error - undefined variable
   Put_Line (Invalid_Name & "");
   -- This will cause compile error - undefined variable
   Put_Line (Validname & "");
   -- This will cause compile error - undefined variable
   Put_Line (Wrongcase & "");
   -- This will cause compile error - undefined variable
   Put_Line (Wr0ng_Letter & "");
 
   -- Don't start your variables with numbers or use hyphens
   -- This will cause compile error - invalid identifier
   2_Name_Invalid : Integer := 5;
   -- This will cause compile error - invalid identifier (hyphens not allowed)
   Invalid-Name : Character := 'a';
   
   Put_Line (Integer'Image(2_Name_Invalid));
   Put_Line (Invalid-Name & "");
   
   -- Also avoid using keywords already reserved by the programming language
   -- This will cause compile error - reserved keyword
   Procedure : Integer := 1;
   -- This will cause compile error - reserved keyword
   Package : Integer := 2;
   
   Put_Line (Integer'Image(Procedure));
   Put_Line (Integer'Image(Package));

end BasicConcepts_InvalidNames;
