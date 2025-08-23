with Ada.Text_IO;

procedure Palindrome is
   use Ada.Text_IO;
   
   testString : String := "radar";
   isAPalindrome : Boolean := False;
   leftCharIndex : Integer := 1;
   rightCharIndex : Integer := testString'Length;
   notAPalindrome : Boolean;
   
begin
   -- Use nested conditions to test a palindrome up to a set length
   if ( testString( leftCharIndex ) = testString( rightCharIndex ) ) then
      leftCharIndex := leftCharIndex + 1;
      rightCharIndex := rightCharIndex - 1;
      if ( testString( leftCharIndex ) = testString( rightCharIndex ) ) then
         leftCharIndex := leftCharIndex + 1;
         rightCharIndex := rightCharIndex - 1;
         if ( testString( leftCharIndex ) = testString( rightCharIndex ) ) then
            isAPalindrome := True;
         end if;
      end if;
   end if;
   
   Put( "Input string is a palindrome: " );
   Put( Boolean'Image( isAPalindrome ) );
   New_Line;
   
   -- Guarded conditions version with length protections
   testString := "radar";
   notAPalindrome := False;
   leftCharIndex := 1;
   rightCharIndex := testString'Length;
   
   if ( leftCharIndex <= testString'Length ) then
      if ( testString( leftCharIndex ) /= testString( rightCharIndex ) ) then
         notAPalindrome := True;
      end if;
   end if;
   
   if ( leftCharIndex <= testString'Length ) then
      if ( notAPalindrome /= True ) then
         leftCharIndex := leftCharIndex + 1;
         rightCharIndex := rightCharIndex - 1;
         if ( testString( leftCharIndex ) /= testString( rightCharIndex ) ) then
            notAPalindrome := True;
         end if;
      end if;
   end if;
   
   if ( leftCharIndex <= testString'Length ) then
      if ( notAPalindrome /= True ) then
         leftCharIndex := leftCharIndex + 1;
         rightCharIndex := rightCharIndex - 1;
         if ( testString( leftCharIndex ) /= testString( rightCharIndex ) ) then
            notAPalindrome := True;
         end if;
      end if;
   end if;
   
   Put( "Input string is a palindrome: " );
   Put( Boolean'Image( not notAPalindrome ) );
   New_Line;
   
end Palindrome;
