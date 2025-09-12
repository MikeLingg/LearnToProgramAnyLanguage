with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Loop_Examples is

begin
   -- Basic loop using Ada's for loop with range
   Put_Line ( "" );
   Put_Line ( "Simple 100 iteration loop." );
   for loopIndex in 0 .. 99 loop
      Put_Line ( Integer'Image ( loopIndex ) );
   end loop;
   Put_Line ( "After Loop" );

   -- Palindrome checker
   -- It is a palindrome until we prove it is not.
   Put_Line ( "" );
   Put_Line ( "Palindrome checker." );
   declare
      possiblePalindrome : String := "step on no pets";
      isPalindrome      : Boolean := True;
      halfLength        : Integer := possiblePalindrome'Length / 2;
   begin
      for palindromeIndex in 1 .. halfLength loop
         if possiblePalindrome ( palindromeIndex ) /= 
            possiblePalindrome ( possiblePalindrome'Length - palindromeIndex + 1 ) then
            isPalindrome := False;
            exit;
         end if;
      end loop;

      if isPalindrome = True then
         Put_Line ( possiblePalindrome & " is a palindrome." );
      else
         Put_Line ( possiblePalindrome & " is NOT a palindrome." );
      end if;
   end;

   -- Ada cannot have two loop variables in a single for loop
   -- Must use separate variables and a while loop

   -- Ada requires a slight change to continue a loop variable
   Put_Line ( "" );
   Put_Line ( "Loops continuing a loop variable." );
   declare
      loopIndex : Integer;
   begin
      for i in 5 .. 14 loop
         Put_Line ( "Loop Index First Loop:" & Integer'Image ( i ) );
         loopIndex := i;
      end loop;
      
      for i in ( loopIndex + 1 ) .. 54 loop
         Put_Line ( "Loop Index Second Loop:" & Integer'Image ( i ) );
      end loop;
   end;

   -- Ada does NOT allow floating point loop variables

   -- Range variants
   Put_Line ( "" );
   Put_Line ( "Different methods of looping over a range." );
   Put_Line ( "" );
   Put_Line ( "Start at different index" );
   Put_Line ( "" );
   for loopIndex in 1 .. 10 loop
      Put_Line ( "Loop Index:" & Integer'Image ( loopIndex ) );
   end loop;
   Put_Line ( "After Loop" );

   -- Ada does NOT allow loop steps other than 1, must adapt.
   Put_Line ( "" );
   Put_Line ( "Different loop increment using step" );
   Put_Line ( "" );
   for loopIndex in 1 .. 6 loop
      declare
         actualIndex : Integer := 5 + ( loopIndex - 1 ) * 2;
      begin
         if actualIndex < 12 then
            Put_Line ( "Loop Index:" & Integer'Image ( actualIndex ) );
         end if;
      end;
   end loop;
   Put_Line ( "After Loop" );

   Put_Line ( "" );
   Put_Line ( "Reverse increment" );
   Put_Line ( "" );
   for loopIndex in reverse 2 .. 9 loop
      Put_Line ( "Loop Index:" & Integer'Image ( loopIndex ) );
   end loop;
   Put_Line ( "After Loop" );

   -- Looping over array indices
   Put_Line ( "Looping over a range of indices in an array" );
   declare
      myList : array ( 1 .. 4 ) of Integer := ( 1, 2, 3, 4 );
   begin
      for loopIndex in myList'Range loop
         Put_Line ( "Loop Index:" & Integer'Image ( loopIndex ) );
      end loop;
   end;
   Put_Line ( "After Loop" );

   -- Bad, myList is not a number - This will not compile
   Put_Line ( "" );
   Put_Line ( "Attempting bad range usage - myList is not a number" );
   declare
      myList : array ( 1 .. 4 ) of Integer := ( 1, 2, 3, 4 );
   begin
      for loopIndex in 0 .. myList loop
         Put_Line ( "Loop Index:" & Integer'Image ( loopIndex ) );
      end loop;
   end;
   Put_Line ( "After Loop" );

   -- Initialize all entries in a large array
   Put_Line ( "" );
   Put_Line ( "Initialize values in an array" );
   declare
      arraySamples : constant Integer := 10000;
      intArray     : array ( 1 .. arraySamples ) of Integer := ( others => 0 );
   begin
      -- This is pointless in Ada as the above declaration initializes all values to 0
      -- The above statement is much faster than this loop.
      for arrayIndex in intArray'Range loop
         intArray ( arrayIndex ) := 0;
      end loop;

      -- Initialize all entries to index * 2 + 1
      for arrayIndex in intArray'Range loop
         intArray ( arrayIndex ) := arrayIndex * 2 + 1;
      end loop;
   end;

   -- Example with string validation
   -- Loop through a string and ensure it contains a valid number
   Put_Line ( "" );
   Put_Line ( "Verify string contains valid number." );
   declare
      myString    : String := "123.45";
      periodCount : Integer := 0;
      validNumber : Boolean := True;
   begin
      for arrayIndex in myString'Range loop
         declare
            testCharacter : Character := myString ( arrayIndex );
         begin
            if testCharacter = '.' then
               periodCount := periodCount + 1;
               if periodCount > 1 then
                  validNumber := False;
                  exit;
               end if;
            elsif testCharacter < '0' or testCharacter > '9' then
               validNumber := False;
               exit;
            end if;
         end;
      end loop;

      Put_Line ( "String is valid number:" & Boolean'Image ( validNumber ) );
   end;

   -- Basic condition loop
   Put_Line ( "" );
   Put_Line ( "Simple conditional loop" );
   declare
      intCount : Integer := 1000000;
   begin
      while True loop
         -- Just print the hello once so the terminal isn't overwhelmed.
         if intCount = 1000000 then
            Put_Line ( "Hello" );
         end if;

         -- Break added so this isn't an infinite loop
         intCount := intCount - 1;
         exit when intCount <= 0;
      end loop;
   end;

   -- While loop executing like our basic counting loop example
   Put_Line ( "" );
   Put_Line ( "Example while loop performing basic counting loop" );
   declare
      loopIndex : Integer := 0;
   begin
      while loopIndex < 100 loop
         Put_Line ( Integer'Image ( loopIndex ) );
         loopIndex := loopIndex + 1;
      end loop;
   end;
   Put_Line ( "After loop" );

   -- More appropriate use of a while loop
   Put_Line ( "" );
   Put_Line ( "Better while loop with length of loop unknown at start." );
   declare
      dataArray          : array ( 1 .. 19 ) of Integer := 
         ( 1, 2, 3, 4, 5, 6, 5, 7, 5, 8, 5, 9, 5, 10, 11, 12, 13, 14, 15 );
      dataArraySize     : Integer := dataArray'Length;
      loopIndex          : Integer := 1;
      foundCount         : Integer := 0;
      fiveElementsFound : Boolean := False;
   begin
      while ( loopIndex <= dataArraySize ) and ( fiveElementsFound = False ) loop
         if dataArray ( loopIndex ) < 10 then
            foundCount := foundCount + 1;
            if foundCount >= 5 then
               fiveElementsFound := True;
            end if;
         end if;
         loopIndex := loopIndex + 1;
      end loop;
   end;

   -- Using a loop to prompt user input until valid
   Put_Line ( "" );
   Put_Line ( "Conditional loop based on user input." );
   declare
      exitMenu : Boolean := False;
      choice    : Integer;
   begin
      while exitMenu = False loop
         Put_Line ( "Main Menu:" );
         Put_Line ( "1. Start Game" );
         Put_Line ( "2. Load Game" );
         Put_Line ( "3. Show Help" );
         Put_Line ( "4. Exit" );
         Put ( "Enter your choice: " );
         
         Get ( choice );

         -- Ada has case statements
         case choice is
            when 1 =>
               Put_Line ( "Starting new game..." );
            when 2 =>
               Put_Line ( "Loading saved game..." );
            when 3 =>
               Put_Line ( "Help: Use the number keys to navigate the menu." );
            when 4 =>
               Put_Line ( "Exiting program. Goodbye!" );
               exitMenu := True;
            when others =>
               Put_Line ( "Invalid choice. Please select a valid option." );
         end case;
      end loop;
   end;

   -- Ada has no do-while equivalent, but can simulate with loop-exit
   Put_Line ( "Loop equivalent example: Hint, entering 42 will exit this loop" );
   declare
      number : Integer;
   begin
      number := 42;
      while number /= 42 loop
         Put ( "Guess a number: " );
         Get ( number );
      end loop;
   end;

   -- Nested loop for simple 2d array print
   Put_Line ( "" );
   Put_Line ( "Nested loop, 2d, example." );
   declare
      rowCount    : Integer := 3;
      columnCount : Integer := 3;
      twoDList   : array ( 1 .. 3, 1 .. 3 ) of Integer :=
         ( ( 1, 2, 3 ), ( 4, 5, 6 ), ( 7, 8, 9 ) );
   begin
      for rowIndex in 1 .. rowCount loop
         for columnIndex in 1 .. columnCount loop
            Put ( Integer'Image ( twoDList ( rowIndex, columnIndex ) ) & " " );
         end loop;
         New_Line;
      end loop;
   end;

   -- Nested loop to print character permutations
   Put_Line ( "" );
   Put_Line ( "Nested permutations loop." );
   declare
      letters      : array ( 1 .. 5 ) of Character := ( 'a', 'b', 'c', 'd', 'e' );
      lettersSize : Integer := letters'Length;
   begin
      for firstCharIndex in 1 .. lettersSize loop
         for secondCharIndex in 1 .. lettersSize loop
            Put_Line ( letters ( firstCharIndex ) & letters ( secondCharIndex ) );
         end loop;
      end loop;
   end;

   -- Nested loop to order a list of numbers
   Put_Line ( "" );
   Put_Line ( "Nested loop for sorting numbers." );
   declare
      myList2      : array ( 1 .. 10 ) of Integer := ( 6, 8, 9, 7, 4, 5, 0, 3, 1, 2 );
      myList2Size : Integer := myList2'Length;
      swapValue    : Integer;
   begin
      for listIndex in 1 .. myList2Size loop
         for swapIndex in 1 .. ( myList2Size - listIndex ) loop
            if myList2 ( swapIndex ) > myList2 ( swapIndex + 1 ) then
               swapValue := myList2 ( swapIndex );
               myList2 ( swapIndex ) := myList2 ( swapIndex + 1 );
               myList2 ( swapIndex + 1 ) := swapValue;
            end if;
         end loop;
      end loop;
   end;

   -- Break within nested loop using exit
   Put_Line ( "" );
   Put_Line ( "Using an exit in a nested loop." );
   declare
      twoDList2  : array ( 1 .. 2, 1 .. 2 ) of Integer := ( ( 1, 2 ), ( 3, 4 ) );
      rowCount    : Integer := 2;
      columnCount : Integer := 2;
   begin
      for rowIndex in 1 .. rowCount loop
         for columnIndex in 1 .. columnCount loop
            Put_Line ( Integer'Image ( twoDList2 ( rowIndex, columnIndex ) ) );
            exit;
         end loop;
      end loop;
   end;

   -- Off by 1 error - Will cause Constraint_Error at runtime
   Put_Line ( "" );
   Put_Line ( "Off by 1 loop error for array access." );
   declare
      myArray   : array ( 1 .. 4 ) of Integer := ( 1, 2, 3, 4 );
      arraySize : Integer := myArray'Length;
   begin
      Put_Line ( "Loop through array off by 1." );
      for loopIndex in 1 .. ( arraySize + 1 ) loop
         declare
            arrayValue : Integer := myArray ( loopIndex );
         begin
            Put_Line ( Integer'Image ( loopIndex ) & " " & Integer'Image ( arrayValue ) );
         end;
      end loop;
   end;

   Put_Line ( "Loop through array correct." );
   declare
      myArray : array ( 1 .. 4 ) of Integer := ( 1, 2, 3, 4 );
   begin
      for loopIndex in myArray'Range loop
         Put_Line ( Integer'Image ( myArray ( loopIndex ) ) );
      end loop;
   end;

   -- Off by 1 when performing a partial list sort - Will cause Constraint_Error
   Put_Line ( "" );
   Put_Line ( "Sort list with off by 1 error." );
   declare
      myList3      : array ( 1 .. 4 ) of Integer := ( 4, 3, 2, 1 );
      myList3Size : Integer := myList3'Length;
      swapValue    : Integer;
   begin
      for loopIndex in 1 .. myList3Size loop
         if myList3 ( loopIndex ) > myList3 ( loopIndex + 1 ) then
            swapValue := myList3 ( loopIndex );
            myList3 ( loopIndex ) := myList3 ( loopIndex + 1 );
            myList3 ( loopIndex + 1 ) := swapValue;
         end if;
      end loop;
   end;

   -- Off by 1 when performing a partial list sort - Will NOT cause error
   Put_Line ( "" );
   Put_Line ( "Sort list without off by 1 error." );
   declare
      myList3   : array ( 1 .. 4 ) of Integer := ( 4, 3, 2, 1 );
      swapValue : Integer;
   begin
      for loopIndex in 1 .. ( myList3'Length - 1 ) loop
         if myList3 ( loopIndex ) > myList3 ( loopIndex + 1 ) then
            swapValue := myList3 ( loopIndex );
            myList3 ( loopIndex ) := myList3 ( loopIndex + 1 );
            myList3 ( loopIndex + 1 ) := swapValue;
         end if;
      end loop;
   end;

   -- Ada does not allow empty loop blocks

   -- Redeclaring loop variable in blocks
   Put_Line ( "" );
   Put_Line ( "Redeclared loop variable with loop blocks." );
   for loopIndex in 1 .. 10 loop
      for loopIndex in 1 .. 10 loop
         Put_Line ( "Loop Index:" & Integer'Image ( loopIndex ) );
      end loop;
   end loop;

   -- Loop condition untrue on entry
   Put_Line ( "" );
   Put_Line ( "Loop condition untrue on entry." );
   declare
      currentTime : Integer := 5;
      nextFrame   : Integer := 5;
   begin
      while currentTime < nextFrame loop
         Put_Line ( "Processing background tasks" );
         currentTime := currentTime + 1;
      end loop;
   end;

   -- Wrong reverse range loop
   Put_Line ( "" );
   Put_Line ( "Wrong reverse range loop." );
   declare
      arraySize : Integer := 100;
   begin
      for loopIndex in arraySize .. 1 loop
         Put_Line ( "Loop index:" & Integer'Image ( loopIndex ) );
      end loop;
   end;

   -- Right reverse range loop
   Put_Line ( "Reverse range loop correct." );
   declare
      arraySize : Integer := 100;
   begin
      for loopIndex in reverse 1 .. arraySize loop
         Put_Line ( "Loop index:" & Integer'Image ( loopIndex ) );
      end loop;
   end;

   -- Ada has very limited dynamic array modification capabilities in basic arrays
   -- Would typically use Ada.Containers for dynamic operations

   -- Ada integers can overflow if loop comparison is wrong
   Put_Line ( "Loop to very large number." );
   declare
      largeNumber : Integer := Integer'Last - 1000;
      loopIndex   : Integer := largeNumber;
   begin
      while loopIndex <= Integer'Last loop
         if loopIndex < 0 then
            Put_Line ( "Loop index became less than 0!" );
            exit;
         end if;
         loopIndex := loopIndex + 1;
      end loop;
   end;

end Loop_Examples;
