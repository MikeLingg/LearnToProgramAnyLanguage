with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Float_Text_IO;
with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with Ada.Characters.Handling;

procedure Branching_Concepts is
   use Ada.Text_IO;
   use Ada.Integer_Text_IO;
   use Ada.Float_Text_IO;
   use Ada.Strings.Unbounded;
   use Ada.Strings.Fixed;

   -- Stubbed out functions
   function userInput return Integer is
   begin
      return 3;
   end userInput;

   function someFunctionResult return Boolean is
   begin
      return True;
   end someFunctionResult;

begin
   -- Basic if statements with hard coded conditions
   Put( "Before Conditions" );
   New_Line;
   if ( True ) then
      Put( "Branch Executed" );
      New_Line;
   end if;

   if ( False ) then
      Put( "Branch Not Executed" );
      New_Line;
   end if;

   Put( "After Conditions" );
   New_Line;

   -- If with hard coded assignment - Ada doesn't allow assignment as condition
   declare
      myVariable : Boolean;
   begin
      myVariable := True;
      if ( myVariable ) then
         Put( "Branch Executed" );
         New_Line;
      end if;

      -- If with variable assignment (using function result) - Ada doesn't allow assignment as condition
      myVariable := someFunctionResult;
      if ( myVariable ) then
         Put( "Branch Executed" );
         New_Line;
      end if;
   end;

   -- More proper conditional branch
   declare
      temperature : Integer := 64;
      heaterOn : Boolean := False;
      temperatureCold : Boolean;
   begin
      temperatureCold := ( temperature < 70 );
      if ( temperatureCold = True ) then
         heaterOn := True;
      end if;

      Put( "Heater is on: " );
      Put( Boolean'Image( heaterOn ) );
      New_Line;

      -- Alternate code to evaluate a conditional branch
      heaterOn := False;
      if ( temperature < 70 ) then
         heaterOn := True;
      end if;

      Put( "Heater is on: " );
      Put( Boolean'Image( heaterOn ) );
      New_Line;
   end;

   -- Using a code block that executes if a condition is true.
   declare
      budget : Float := 5000.00;
      buffer : Float := 500.00;
      estimatedCost : Float := 4750.00;
      budgetOverrun : Boolean := False;
      overrunAmount : Float;
   begin
      if ( ( estimatedCost + buffer ) > budget ) then
         overrunAmount := ( estimatedCost + buffer ) - budget;
         Put( "Overrun Amount: " );
         Put( overrunAmount, Fore => 1, Aft => 2, Exp => 0 );
         New_Line;
         buffer := 50.0;
         estimatedCost := budget - buffer;
         budgetOverrun := True;
      end if;

      Put( "Budget overrun occurred: " );
      Put( Boolean'Image( budgetOverrun ) );
      New_Line;
   end;

   -- Variable scope in code blocks.
   -- Many languages will fail trying to access innerVariable outside of the block.
   -- In most languages I can even take the if True line out and just have the code block.
   --[[
   --declare
   --   outerVariable : Integer := 10;
   --begin
   --   if ( True ) then
   --      declare
   --         innerVariable : Integer := 20;
   --      begin
   --         null;
   --      end;
   --   end if;
   --
   --   Put( "Variables: " );
   --   Put( outerVariable );
   --   Put( " : " );
   --   Put( innerVariable );
   --   New_Line;
   --end;

   -- Variable scope: Shadowed variables
   declare
      variable : Integer := 10;
   begin
      if ( True ) then
         declare
            variable : Integer := 20; -- This shadows the outer variable
         begin
            Put( "Variable inside: " );
            Put( variable );
            New_Line;
         end;
      end if;

      Put( "Variable outside: " );
      Put( variable );
      New_Line;
   end;

   -- Error of line being outside of if block, note the lack of begin/end block.
   -- Different languages handle this differently.
   -- Ada requires then/end if for all if statements so we can't show this
   if ( False ) then
      Put( "Statement One" );
      New_Line;
   end if;
   Put( "Statement Two" );
   New_Line;

   if ( False ) then
      Put( "Statement Three" );
      New_Line;
      Put( "Statement Four" );
      New_Line;
   end if;

   -- Good in Ada
   if ( True ) then
      Put( "Good branch!" );
      New_Line;
   end if;

   -- Bad in Ada
   if true then
      Put( "Bad branch!" );
      New_Line;
   end if;

   -- If disconnected from the block, note the semicolon.
   -- This behavior is going to vary from language to language
   -- Ada prevents this pattern - it requires proper if statement syntax
   --[[
   if ( False ) then
      Put( "Hello" );
      New_Line;
   end if;
   --]]

   -- Multiple separate if statements with overlapping conditions
   declare
      score : Integer := 85;
   begin
      if ( score >= 90 and score <= 100 ) then
         Put( "You got an A" );
         New_Line;
      end if;
      if ( score >= 80 and score < 90 ) then
         Put( "You got a B" );
         New_Line;
      end if;
      if ( score >= 70 and score < 80 ) then
         Put( "You got a C" );
         New_Line;
      end if;
      if ( score >= 60 and score < 70 ) then
         Put( "You got a D" );
         New_Line;
      end if;
      if ( score < 60 ) then
         Put( "You failed the test" );
         New_Line;
      end if;

      -- Else if example (elsif in Ada)
      if ( score >= 90 ) then
         Put( "You got an A" );
         New_Line;
      elsif ( score >= 80 ) then
         Put( "You got a B" );
         New_Line;
      elsif ( score >= 70 ) then
         Put( "You got a C" );
         New_Line;
      elsif ( score >= 60 ) then
         Put( "You got a D" );
         New_Line;
      elsif ( score < 60 ) then
         Put( "You failed the test" );
         New_Line;
      end if;

      -- Forgetting the else on an elsif
      if ( score >= 90 ) then
         Put( "You got an A" );
         New_Line;
      end if;
      if ( score >= 80 ) then
         Put( "You got a B" );
         New_Line;
      elsif ( score >= 70 ) then
         Put( "You got a C" );
         New_Line;
      end if;

      -- Elsif without if, this will not compile in most languages.
      --elsif ( score >= 90 ) then
      --   Put( "You got an A" );
      --   New_Line;
      --end if;

      -- Adding an else to our if/elsif
      if ( score >= 90 ) then
         Put( "You got an A" );
         New_Line;
      elsif ( score >= 80 ) then
         Put( "You got a B" );
         New_Line;
      elsif ( score >= 70 ) then
         Put( "You got a C" );
         New_Line;
      elsif ( score >= 60 ) then
         Put( "You got a D" );
         New_Line;
      else
         Put( "You failed the test" );
         New_Line;
      end if;
   end;

   -- Unreachable else, programming languages may not warn about this
   declare
      age : Integer := 125;
   begin
      if ( ( age > 0 ) or ( age < 100 ) ) then
         Put( "Valid age" );
         New_Line;
      else
         Put( "Invalid age" );
         New_Line;
      end if;
   end;

   -- Else not following If or Elsif, very uncompilable.
   --else
   --   Put( "Hello from else!" );
   --   New_Line;
   --elsif ( True ) then
   --   Put( "Hello from elsif!" );
   --   New_Line;
   --if ( True ) then
   --   Put( "Hello from if!" );
   --   New_Line;
   --end if;

   -- Example of a complex condition that could be made a nested if
   declare
      isLoggedIn : Boolean := True;
      role : String( 1..5 ) := "admin";
      method : String( 1..4 ) := "POST";
      isBanned : Boolean := False;
      resourceIsAvailable : Boolean := True;
   begin
      if ( ( isLoggedIn = True ) and ( role = "admin" ) and ( method = "POST" ) and ( isBanned = False ) and ( resourceIsAvailable = True ) ) then
         Put( "Access granted" );
         New_Line;
      end if;

      -- Breaking the complex condition into a nested if
      if ( isLoggedIn = True ) then
         if ( role = "admin" ) then
            if ( method = "POST" ) then
               if ( isBanned = False ) then
                  if ( resourceIsAvailable = True ) then
                     Put( "Access granted" );
                     New_Line;
                  else
                     Put( "Resource Unavailable" );
                     New_Line;
                  end if;
               else
                  Put( "User is Banned" );
                  New_Line;
               end if;
            else
               Put( "Wrong Method" );
               New_Line;
            end if;
         else
            Put( "Wrong User Level" );
            New_Line;
         end if;
      else
         Put( "Please Log In" );
         New_Line;
      end if;
   end;

   -- Dangling Else - How this is handled will differ in different languages
   -- Ada prevents this pattern - it requires then/end if for all if statements
   declare
      userExists : Boolean := True;
      passwordValid : Boolean := True;
   begin
      --[[
      if ( userExists = True ) then
         if ( passwordValid = True ) then
            Put( "Access granted" );
            New_Line;
         end if;
      else
         Put( "Retry user name" );
         New_Line;
      end if;
      --]]

      -- No dangling else with blocks explicitly defined
      if ( userExists = True ) then
         if ( passwordValid = True ) then
            Put( "Access granted" );
            New_Line;
         else
            Put( "Retry password" );
            New_Line;
         end if;
      end if;
   end;

   -- Basic switch statement (using case in Ada)
   declare
      switchVariable : Integer := 2;
   begin
      case ( switchVariable ) is
         when 1 =>
            Put( "Variable is 1" );
            New_Line;
         when 2 =>
            Put( "Variable is 2" );
            New_Line;
         when others =>
            Put( "Variable is unexpected value!" );
            New_Line;
      end case;
   end;

   -- Switch on user input
   Put( "Main Menu:" );
   New_Line;
   Put( "1. Start Game" );
   New_Line;
   Put( "2. Load Game" );
   New_Line;
   Put( "3. Show Help" );
   New_Line;
   Put( "4. Exit" );
   New_Line;
   Put( "Enter your choice: " );

   declare
      choice : Integer := userInput;
   begin
      case ( choice ) is
         when 1 =>
            Put( "Starting new game..." );
            New_Line;
         when 2 =>
            Put( "Loading saved game..." );
            New_Line;
         when 3 =>
            Put( "Help: Use the number keys to navigate the menu." );
            New_Line;
         when 4 =>
            Put( "Exiting program. Goodbye!" );
            New_Line;
         when others =>
            Put( "Invalid choice. Please select a valid option." );
            New_Line;
      end case;
   end;

   -- Divide by zero defensive condition
   declare
      time : Integer := 0;
      distance : Integer := 100;
      speed : Integer := 0;
   begin
      if ( time /= 0 ) then
         speed := distance / time;
      end if;
      Put( "Speed: " );
      Put( speed );
      New_Line;
   end;

   -- Handling both valid and invalid user inputs converted to booleans
   Put( "Enter a number:" );
   New_Line;
   declare
      inputString : String( 1..256 );
      inputLength : Natural;
      readInt : Integer;
      validInput : Boolean := False;
   begin
      Get_Line( inputString, inputLength );
      
      begin
         readInt := Integer'Value( inputString( 1..inputLength ) );
         validInput := True;
      exception
         when others =>
            readInt := -1;
            validInput := False;
      end;

      if ( validInput ) then
         Put( "User entered a valid number" );
         New_Line;
      else
         Put( "Invalid number entered." );
         New_Line;
      end if;

      -- Method 1 of parsing an input string to a boolean
      Put( "Enter a boolean:" );
      New_Line;
      Get_Line( inputString, inputLength );
      
      declare
         readBool : Boolean;
         readValidBool : Boolean := False;
         inputStr : String := inputString( 1..inputLength );
         lowerInputStr : String := Ada.Characters.Handling.To_Lower( inputStr );
      begin
         begin
            readInt := Integer'Value( inputStr );
            readValidBool := True;
            if ( readInt = 0 ) then
               readBool := False;
            else
               readBool := True;
            end if;
         exception
            when others =>
               if ( lowerInputStr = "false" ) then
                  readBool := False;
                  readValidBool := True;
               elsif ( lowerInputStr = "true" ) then
                  readBool := True;
                  readValidBool := True;
               else
                  Put( "Invalid boolean entered" );
                  New_Line;
               end if;
         end;

         if ( readValidBool = True ) then
            Put( "Entered boolean is valid" );
            New_Line;
         end if;
      end;

      -- Alternate method of parsing a string to boolean using guard clauses instead of nested conditions
      Put( "Enter a boolean:" );
      New_Line;
      Get_Line( inputString, inputLength );
      
      declare
         readBool : Boolean;
         readValidBool : Boolean := False;
         inputStr : String := Trim( inputString( 1..inputLength ), Ada.Strings.Both );
         lowerInputStr : String := Ada.Characters.Handling.To_Lower( inputStr );
      begin
         begin
            readInt := Integer'Value( inputStr );
            readValidBool := True;
            if ( readInt = 0 ) then
               readBool := False;
            else
               readBool := True;
            end if;
         exception
            when others =>
               null;
         end;

         if ( readValidBool = False ) then
            if ( lowerInputStr = "false" ) then
               readBool := False;
               readValidBool := True;
            elsif ( lowerInputStr = "true" ) then
               readBool := True;
               readValidBool := True;
            end if;
         end if;

         if ( readValidBool = True ) then
            Put( "Valid boolean entered" );
            New_Line;
         else
            Put( "Invalid boolean entered" );
            New_Line;
         end if;
      end;

      -- Compare two strings, only up to the length of the shortest string
      declare
         falseString : String := "false";
         inputStr : String := inputString( 1..inputLength );
         inputMatchesFalse : Boolean;
      begin
         inputMatchesFalse := ( Index( inputStr, falseString ) = 1 );

         -- Make sure both strings have the appropriate length
         declare
            subStringLength : Integer := falseString'Length;
         begin
            if ( subStringLength > inputStr'Length ) then
               subStringLength := inputStr'Length;
            end if;

            inputMatchesFalse := ( inputStr( 1..subStringLength ) = falseString );

            Put( "False Entered " );
            Put( Boolean'Image( inputMatchesFalse ) );
            New_Line;
         end;
      end;
   end;

   -- Float comparison with both positive and negative differences
   declare
      firstFloat : Float := 0.1;
      secondFloat : Float := 0.2;
      sum : Float := firstFloat + secondFloat;
      thirdFloat : Float := 0.3;
      tolerance : Float := 0.000001;
      difference : Float;
   begin
      difference := sum - thirdFloat;

      if ( ( difference > -tolerance ) and ( difference < tolerance ) ) then
         Put( "First float plus second float is equal to third float." );
         New_Line;
      else
         Put( "First float plus second float is NOT equal to third float." );
         New_Line;
      end if;

      -- Float comparison with condition ensuring positive difference
      firstFloat := 0.1;
      secondFloat := 0.2;
      sum := firstFloat + secondFloat;
      thirdFloat := 0.3;
      tolerance := 0.000001;

      difference := sum - thirdFloat;
      if ( difference < 0.0 ) then
         difference := -difference;
      end if;

      if ( difference < tolerance ) then
         Put( "First float plus second float is equal to third float." );
         New_Line;
      else
         Put( "First float plus second float is NOT equal to third float." );
         New_Line;
      end if;

      -- Float comparison using abs to ensure positive difference
      firstFloat := 0.1;
      secondFloat := 0.2;
      sum := firstFloat + secondFloat;
      thirdFloat := 0.3;
      tolerance := 0.000001;

      difference := abs( sum - thirdFloat );

      if ( difference < tolerance ) then
         Put( "First float plus second float is equal to third float." );
         New_Line;
      else
         Put( "First float plus second float is NOT equal to third float." );
         New_Line;
      end if;
   end;

end Branching_Concepts;
