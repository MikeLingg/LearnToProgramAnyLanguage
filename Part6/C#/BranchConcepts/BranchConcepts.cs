using System;

class Program
{
    // Stubbed out functions
    static int userInput()
    {
        return 3;
    }

    static bool someFunctionResult()
    {
        return true;
    }

    static void Main( string[] args )
    {
        // Basic if statements with hard coded conditions
        Console.Write( "Before Conditions\n" );
        if ( true )
        {
            Console.Write( "Branch Executed\n" );
        }

        if ( false )
        {
            Console.Write( "Branch Not Executed\n" );
        }

        Console.Write( "After Conditions\n" );

        // If with hard coded assignment
        bool myVariable;
        if ( myVariable = true )
        {
            Console.Write( "Branch Executed\n" );
        }

        // If with variable assignment (using function result)
        if ( myVariable = someFunctionResult() )
        {
            Console.Write( "Branch Executed\n" );
        }

        // More proper conditional branch
        int temperature = 64;

        bool heaterOn = false;
        bool temperatureCold = ( temperature < 70 );
        if ( temperatureCold == true )
        {
            heaterOn = true;
        }

        Console.Write( $"Heater is on: {heaterOn}\n" );

        // Alternate code to evaluate a conditional branch
        heaterOn = false;
        if ( temperature < 70 )
        {
            heaterOn = true;
        }

        Console.Write( $"Heater is on: {heaterOn}\n" );

        // Short Circuit Example
        Console.WriteLine("Short Circuit Example");
        int gamesPlayed = 0;
        int totalScore = 150;
        
        if ( gamesPlayed > 0 && totalScore / gamesPlayed > 10 )
        {
            Console.WriteLine ( "You are averaging more than 10 points per game!" );
        }
        
        // Using a code block that executes if a condition is true.
        float budget = 5000.00f;
        float buffer = 500.00f;
        float estimatedCost = 4750.00f;

        bool budgetOverrun = false;
        float overrunAmount;

        if ( ( estimatedCost + buffer ) > budget )
        {
            overrunAmount = ( estimatedCost + buffer ) - budget;
            Console.Write( $"Overrun Amount: {overrunAmount:F2}\n" );
            buffer = 50;
            estimatedCost = budget - buffer;
            budgetOverrun = true;
        }

        Console.Write( $"Budget overrun occurred: {budgetOverrun}\n" );

        // Variable scope in code blocks.
        // Many languages will fail trying to access innerVariable outside of the block.
        // In most languages I can even take the if True line out and just have the code block.
        /*int outerVariable = 10;
        if ( true )
        {
            int innerVariable = 20;
        }

        Console.Write( $"Variables: {outerVariable} : {innerVariable}\n" );*/

        // Variable scope: Shadowed variables
        // C# does not allow shadowing
        //int variable = 10;
        //if ( true )
        //{
        //    int variable = 20;
        //    Console.Write( $"Variable inside: {variable}\n" );
        //}
        //
        //Console.Write( $"Variable outside: {variable}\n" );

        // Error of line being outside of if block, note the lack of begin/end block.
        // Different languages handle this differently.
        if ( false )
            Console.Write( "Statement One\n" );
            Console.Write( "Statement Two\n" ); // This will always execute in C#

        if ( false )
        {
            Console.Write( "Statement Three\n" );
            Console.Write( "Statement Four\n" );
        }

        // Good in C#
        if ( true )
        {
            Console.Write( "Good branch!\n" );
        }

        // Bad in C#
        /*if true
        {
            Console.Write( "Bad branch!\n" );
        }*/

        // If disconnected from the block, note the semicolon.
        // This behavior is going to vary from language to language
        if ( false );
        {
            Console.Write( "Hello\n" ); // This will always execute
        }

        // Multiple separate if statements with overlapping conditions
        int score = 85;

        if ( score >= 90 && score <= 100 )
        {
            Console.Write( "You got an A\n" );
        }
        if ( score >= 80 && score < 90 )
        {
            Console.Write( "You got a B\n" );
        }
        if ( score >= 70 && score < 80 )
        {
            Console.Write( "You got a C\n" );
        }
        if ( score >= 60 && score < 70 )
        {
            Console.Write( "You got a D\n" );
        }
        if ( score < 60 )
        {
            Console.Write( "You failed the test\n" );
        }

        // Else if example
        if ( score >= 90 )
        {
            Console.Write( "You got an A\n" );
        }
        else if ( score >= 80 )
        {
            Console.Write( "You got a B\n" );
        }
        else if ( score >= 70 )
        {
            Console.Write( "You got a C\n" );
        }
        else if ( score >= 60 )
        {
            Console.Write( "You got a D\n" );
        }
        else if ( score < 60 )
        {
            Console.Write( "You failed the test\n" );
        }

        // Forgetting the else on an else if
        if ( score >= 90 )
        {
            Console.Write( "You got an A\n" );
        }
        if ( score >= 80 )
        {
            Console.Write( "You got a B\n" );
        }
        else if ( score >= 70 )
        {
            Console.Write( "You got a C\n" );
        }

        // Else if without if, this will not compile in most languages.
        /*else if ( score >= 90 )
        {
            Console.Write( "You got an A\n" );
        }*/

        // Adding an else to our if/else if
        if ( score >= 90 )
        {
            Console.Write( "You got an A\n" );
        }
        else if ( score >= 80 )
        {
            Console.Write( "You got a B\n" );
        }
        else if ( score >= 70 )
        {
            Console.Write( "You got a C\n" );
        }
        else if ( score >= 60 )
        {
            Console.Write( "You got a D\n" );
        }
        else
        {
            Console.Write( "You failed the test\n" );
        }

        // Unreachable else, programming languages may not warn about this
        int age = 125;

        if ( ( age > 0 ) || ( age < 100 ) )
        {
            Console.Write( "Valid age\n" );
        }
        else
        {
            Console.Write( "Invalid age\n" );
        }

        // Else not following If or Else If, very uncompilable.
        /*else
        {
            Console.Write( "Hello from else!" );
        }
        else if ( true )
        {
            Console.Write( "Hello from else if!" );
        }
        if ( true )
        {
            Console.Write( "Hello from if!" );
        }*/

        // Example of a complex condition that could be made a nested if
        bool isLoggedIn = true;
        string role = "admin";
        string method = "POST";
        bool isBanned = false;
        bool resourceIsAvailable = true;

        if ( ( isLoggedIn == true ) && ( role == "admin" ) && ( method == "POST" ) && ( isBanned == false ) && ( resourceIsAvailable == true ) )
        {
            Console.Write( "Access granted\n" );
        }

        // Breaking the complex condition into a nested if
        if ( isLoggedIn == true )
        {
            if ( role == "admin" )
            {
                if ( method == "POST" )
                {
                    if ( isBanned == false )
                    {
                        if ( resourceIsAvailable == true )
                        {
                            Console.Write( "Access granted\n" );
                        }
                        else
                        {
                            Console.Write( "Resource Unavailable\n" );
                        }
                    }
                    else
                    {
                        Console.Write( "User is Banned\n" );
                    }
                }
                else
                {
                    Console.Write( "Wrong Method\n" );
                }
            }
            else
            {
                Console.Write( "Wrong User Level\n" );
            }
        }
        else
        {
            Console.Write( "Please Log In\n" );
        }

        // Dangling Else - How this is handled will differ in different languages
        bool userExists = true;
        bool passwordValid = true;

        if ( userExists == true )
            if ( passwordValid == true )
                Console.Write( "Access granted\n" );
        else
            Console.Write( "Retry user name and password\n" );

        // No dangling else with blocks explicitly defined
        if ( userExists == true )
        {
            if ( passwordValid == true )
            {
                Console.Write( "Access granted\n" );
            }
            else
            {
                Console.Write( "Retry password\n" );
            }
        }

        // Basic switch statement
        int switchVariable = 2;

        switch ( switchVariable )
        {
            case 1:
                Console.Write( "Variable is 1\n" );
                break;
            case 2:
                Console.Write( "Variable is 2\n" );
                break;
            default:
                Console.Write( "Variable is unexpected value!\n" );
                break;
        }

        // Switch on user input
        Console.Write( "Main Menu:\n" );
        Console.Write( "1. Start Game\n" );
        Console.Write( "2. Load Game\n" );
        Console.Write( "3. Show Help\n" );
        Console.Write( "4. Exit\n" );
        Console.Write( "Enter your choice: " );

        int choice = userInput();

        switch ( choice )
        {
            case 1:
                Console.Write( "Starting new game...\n" );
                break;
            case 2:
                Console.Write( "Loading saved game...\n" );
                break;
            case 3:
                Console.Write( "Help: Use the number keys to navigate the menu.\n" );
                break;
            case 4:
                Console.Write( "Exiting program. Goodbye!\n" );
                break;
            default:
                Console.Write( "Invalid choice. Please select a valid option.\n" );
                break;
        }

        // Divide by zero defensive condition
        int time = 0;
        int distance = 100;
        int speed = 0;
        if ( time != 0 )
        {
            speed = distance / time;
        }
        Console.Write( $"Speed: {speed}\n" );

        // Handling both valid and invalid user inputs converted to booleans
        Console.Write( "Enter a number:\n" );
        string inputString = Console.ReadLine();
        int readInt;

        if ( int.TryParse( inputString, out readInt ) )
        {
            Console.Write( "User entered a valid number\n" );
        }
        else
        {
            readInt = -1;
            Console.Write( "Invalid number entered.\n" );
        }

        // Method 1 of parsing an input string to a boolean
        Console.Write( "Enter a boolean:\n" );
        inputString = Console.ReadLine();
        
        bool readBool;
        bool readValidBool = false;

        if ( int.TryParse( inputString, out readInt ) )
        {
            readValidBool = true;
            if ( readInt == 0 )
            {
                readBool = false;
            }
            else
            {
                readBool = true;
            }
        }
        else
        {
            if ( string.Equals( inputString, "false", StringComparison.OrdinalIgnoreCase ) )
            {
                readBool = false;
                readValidBool = true;
            }
            else if ( string.Equals( inputString, "true", StringComparison.OrdinalIgnoreCase ) )
            {
                readBool = true;
                readValidBool = true;
            }
            else
            {
                Console.Write( "Invalid boolean entered\n" );
            }
        }

        if ( readValidBool == true )
        {
            Console.Write( "Entered boolean is valid\n" );
        }

        // Alternate method of parsing a string to boolean using guard clauses instead of nested conditions
        Console.Write( "Enter a boolean:\n" );
        inputString = Console.ReadLine().Trim();
        
        readValidBool = false;

        if ( int.TryParse( inputString, out readInt ) )
        {
            readValidBool = true;
            if ( readInt == 0 )
            {
                readBool = false;
            }
            else
            {
                readBool = true;
            }
        }

        if ( readValidBool == false )
        {
            if ( string.Equals( inputString, "false", StringComparison.OrdinalIgnoreCase ) )
            {
                readBool = false;
                readValidBool = true;
            }
            else if ( string.Equals( inputString, "true", StringComparison.OrdinalIgnoreCase ) )
            {
                readBool = true;
                readValidBool = true;
            }
        }

        if ( readValidBool == true )
        {
            Console.Write( "Valid boolean entered\n" );
        }
        else
        {
            Console.Write( "Invalid boolean entered\n" );
        }

        // Compare two strings, only up to the length of the shortest string
        string falseString = "false";
        
        bool inputMatchesFalse = inputString.StartsWith( falseString, StringComparison.OrdinalIgnoreCase );

        // Make sure both strings have the appropriate length
        falseString = "false";
        int subStringLength = falseString.Length;

        if ( subStringLength > inputString.Length )
        {
            subStringLength = inputString.Length;
        }

        inputMatchesFalse = ( inputString.Substring( 0, subStringLength ).Equals( falseString, StringComparison.OrdinalIgnoreCase ) );

        Console.Write( $"False Entered {inputMatchesFalse}\n" );

        // Float comparison with both positive and negative differences
        float firstFloat = 0.1f;
        float secondFloat = 0.2f;
        float sum = firstFloat + secondFloat;
        float thirdFloat = 0.3f;

        float tolerance = 0.000001f;

        float difference = sum - thirdFloat;

        if ( ( difference > -tolerance ) && ( difference < tolerance ) )
        {
            Console.Write( "First float plus second float is equal to third float.\n" );
        }
        else
        {
            Console.Write( "First float plus second float is NOT equal to third float.\n" );
        }

        // Float comparison with condition ensuring positive difference
        firstFloat = 0.1f;
        secondFloat = 0.2f;
        sum = firstFloat + secondFloat;
        thirdFloat = 0.3f;

        tolerance = 0.000001f;

        difference = sum - thirdFloat;
        if ( difference < 0 )
        {
            difference = -difference;
        }

        if ( difference < tolerance )
        {
            Console.Write( "First float plus second float is equal to third float.\n" );
        }
        else
        {
            Console.Write( "First float plus second float is NOT equal to third float.\n" );
        }

        // Float comparison using Math.Abs to ensure positive difference
        firstFloat = 0.1f;
        secondFloat = 0.2f;
        sum = firstFloat + secondFloat;
        thirdFloat = 0.3f;

        tolerance = 0.000001f;

        difference = Math.Abs( sum - thirdFloat );

        if ( difference < tolerance )
        {
            Console.Write( "First float plus second float is equal to third float.\n" );
        }
        else
        {
            Console.Write( "First float plus second float is NOT equal to third float.\n" );
        }
    }
}
