#include <iostream>
#include <string>
#include <cmath>
#include <algorithm>
#include <cstdio>
#include <cstdlib>
using namespace std;

// Stubbed out functions
int userInput()
{
    return 3;
}

bool someFunctionResult()
{
    return true;
}

int main()
{
    // Basic if statements with hard coded conditions
    printf( "Before Conditions\n" );
    if ( true )
    {
        printf( "Branch Executed\n" );
    }

    if ( false )
    {
        printf( "Branch Not Executed\n" );
    }

    printf( "After Conditions\n" );

    // If with hard coded assignment
    bool myVariable;
    if ( myVariable = true )
    {
        printf( "Branch Executed\n" );
    }

    // If with variable assignment (using function result)
    if ( myVariable = someFunctionResult() )
    {
        printf( "Branch Executed\n" );
    }

    // More proper conditional branch
    int temperature = 64;

    bool heaterOn = false;
    bool temperatureCold = ( temperature < 70 );
    if ( temperatureCold == true )
    {
        heaterOn = true;
    }

    printf( "Heater is on: %d\n", heaterOn );

    // Alternate code to evaluate a conditional branch
    heaterOn = false;
    if ( temperature < 70 )
    {
        heaterOn = true;
    }

    printf( "Heater is on: %d\n", heaterOn );

    // Short Circuit Example
    printf ( "Short Circuit Example\n" );
    int gamesPlayed = 0;
    int totalScore = 150;
    
    if ( gamesPlayed > 0 && totalScore / gamesPlayed > 10 )
    {
        printf ( "You are averaging more than 10 points per game!\n" );
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
        printf( "Overrun Amount: %.2f\n", overrunAmount );
        buffer = 50;
        estimatedCost = budget - buffer;
        budgetOverrun = true;
    }

    printf( "Budget overrun occurred: %d\n", budgetOverrun );

    // Variable scope in code blocks.
    // Many languages will fail trying to access innerVariable outside of the block.
    // In most languages I can even take the if True line out and just have the code block.
    /*int outerVariable = 10;
    if ( true )
    {
        int innerVariable = 20;
    }

    printf( "Variables: %d : %d\n", outerVariable, innerVariable );*/

    // Variable scope: Shadowed variables
    int variable = 10;
    if ( true )
    {
        int variable = 20; // This shadows the outer variable
        printf( "Variable inside: %d\n", variable );
    }

    printf( "Variable outside: %d\n", variable );

    // Error of line being outside of if block, note the lack of begin/end block.
    // Different languages handle this differently.
    if ( false )
        printf( "Statement One\n" );
        printf( "Statement Two\n" );

    if ( false )
    {
        printf( "Statement Three\n" );
        printf( "Statement Four\n" );
    }

    // Good in C++
    if ( true )
    {
        printf( "Good branch!\n" );
    }

    // Bad in C++
    /*if true
    {
        printf( "Bad branch!\n" );
    }*/

    // If disconnected from the block, note the semicolon.
    // This behavior is going to vary from language to language
    if ( false );
    {
        printf( "Hello\n" );
    }

    // Multiple separate if statements with overlapping conditions
    int score = 85;

    if ( score >= 90 && score <= 100 )
    {
        printf( "You got an A\n" );
    }
    if ( score >= 80 && score < 90 )
    {
        printf( "You got a B\n" );
    }
    if ( score >= 70 && score < 80 )
    {
        printf( "You got a C\n" );
    }
    if ( score >= 60 && score < 70 )
    {
        printf( "You got a D\n" );
    }
    if ( score < 60 )
    {
        printf( "You failed the test\n" );
    }

    // Else if example
    if ( score >= 90 )
    {
        printf( "You got an A\n" );
    }
    else if ( score >= 80 )
    {
        printf( "You got a B\n" );
    }
    else if ( score >= 70 )
    {
        printf( "You got a C\n" );
    }
    else if ( score >= 60 )
    {
        printf( "You got a D\n" );
    }
    else if ( score < 60 )
    {
        printf( "You failed the test\n" );
    }

    // Forgetting the else on an else if
    if ( score >= 90 )
    {
        printf( "You got an A\n" );
    }
    if ( score >= 80 )
    {
        printf( "You got a B\n" );
    }
    else if ( score >= 70 )
    {
        printf( "You got a C\n" );
    }

    // Else if without if, this will not compile in most languages.
    // Note we have to put an else here to terminate the previous else if
    //else;
    //else if ( score >= 90 )
    //{
    //    printf( "You got an A\n" );
    //}

    // Adding an else to our if/else if
    if ( score >= 90 )
    {
        printf( "You got an A\n" );
    }
    else if ( score >= 80 )
    {
        printf( "You got a B\n" );
    }
    else if ( score >= 70 )
    {
        printf( "You got a C\n" );
    }
    else if ( score >= 60 )
    {
        printf( "You got a D\n" );
    }
    else
    {
        printf( "You failed the test\n" );
    }

    // Unreachable else, programming languages may not warn about this
    int age = 125;

    if ( ( age > 0 ) || ( age < 100 ) )
    {
        printf( "Valid age\n" );
    }
    else
    {
        printf( "Invalid age\n" );
    }

    // Else not following If or Else If, very uncompilable.
    /*else
	{
        printf( "Hello from else!" );
    }
    else if ( True )
	{
        printf( "Hello from else if!" );
    }
    if ( True )
	{
        printf( "Hello from if!" );
    }*/

    // Example of a complex condition that could be made a nested if
    bool isLoggedIn = true;
    string role = "admin";
    string method = "POST";
    bool isBanned = false;
    bool resourceIsAvailable = true;

    if ( ( isLoggedIn == true ) && ( role == "admin" ) && ( method == "POST" ) && ( isBanned == false ) && ( resourceIsAvailable == true ) )
    {
        printf( "Access granted\n" );
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
                        printf( "Access granted\n" );
                    }
                    else
                    {
                        printf( "Resource Unavailable\n" );
                    }
                }
                else
                {
                    printf( "User is Banned\n" );
                }
            }
            else
            {
                printf( "Wrong Method\n" );
            }
        }
        else
        {
            printf( "Wrong User Level\n" );
        }
    }
    else
    {
        printf( "Please Log In\n" );
    }

    // Dangling Else - How this is handled will differ in different languages
    bool userExists = true;
    bool passwordValid = true;

    if ( userExists == true )
        if ( passwordValid == true )
            printf( "Access granted\n" );
    else
        printf( "Retry user name and password\n" );

    // No dangling else with blocks explicitly defined
    if ( userExists == true )
    {
        if ( passwordValid == true )
        {
            printf( "Access granted\n" );
        }
        else
        {
            printf( "Retry password\n" );
        }
    }

    // Basic switch statement
    int switchVariable = 2;

    switch ( switchVariable )
    {
        case 1:
            printf( "Variable is 1\n" );
            break;
        case 2:
            printf( "Variable is 2\n" );
            break;
        default:
            printf( "Variable is unexpected value!\n" );
    }

    // Switch on user input
    printf( "Main Menu:\n" );
    printf( "1. Start Game\n" );
    printf( "2. Load Game\n" );
    printf( "3. Show Help\n" );
    printf( "4. Exit\n" );
    printf( "Enter your choice: " );

    int choice = userInput();

    switch ( choice )
    {
        case 1:
            printf( "Starting new game...\n" );
            break;
        case 2:
            printf( "Loading saved game...\n" );
            break;
        case 3:
            printf( "Help: Use the number keys to navigate the menu.\n" );
            break;
        case 4:
            printf( "Exiting program. Goodbye!\n" );
            break;
        default:
            printf( "Invalid choice. Please select a valid option.\n" );
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

    // Handling both valid and invalid user inputs converted to booleans
    printf( "Enter a number:\n" );
    char inputBuffer[256];
    fgets( inputBuffer, sizeof( inputBuffer ), stdin );
    int readInt;

    if ( sscanf( inputBuffer, "%d", &readInt ) == 1 )
    {
        printf( "User entered a valid number\n" );
    }
    else
    {
        readInt = -1;
        printf( "Invalid number entered.\n" );
    }

    // Method 1 of parsing an input string to a boolean
    printf( "Enter a boolean:\n" );
    char inputBuffer2[256];
    fgets( inputBuffer2, sizeof( inputBuffer2 ), stdin );
    string inputString( inputBuffer2 );
    
    bool readBool;
    bool readValidBool = false;

    if ( sscanf( inputBuffer2, "%d", &readInt ) == 1 )
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
        string lowerInput = inputString;
        transform( lowerInput.begin(), lowerInput.end(), lowerInput.begin(), ::tolower );
        
        if ( lowerInput == "false" )
        {
            readBool = false;
            readValidBool = true;
        }
        else if ( lowerInput == "true" )
        {
            readBool = true;
            readValidBool = true;
        }
        else
        {
            printf( "Invalid boolean entered\n" );
        }
    }

    if ( readValidBool == true )
    {
        printf( "Entered boolean is valid\n" );
    }

    // Alternate method of parsing a string to boolean using guard clauses instead of nested conditions
    printf( "Enter a boolean:\n" );
    char inputBuffer3[256];
    fgets( inputBuffer3, sizeof( inputBuffer3 ), stdin );
    inputString = string( inputBuffer3 );
    // Remove newline if present
    if ( !inputString.empty() && inputString.back() == '\n' )
    {
        inputString.pop_back();
    }
    readInt = atoi( inputBuffer3 );
    readValidBool = false;

    if ( readInt != 0 || inputBuffer3[0] == '0' )
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
        string lowerInput = inputString;
        transform( lowerInput.begin(), lowerInput.end(), lowerInput.begin(), ::tolower );
        
        if ( lowerInput == "false" )
        {
            readBool = false;
            readValidBool = true;
        }
        else if ( lowerInput == "true" )
        {
            readBool = true;
            readValidBool = true;
        }
    }

    if ( readValidBool == true )
    {
        printf( "Valid boolean entered\n" );
    }
    else
    {
        printf( "Invalid boolean entered\n" );
    }

    // Compare two strings, only up to the length of the shortest string
    string falseString = "false";
    
    bool inputMatchesFalse = ( inputString.substr( 0, falseString.length() ) == falseString );

    // Make sure both strings have the appropriate length
    falseString = "false";
    int subStringLength = falseString.length();

    if ( subStringLength > static_cast<int>( inputString.length() ) )
    {
        subStringLength = inputString.length();
    }

    inputMatchesFalse = ( inputString.substr( 0, subStringLength ) == falseString );

    printf( "False Entered %d\n", inputMatchesFalse );

    // Float comparison with both positive and negative differences
    float firstFloat = 0.1f;
    float secondFloat = 0.2f;
    float sum = firstFloat + secondFloat;
    float thirdFloat = 0.3f;

    float tolerance = 0.000001f;

    float difference = sum - thirdFloat;

    if ( ( difference > -tolerance ) && ( difference < tolerance ) )
    {
        printf( "First float plus second float is equal to third float.\n" );
    }
    else
    {
        printf( "First float plus second float is NOT equal to third float.\n" );
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
        printf( "First float plus second float is equal to third float.\n" );
    }
    else
    {
        printf( "First float plus second float is NOT equal to third float.\n" );
    }

    // Float comparison using fabs to ensure positive difference
    firstFloat = 0.1f;
    secondFloat = 0.2f;
    sum = firstFloat + secondFloat;
    thirdFloat = 0.3f;

    tolerance = 0.000001f;

    difference = fabs( sum - thirdFloat );

    if ( difference < tolerance )
    {
        printf( "First float plus second float is equal to third float.\n" );
    }
    else
    {
        printf( "First float plus second float is NOT equal to third float.\n" );
    }

    return 0;
}
