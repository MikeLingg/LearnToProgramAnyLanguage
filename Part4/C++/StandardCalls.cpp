#include <cmath>
#include <cstdio>
#include <cstdlib>

int main ( )
{
    // Note: For this program to function as expected, the user will have to correctly enter the requested values.

    // Boolean strings of true/false cannot be converted to a bool variable without conditions,
    // so we will discuss how that works in the branches video coming soon.
    // Some languages will not even allow for reading values of 0 or 1 from the terminal as
    // booleans so we will identify which languages this fails with, and revisit how to make 
    // this work in the branches video.
    char userInput[100] = "";

    // Note we are storing the fgets result to avoid compiler warning but not using it, 
    // we will cover this handling in the branches video and 
    // cover what the * means in the references video after we describe functions

    char* terminalResult;

    printf ( "Type 55 and press enter.\n" );
    terminalResult = fgets ( userInput, sizeof ( userInput ), stdin );
    int enteredInteger = atoi ( userInput );
    printf ( "The user entered the integer %d\n", enteredInteger );

    printf ( "Type 55.5 and press enter.\n" );
    terminalResult = fgets ( userInput, sizeof ( userInput ), stdin );
    float enteredFloat = atof ( userInput );
    printf ( "The user entered the float %f\n", enteredFloat );
    printf ( "The user entered the float %4.1f\n", enteredFloat );
    printf ( "The user entered the float %3.1f\n", enteredFloat );
    printf ( "The user entered the float %5.1f\n", enteredFloat );
    printf ( "The user entered the float %4.0f\n", enteredFloat );
    printf ( "The user entered the float %4.2f\n", enteredFloat );

    printf ( "Type 0 and press enter.\n" );
    terminalResult = fgets ( userInput, sizeof ( userInput ), stdin );
    bool enteredBoolean = atoi ( userInput );
    printf ( "The user entered the boolean %d\n", enteredBoolean );

    printf ( "Type 1 and press enter.\n" );
    terminalResult = fgets ( userInput, sizeof ( userInput ), stdin );
    enteredBoolean = atoi ( userInput );
    printf ( "The user entered the boolean %d\n", enteredBoolean );

    printf ( "Type 11 and press enter.\n" );
    terminalResult = fgets ( userInput, sizeof ( userInput ), stdin );
    enteredBoolean = atoi ( userInput );
    printf ( "The user entered the boolean %d\n", enteredBoolean );

    printf ( "Type -1 and press enter.\n" );
    terminalResult = fgets ( userInput, sizeof ( userInput ), stdin );
    enteredBoolean = atoi ( userInput );
    printf ( "The user entered the boolean %d\n", enteredBoolean );

    printf ( "Type Hello World! and press enter.\n" );
    terminalResult = fgets ( userInput, sizeof ( userInput ), stdin );
    printf ( "The user entered the string %s", userInput );

    printf ( "Type 123.45 and press enter.\n" );
    terminalResult = fgets ( userInput, sizeof ( userInput ), stdin );
    enteredInteger = atoi ( userInput );
    printf ( "The user entered the integer %d\n", enteredInteger );

    printf ( "Type abc123 and press enter.\n" );
    terminalResult = fgets ( userInput, sizeof ( userInput ), stdin );
    enteredInteger = atoi ( userInput );
    printf ( "The user entered the integer %d\n", enteredInteger );

    printf ( "Type <space> 567 and press enter.\n" );
    terminalResult = fgets ( userInput, sizeof ( userInput ), stdin );
    enteredInteger = atoi ( userInput );
    printf ( "The user entered the integer %d\n", enteredInteger );

    printf ( "Type +567 and press enter.\n" );
    terminalResult = fgets ( userInput, sizeof ( userInput ), stdin );
    enteredInteger = atoi ( userInput );
    printf ( "The user entered the integer %d\n", enteredInteger );

    printf ( "Type -567 and press enter.\n" );
    terminalResult = fgets ( userInput, sizeof ( userInput ), stdin );
    enteredInteger = atoi ( userInput );
    printf ( "The user entered the integer %d\n", enteredInteger );

    // At least C does not allow abs of floats, so this will be moved to a separate file.
    printf ( "Abs of -5 is %d\n", abs ( -5 ) );
    printf ( "Abs of -5.5 is %d\n", abs ( -5.5) );
    printf ( "Abs of a is %d\n", abs ( 'a' ) );

    printf ( "Fabs of -5 is %g\n", fabs ( -5 ) );
    printf ( "Fabs of -5.5 is %g\n", fabs ( -5.5 ) );
    printf ( "Fabs of a is %g\n", fabs ( 'a' ) );

    printf ( "Pow of 2^5 is %g\n", pow ( 2, 5 ) );
    printf ( "Pow of 2.2^5.2 is %g\n", pow ( 2.2, 5.2 ) );
    printf ( "Pow of a^b is %g\n", pow ( 'a', 'b' ) );

    // Note trig functions are almost always is radians, not degrees
    printf ( "Sin of 90 is %g\n", sin ( 90 ) );
    printf ( "Sin of pi/2 is %g\n", sin ( 3.14159265358979323846 / 2 ) );

    printf ( "Cos of 180 is %g\n", cos ( 180) );
    printf ( "Cos of pi is %g\n", cos ( 3.14159265358979323846 ) );

    // Rounding type functions are very useful for explicit float to int conversions
    printf ( "Floor of 5.5 is %g\n", floor ( 5.5 ) );
    printf ( "Floor of -5.5 is %g\n", floor ( -5.5 ) );

    printf ( "Ceil of 5.5 is %g\n", ceil ( 5.5 ) );
    printf ( "Ceil of -5.5 is %g\n", ceil ( -5.5 ) );

    printf ( "Round of 5.5 is %g\n", round ( 5.5 ) );
    printf ( "Round of -5.5 is %g\n", round ( -5.5 ) );

    printf ( "Trunc of 5.5 is %g\n", trunc ( 5.5 ) );
    printf ( "Trunc of -5.5 is %g\n", trunc ( -5.5 ) );

    // This will crash some, if not all, languages, so will be moved to a separate program.
    printf ( "Type Hello World! and press enter.\n" );
    terminalResult = fgets ( userInput, sizeof ( userInput ), stdin );
    enteredInteger = atoi ( userInput );
    printf ( "The user entered the integer %d\n", enteredInteger );

    // This will crash some programs so will be moved to a separate program.
    printf ( "Type abc123 and press enter.\n" );
    terminalResult = fgets ( userInput, sizeof ( userInput ), stdin );
    enteredInteger = atoi ( userInput );
    printf ( "The user entered the integer %d\n", enteredInteger );

    // Make the compiler think we are using this variable.
    ( void )terminalResult;

    return 0;
}