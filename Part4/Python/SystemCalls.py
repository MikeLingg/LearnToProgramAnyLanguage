import math

def main ():
    # Note: For this program to function as expected, the user will have to correctly enter the requested values.

    # Boolean strings of true/false cannot be converted to a bool variable without conditions,
    # so we will discuss how that works in the branches video coming soon.
    # Some languages will not even allow for reading values of 0 or 1 from the terminal as
    # booleans so we will identify which languages this fails with, and revisit how to make 
    # this work in the branches video.

    print ( "Type 55 and press enter." )
    user_input = input ()
    entered_integer = int ( user_input )
    print ( f"The user entered the integer {entered_integer}" )

    print ( "Type 0 and press enter." )
    user_input = input ()
    entered_boolean = bool ( user_input )
    print ( f"The user entered the boolean {entered_boolean}" )

    print ( "Type 1 and press enter." )
    user_input = input ()
    entered_boolean = bool ( user_input )
    print ( f"The user entered the boolean {entered_boolean}" )

    print ( "Type False and press enter." )
    user_input = input ()
    entered_boolean = bool ( user_input )
    print ( f"The user entered the boolean {entered_boolean}" )

    print ( "Type True and press enter." )
    user_input = input ()
    entered_boolean = bool ( user_input )
    print ( f"The user entered the boolean {entered_boolean}" )

    print ( "Type TRUE and press enter." )
    user_input = input ()
    entered_boolean = bool ( user_input )
    print ( f"The user entered the boolean {entered_boolean}" )

    print ( "Type 11 and press enter." )
    user_input = input ()
    entered_boolean = bool ( int ( user_input ) )
    print ( f"The user entered the boolean {entered_boolean}" )

    print ( "Type -1 and press enter." )
    user_input = input ()
    entered_boolean = bool ( int ( user_input ) )
    print ( f"The user entered the boolean {entered_boolean}" )

    print ( "Type 0 and press enter." )
    user_input = input ()
    entered_boolean = bool ( int ( user_input ) )
    print ( f"The user entered the boolean {entered_boolean}" )

    print ( "Type 55.5 and press enter." )
    user_input = input ()
    entered_float = float ( user_input )
    print ( f"The user entered the float {entered_float}" )
    print ( f"The user entered the float {entered_float:4.1f}" )
    print ( f"The user entered the float {entered_float:3.1f}" )
    print ( f"The user entered the float {entered_float:5.1f}" )
    print ( f"The user entered the float {entered_float:4.0f}" )
    print ( f"The user entered the float {entered_float:4.2f}" )

    print ( "Type Hello World! and press enter." )
    user_input = input ()
    print ( f"The user entered the string {user_input}" )

    print ( "Type  567 and press enter." )
    user_input = input ()
    entered_integer = int ( user_input )
    print ( f"The user entered the integer {entered_integer}" )

    print ( "Type +567 and press enter." )
    user_input = input ()
    entered_integer = int ( user_input )
    print ( f"The user entered the integer {entered_integer}" )

    print ( "Type -567 and press enter." )
    user_input = input ()
    entered_integer = int ( user_input )
    print ( f"The user entered the integer {entered_integer}" )

    # Python's abs () works with both integers and floats
    print ( f"Abs of -5 is {abs ( -5 )}" )
    print ( f"Abs of -5.5 is {abs ( -5.5 )}" )
    print ( f"Abs of a is {abs ( ord ( 'a' ) )}" )

    print ( f"Fabs of -5 is {math.fabs ( -5 )}" )
    print ( f"Fabs of -5.5 is {math.fabs ( -5.5 )}" )
    print ( f"Fabs of a is {math.fabs ( ord ( 'a' ) )}" )

    print ( f"Pow of 2^5 is {pow ( 2, 5 )}" )
    print ( f"Pow of 2.2^5.2 is {pow ( 2.2, 5.2 )}" )
    print ( f"Pow of a^b is {pow ( ord ( 'a' ), ord ( 'b' ) )}" )

    # Note trig functions are almost always in radians, not degrees
    print ( f"Sin of 90 is {math.sin ( 90 )}" )
    print ( f"Sin of pi/2 is {math.sin ( 3.14159265358979323846 / 2 )}" )

    print ( f"Cos of 180 is {math.cos ( 180 )}" )
    print ( f"Cos of pi is {math.cos ( 3.14159265358979323846 )}" )

    # Rounding type functions are very useful for explicit float to int conversions
    print ( f"Floor of 5.5 is {math.floor ( 5.5 )}" )
    print ( f"Floor of -5.5 is {math.floor ( -5.5 )}" )

    print ( f"Ceil of 5.5 is {math.ceil ( 5.5 )}" )
    print ( f"Ceil of -5.5 is {math.ceil ( -5.5 )}" )

    print ( f"Round of 5.5 is {round ( 5.5 )}" )
    print ( f"Round of -5.5 is {round ( -5.5 )}" )

    print ( f"Trunc of 5.5 is {math.trunc ( 5.5 )}" )
    print ( f"Trunc of -5.5 is {math.trunc ( -5.5 )}" )

if __name__ == "__main__":
    main ()