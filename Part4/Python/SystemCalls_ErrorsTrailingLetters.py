import math

def main ():
    # This will crash python due to the non numeric characters.
    print ( "Type 123abc and press enter." )
    user_input = input ()
    entered_integer = int ( user_input )
    print ( f"The user entered the integer {entered_integer}" )

    # This will crash python due to the non numeric characters.
    print ( "Type abc123 and press enter." )
    user_input = input ()
    entered_integer = int ( user_input )
    print ( f"The user entered the integer {entered_integer}" )

    # This will crash Python with ValueError for invalid input
    print ( "Type Hello World! and press enter." )
    user_input = input ()
    entered_integer = int ( user_input )
    print ( f"The user entered the integer {entered_integer}" )

    # This will crash Python with ValueError for invalid input
    print ( "Type abc123 and press enter." )
    user_input = input ()
    entered_integer = int ( user_input )
    print ( f"The user entered the integer {entered_integer}" )

    # This will crash python as python will not convert a float string to an integer.
    # Other languages will allow this.
    print ( "Type 123.45 and press enter." )
    user_input = input ()
    entered_integer = int ( user_input )
    print ( f"The user entered the integer {entered_integer}" )


if __name__ == "__main__":
    main ()