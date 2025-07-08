import math

def main ():
    # We can convert a string with a number to a int and then boolean, we cannot convert True to a number and then boolean.
    print ( "Type true and press enter." )
    user_input = input ()
    entered_boolean = bool ( int ( user_input ) )
    print ( f"The user entered the boolean {entered_boolean}" )

if __name__ == "__main__":
    main ()