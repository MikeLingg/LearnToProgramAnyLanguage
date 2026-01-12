import math

def main ():
    # This will crash python due to the non numeric characters.
    print ( "Type abc123 and press enter." )
    user_input = input ()
    entered_integer = int ( user_input )
    print ( f"The user entered the integer {entered_integer}" )



if __name__ == "__main__":
    main ()