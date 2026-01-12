import math

def main ():
    # This will crash Python with ValueError for invalid input
    print ( "Type Hello World! and press enter." )
    user_input = input ()
    entered_integer = int ( user_input )
    print ( f"The user entered the integer {entered_integer}" )



if __name__ == "__main__":
    main ()