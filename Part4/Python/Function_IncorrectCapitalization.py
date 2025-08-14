def main():
    print( "Type 1 and press enter." )
    user_input = input()
    
    # Function name has wrong capitalization
    entered_integer = Int( user_input )
    
    print( f"The user entered the integer { entered_integer }" )

if __name__ == "__main__":
    main()
