def main():
    extra_input = 5
    
    print( "Type 1 and press enter." )
    user_input = input()
    
    # Function only takes 1 parameter, but we're passing 2
    entered_integer = int( user_input, extra_input )
    
    print( f"The user entered the integer { entered_integer }" )

if __name__ == "__main__":
    main()
