def main():
    user_input = ""
    entered_integer = None
    
    print( "Type 1 and press enter." )
    user_input = input()
    
    # Function called but return value ignored
    int( user_input )
    
    print( f"The user entered the integer { entered_integer }" )
    
if __name__ == "__main__":
    main()
