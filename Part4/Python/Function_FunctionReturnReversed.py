def main():
    print( "Type 1 and press enter." )
    user_input = input()
    
    # Trying to assign to function name instead of calling function
    int = entered_integer( user_input )
    
    print( f"The user entered the integer { entered_integer }" )

if __name__ == "__main__":
    main()
