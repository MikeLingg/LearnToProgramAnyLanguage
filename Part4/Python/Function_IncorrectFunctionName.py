def main():
    print( "Type 1 and press enter." )
    user_input = input()
    
    # Function name is wrong - should be int()
    entered_integer = string_to_number( user_input )
    
    print( f"The user entered the integer { entered_integer }" )

if __name__ == "__main__":
    main()
