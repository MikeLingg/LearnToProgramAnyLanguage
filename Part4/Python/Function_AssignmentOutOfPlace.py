def main():
    print( "Type 1 and press enter." )
    user_input = input()
    
    # Assignment operator placed incorrectly in declaration
    entered_integer int = ( user_input )
    
    print( f"The user entered the integer { entered_integer }" )

if __name__ == "__main__":
    main()
