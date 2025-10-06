def main():
    # Don't forget to declare your variables as appropriate to the language, some languages will fail to compile with this program
    validName = 'a'
    wrongCase = 'a'
    wrOngLetter = 'a'
    
    # This will cause NameError - undefined variable
    print ( invalidName )
    # This will cause NameError - undefined variable
    print ( validname )
    # This will cause NameError - undefined variable
    print ( wrongcase )
    # This will cause NameError - undefined variable
    print ( wr0ngLetter )
 
    # Don't start your variables with numbers or use hyphens
    # This will cause SyntaxError - invalid identifier
    2NameInvalid = 5
    # This will cause SyntaxError - invalid identifier
    invalid-name = 'a'
    
    print ( 2NameInvalid )
    print ( invalid-name )
    
    # Also avoid using keywords already reserved by the programming language
    # This will cause SyntaxError - reserved keyword
    class = 1
    # This will cause SyntaxError - reserved keyword
    def = 2
    
    print ( class )
    print ( def )

if __name__ == "__main__":
    main()