# This example will not run as we cannot call a variable like a function
def myFunction():
    print ( "Called myFunction" )

def main():
    myVariable = 5
    myVariable()  # Will cause TypeError

if __name__ == "__main__":
    main()
