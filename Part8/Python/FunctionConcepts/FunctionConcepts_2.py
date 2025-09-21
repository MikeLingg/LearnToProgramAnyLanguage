# An example of function variable scope
GlobalVariable = 15
GlobalToBeShadowed = 5

def myFunction():
    global GlobalVariable
    myVariable = 55
    GlobalVariable = 42
    globalToBeShadowed = 15

def main():
    print ( f"Global variable: {GlobalVariable}" )
    print ( f"Global shadowed: {GlobalToBeShadowed}" )
    myFunction()
    print ( f"Function variable: {myVariable}" )  # Will cause NameError
    print ( f"Global variable: {GlobalVariable}" )
    print ( f"Global shadowed: {GlobalToBeShadowed}" )

if __name__ == "__main__":
    main()
