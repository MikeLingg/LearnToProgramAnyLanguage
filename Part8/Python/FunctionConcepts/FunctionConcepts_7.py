# Example of function overloading
# Python does not support traditional function overloading, last definition wins
def myFunction ( IntParameter_Par ):
    print ( f"Int version of my function called {IntParameter_Par}" )

def myFunction ( DoubleParameter_Par ):
    print ( f"Double version of my function called {DoubleParameter_Par}" )

def main():
    myFunction ( 5 )
    myFunction ( 5.5 )

if __name__ == "__main__":
    main()
