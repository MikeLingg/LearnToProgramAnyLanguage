# Python does not access arguments through main.
import sys

def main():
    print ( f"Number of arguments: {len(sys.argv)}" )
    print ( "Arguments:" )
    for i, arg in enumerate ( sys.argv ):
        print ( f"\tArgument {i}: {arg}" )

if __name__ == "__main__":
    main()
