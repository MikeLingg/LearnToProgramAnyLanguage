# Example with three default parameters
def threeDefaultParameters ( FirstParameter_Par=5, SecondParameter_Par=10, ThirdParameter_Par=15 ):
    print ( f"Parameters: {FirstParameter_Par} {SecondParameter_Par} {ThirdParameter_Par}" )

def main():
    threeDefaultParameters ( FirstParameter_Par=20, SecondParameter_Par=25, ThirdParameter_Par=30 )
    # Python supports named parameters so can set any parameter by name
    threeDefaultParameters ( SecondParameter_Par=25 )

if __name__ == "__main__":
    main()
