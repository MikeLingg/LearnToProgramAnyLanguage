def main ():
    # Other languages might allow this dangerous behavior, python will not.
    class TestStruct:
        def __init__ ( self ):
            self.intArray = [ 0 ] * 10
            self.myInt = 0

    testStruct = TestStruct ()

    # In Python, this raisees an IndexError
    testStruct.intArray[ 10 ] = 55 


if __name__ == "__main__":
    main ()
