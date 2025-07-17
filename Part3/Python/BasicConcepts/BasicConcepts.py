def main ():
    # Note the structured example is assuming zero based indexing.
    # One based index languages will differ.
    # Also note how some references are array location, with ranges from first to last, 
    # while indexes being zero based are 0 to size - 1.

    temperatures = [ 55, 58, 60, 65, 70, 73, 76, 79, 81, 83, 84, 85, 85, 84, 83, 81, 78, 75, 72, 69, 65, 62, 59, 57 ]
    testScores = [ 95, 75, 86, 86, 78, 94 ]
    bookNumber = [ 12495, 35786, 15863, 84962, 42697 ]

    # 10th entry (0-based index 9)
    print ( f"Temperature at tenth hour is { temperatures[ 9 ] }" )
    # 4th entry (0-based index 3)
    print ( f"Fourth student grade is { testScores[ 3 ] }" )

    # 2nd entry (0-based index 1)
    bookTwoIndex = 1
    print ( f"Second book index is { bookNumber[ bookTwoIndex ] }" )

    # First and last 0 based indexes, 0 and array size - 1.
    hourCount = 24
    firstTemperatureIndex = 0
    lastTemperatureIndex = hourCount - 1
    print ( f"First temperature is { temperatures[ firstTemperatureIndex ] }" )
    print ( f"Last temperature is { temperatures[ lastTemperatureIndex ] }" )

    # set temperature first entry to 65
    temperatures[ 0 ] = 65
    print ( f"First temperature is now { temperatures[ 0 ] }" )

    # set testScores fourth entry to 99
    testScores[ 3 ] = 99
    print ( f"Fourth test score is now { testScores[ 3 ] }" )

    # set bookNumber at index third entry to 75681
    bookIndex = 2
    bookNumber[ bookIndex ] = 75681
    print ( f"Third book number is now { bookNumber[ bookIndex ] }" )

    # Large arrays - Python lists are dynamic, but we can create them with initial values
    largeArray = [ False ] * 10000
    largeArray1 = [ 0 ] * 1000
    largeArray2 = [ 0.0 ] * 5000

    print ( f"First large array first and last initial values: { largeArray[ 0 ] } { largeArray[ 9999 ] }" )
    print ( f"Second large array first and last initial values: { largeArray1[ 0 ] } { largeArray1[ 999 ] }" )
    print ( f"Third large array first and last initial values: { largeArray2[ 0 ] } { largeArray2[ 4999 ] }" )

    # set largeArray first entry to True
    largeArray[ 0 ] = True
    # set largeArray last entry to False
    largeArray[ 9999 ] = False
    print ( f"First large array first and last values: { largeArray[ 0 ] } { largeArray[ 9999 ] }" )

    # set largeArray1 first entry to 25
    largeArray1[ 0 ] = 25
    # set largeArray1 last entry to 55
    largeArray1[ 999 ] = 55
    print ( f"Second large array first and last values: { largeArray1[ 0 ] } { largeArray1[ 999 ] }" )

    # set largeArray2 first entry to 27.5
    largeArray2[ 0 ] = 27.5
    # set largeArray2 last entry to 58.25
    largeArray2[ 4999 ] = 58.25
    print ( f"Third large array first and last values: { largeArray2[ 0 ] } { largeArray2[ 4999 ] }" )

    # Character array (Python strings are immutable, but we can use lists of characters)
    # Pre-allocate list with empty characters, then set each one individually
    myString = [ '' ] * 100  # Pre-size the array like C
    myString[ 0 ] = 'H'
    myString[ 1 ] = 'e'
    myString[ 2 ] = 'l'
    myString[ 3 ] = 'l'
    myString[ 4 ] = 'o'
    myString[ 5 ] = ' '
    myString[ 6 ] = 'W'
    myString[ 7 ] = 'o'
    myString[ 8 ] = 'r'
    myString[ 9 ] = 'l'
    myString[ 10 ] = 'd'
    myString[ 11 ] = '.'
    myString[ 12 ] = '\0'  # Null terminator (not needed in Python but for C similarity)
    
    # Python doesn't automatically detect null terminator, so we need to find it manually
    nullIndex = myString.index( '\0' ) if '\0' in myString else len( myString )
    print ( ''.join( myString[ :nullIndex ] ) )  # Print up to null terminator

    myString1 = [ 'H', 'e', 'l', 'l', 'o', ' ', 'W', 'o', 'r', 'l', 'd', '.' ]
    print ( ''.join( myString1 ) )

    # Note: Python strings are immutable, so this is just a regular string
    myString2 = "Hello World."
    print ( myString2 )

    # Python doesn't have the same memory layout concepts as C/C++
    # We'll simulate the dangerous operation by showing what would happen
    # if we could access beyond array bounds (Python prevents this with exceptions)

    # Simulating the struct behavior - Python doesn't have structs, but we can use a class
    # or simulate with separate variables
    class TestStruct:
        def __init__ ( self ):
            self.intArray = [ 0 ] * 10
            self.myInt = 0

    testStruct = TestStruct ()

    # In Python, this would raise an IndexError, but let's show what the concept means
    # testStruct.intArray[ 10 ] = 55  # This would raise IndexError in Python

    # Instead, let's demonstrate the concept by manually simulating the behavior
    print ( "# Python prevents buffer overflows with bounds checking" )
    print ( "# The following would raise IndexError in Python:" )
    print ( "# testStruct.intArray[ 10 ] = 55" )

    # Let's simulate what would happen if Python allowed this
    testStruct.myInt = 55  # Simulating the corruption that would happen in C/C++
    print ( f"myInt value: { testStruct.myInt }" )

    testStruct.myInt = testStruct.myInt + 1
    print ( f"Simulated out of bounds effect: { testStruct.myInt }" )

    # We can demonstrate actual IndexError if you want to see Python's safety mechanism
    try:
        testStruct.intArray[ 10 ] = 55
    except IndexError as e:
        print ( f"Python safety mechanism: { e }" )

    # 2D Array (nested lists in Python)
    twoDArray = [ [ '' for _ in range ( 4 ) ] for _ in range ( 4 ) ]
    twoDArray[ 0 ][ 0 ] = '0'
    twoDArray[ 0 ][ 1 ] = '1'
    twoDArray[ 0 ][ 2 ] = '2'
    twoDArray[ 0 ][ 3 ] = '3'
    twoDArray[ 1 ][ 0 ] = '4'
    twoDArray[ 1 ][ 1 ] = '5'
    twoDArray[ 1 ][ 2 ] = '6'
    twoDArray[ 1 ][ 3 ] = '7'
    twoDArray[ 2 ][ 0 ] = '8'
    twoDArray[ 2 ][ 1 ] = '9'
    twoDArray[ 2 ][ 2 ] = 'A'
    twoDArray[ 2 ][ 3 ] = 'B'
    twoDArray[ 3 ][ 0 ] = 'C'
    twoDArray[ 3 ][ 1 ] = 'D'
    twoDArray[ 3 ][ 2 ] = 'E'
    twoDArray[ 3 ][ 3 ] = 'F'

    # Note: the actual implementation of this code will use some advanced 
    # techniques that will not be described, only the results of the code observed.
    print ( "twoDArray memory location as flat data: ", end='' )
    for i in range ( 4 ):
        for j in range ( 4 ):
            print ( twoDArray[ i ][ j ], end='' )
    print ()

    # Note these are not defined as constant, but the capital naming 
    # indicates the values should not change.
    RED = 0
    GREEN = 1
    BLUE = 2
    YELLOW = 3
    CYAN = 4
    MAGENTA = 5
    WHITE = 6

    # Columns: Red Intensity, Green Intensity, Blue Intensity
    colorTable = [
        [ 255, 0,   0   ],  # Red
        [ 0,   255, 0   ],  # Green
        [ 0,   0,   255 ],  # Blue
        [ 255, 255, 0   ],  # Yellow = Red + Green
        [ 0,   255, 255 ],  # Cyan   = Green + Blue
        [ 255, 0,   255 ],  # Magenta = Red + Blue
        [ 255, 255, 255 ]   # White = Red + Green + Blue
    ]

    print ( f"CYAN color values: { colorTable[ CYAN ][ 0 ] } { colorTable[ CYAN ][ 1 ] } { colorTable[ CYAN ][ 2 ] }" )

if __name__ == "__main__":
    main ()