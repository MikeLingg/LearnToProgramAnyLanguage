import sys

def main():
    # This signifies a number of basic concepts that would be included in one program, 
    # except some of these concepts will prevent compilation or crash the program. 
    # So this program will be broken up as specific languages require it.

    falseBoolean = False
    trueBoolean = True
    
    print ( "Boolean range:", falseBoolean, trueBoolean )
    
    print ( "Note: Python handles any range of integers automatically, any number of any size is valid unlike C++ and other typed languages." )

    veryLargeInt = 1234567890123456789012345678901234567890
    veryNegativeInt = -1234567890123456789012345678901234567890

    print ( "Python very large number:", veryLargeInt, veryNegativeInt)

    doubleMax = sys.float_info.max
    doubleMin = sys.float_info.min
    
    print ( "Note that scientific notation must be used to print such a small number." )
    print ( "Python float range:", doubleMin, doubleMax )
    
    zeroPointOne = 0.1
    zeroPointTwo = 0.2
    zeroPointThree = 0.3
    
    # So let's look at how far off the actual floating point value is from the value it was set to.
    print ( "Floating point 0.1, 0.2, 0.3 ->", "{:.17f}".format ( zeroPointOne ), "and", "{:.17f}".format ( zeroPointTwo ), "and", "{:.17f}".format ( zeroPointThree ) )
    
    # Shows the basics of ASCII characters, including special ones.
    # This should print 1 followed by a tab followed by 2, then on the next line print 3.
    charOne = '1'
    charTab = '\t'
    singleQuotes = '\''
    charNewLine = '\n'
    doubleQuotes = "\""
    print ( "Characters:", charOne + charTab + singleQuotes + charNewLine + doubleQuotes )
    
    # Show how printing as an integer, not a character, can be confusing
    print ( "charOne as an integer:", ord ( charOne ) )
    
    # Note some languages may not compile this code so it will be in a separate file, others may throw warnings.
    # Python treats any non-zero as True
    outOfRangeBoolean = bool ( 55 )
    
    print ( "Out of range Boolean:", outOfRangeBoolean )
    
    # Note some languages may not compile this code so it will be in a separate file, others may throw warnings.
    # Python handles large integers automatically
    outOfRange = 100000
    
    print ( "Out of range value, not a problem in Python:", outOfRange )
    
    outOfRangeFloat = sys.float_info.max + sys.float_info.max
    
    print ( "Out of range float:", outOfRangeFloat )

    # Python allows larger character values
    outOfRangeChar = chr ( 257 )

    print ( "Out of range char:", outOfRangeChar )

if __name__ == "__main__":
    main()