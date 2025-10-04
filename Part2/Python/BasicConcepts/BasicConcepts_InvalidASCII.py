def main():
    # Python allows multi-character strings, unlike C++
    invalidTab = char ( 'TAB' )
    print ( "Invalid Tab:", invalidTab )

    # If you want a tab you have to use \ escape characters.
    validTab = '\t'
    print ( "Valid Tab:", validTab, ":" )

if __name__ == "__main__":
    main()