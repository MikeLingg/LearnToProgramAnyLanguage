def main():
    # I think in some programs this will cause a crash or failure to compile, so it will be in a separate program.
    # Python doesn't have uninitialized variables - this will cause NameError
    print ( "myCharacter:", myCharacter, "as int:", ord ( myCharacter ) )

if __name__ == "__main__":
    main()