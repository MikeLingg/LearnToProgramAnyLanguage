# Recursive function call to compute factorial
def factorial ( FactorialNumber_Par ):
    if FactorialNumber_Par <= 1:
        return 1
    
    return FactorialNumber_Par * factorial ( FactorialNumber_Par - 1 )

def main():
    factorialResult = factorial ( FactorialNumber_Par=10 )
    print ( f"Factorial of 10 is: {factorialResult}" )

if __name__ == "__main__":
    main()
