# Computing factorial with loop
def factorial ( FactorialNumber_Par ):
    totalFactorial = 1
    
    for factorialNumber in range ( 1, FactorialNumber_Par + 1 ):
        totalFactorial = totalFactorial * factorialNumber
    
    return totalFactorial

def main():
    factorialResult = factorial ( FactorialNumber_Par=10 )
    print ( f"Factorial of 10 is: {factorialResult}" )

if __name__ == "__main__":
    main()
