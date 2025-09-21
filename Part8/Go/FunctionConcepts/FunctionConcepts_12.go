// Computing factorial with loop
package main

import "fmt"

func factorial ( FactorialNumber_Par int ) int {
    totalFactorial := 1
    
    for factorialNumber := 1; factorialNumber <= FactorialNumber_Par; factorialNumber++ {
        totalFactorial = totalFactorial * factorialNumber
    }
    
    return totalFactorial
}

func main() {
    factorialResult := factorial ( 10 )
    fmt.Printf ( "Factorial of 10 is: %d\n", factorialResult )
}
