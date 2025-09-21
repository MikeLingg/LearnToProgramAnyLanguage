// Recursive function call to compute factorial
package main

import "fmt"

func factorial ( FactorialNumber_Par int ) int {
    if FactorialNumber_Par <= 1 {
        return 1
    }
    
    return FactorialNumber_Par * factorial ( FactorialNumber_Par - 1 )
}

func main() {
    factorialResult := factorial ( 10 )
    fmt.Printf ( "Factorial of 10 is: %d\n", factorialResult )
}
