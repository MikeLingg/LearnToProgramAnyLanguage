// An example of function variable scope
package main

import "fmt"

var GlobalVariable = 15
var GlobalToBeShadowed = 5

func myFunction() {
    myVariable := 55
    GlobalVariable = 42
    GlobalToBeShadowed := 15
    //_ = myVariable
    //_ = GlobalToBeShadowed
}

func main() {
    fmt.Printf ( "Global variable: %d\n", GlobalVariable )
    fmt.Printf ( "Global shadowed: %d\n", GlobalToBeShadowed )
    myFunction()
    fmt.Printf ( "Function variable: %d\n", myVariable ) // Will cause compile error
    fmt.Printf ( "Global variable: %d\n", GlobalVariable )
    fmt.Printf ( "Global shadowed: %d\n", GlobalToBeShadowed )
}
