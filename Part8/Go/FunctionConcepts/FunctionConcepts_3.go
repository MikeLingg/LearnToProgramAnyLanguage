// This example will not compile as we cannot call a variable like a function
package main

import "fmt"

func myFunction() {
    fmt.Println ( "Called myFunction" )
}

func main() {
    myVariable := 5
    myVariable() // Will cause compile error
}
