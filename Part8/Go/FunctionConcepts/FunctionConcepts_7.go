// Example of function overloading
// Go does not support function overloading, this will not compile
package main

import "fmt"

func myFunction ( IntParameter_Par int ) {
    fmt.Printf ( "Int version of my function called %d\n", IntParameter_Par )
}

func myFunction ( DoubleParameter_Par float64 ) {
    fmt.Printf ( "Double version of my function called %.1f\n", DoubleParameter_Par )
}

func main() {
    myFunction ( 5 )
    myFunction ( 5.5 )
}
