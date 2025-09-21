// Example with three default parameters
// Go does not support default parameters, this shows limitation
package main

import "fmt"

func threeDefaultParameters ( FirstParameter_Par int, SecondParameter_Par int, ThirdParameter_Par int ) {
    fmt.Printf ( "Parameters: %d %d %d\n", FirstParameter_Par, SecondParameter_Par, ThirdParameter_Par )
}

func main() {
    threeDefaultParameters ( 20, 25, 30 )
    // Go does not support default parameters, must provide all arguments
    threeDefaultParameters ( 5, 10, 15 )
}
