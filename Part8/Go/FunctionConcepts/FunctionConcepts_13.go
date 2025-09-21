// Go does not pass the function arguments to main, access via os.Args
package main

import (
    "fmt"
    "os"
)

func main() {
    fmt.Printf ( "Number of arguments: %d\n", len ( os.Args ) )
    fmt.Println ( "Arguments:" )
    for i, arg := range os.Args {
        fmt.Printf ( "\tArgument %d: %s\n", i, arg )
    }
}
