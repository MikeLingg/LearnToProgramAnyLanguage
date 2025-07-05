package main

import "fmt"

func main() {
	// Don't forget to declare your variables as appropriate to the language, some languages will fail to compile with this program
	validName := 'a'
	wrongCase := 'a'
	wrOngLetter := 'a'
	
	// This will cause compile error - undefined variable
	fmt.Printf ( "%c\n", invalidName )
	// This will cause compile error - undefined variable
	fmt.Printf ( "%c\n", validname )
	// This will cause compile error - undefined variable
	fmt.Printf ( "%c\n", wrongcase )
	// This will cause compile error - undefined variable
	fmt.Printf ( "%c\n", wr0ngLetter )
 
	// Don't start your variables with numbers or use hyphens
	// This will cause compile error - invalid identifier
	2NameInvalid := 5
	// This will cause compile error - invalid identifier
	invalid-name := 'a'
	
	fmt.Printf ( "%d\n", 2NameInvalid )
	fmt.Printf ( "%c\n", invalid-name )
	
	// Also avoid using keywords already reserved by the programming language
	// This will cause compile error - reserved keyword
	func := 1
	// This will cause compile error - reserved keyword
	package := 2
	
	fmt.Printf ( "%d\n", func )
	fmt.Printf ( "%d\n", package )
}
