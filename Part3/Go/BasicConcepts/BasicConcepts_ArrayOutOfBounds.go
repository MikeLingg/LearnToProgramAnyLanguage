package main

import (
	"fmt"
	"unsafe"
)

type TestStruct struct {
	intArray [ 10 ]int
	myInt    int
}

func main () {
	// In Go, buffer overflows are prevented by bounds checking in safe code
	// This code demonstrates what would happen in unsafe code
	// The following would panic in safe Go:
	// testStruct.intArray[ 10 ] = 55  // This would panic with index out of bounds

	// Using unsafe code to demonstrate buffer overflow (dangerous!)
	testStruct := TestStruct{ }


	
	// UNSAFE: This demonstrates the dangerous buffer overflow
	// Convert struct to byte slice to access raw memory
	intArrayPtr := ( *[ 11 ]int )( unsafe.Pointer( &testStruct.intArray[ 0 ] ) )
	
	// This is the dangerous operation - writing beyond array bounds
	testStruct.myInt = 55

	// myInt should now be corrupted due to buffer overflow
	fmt.Printf ( "myInt value: %d\n", testStruct.myInt )

	intArrayPtr[ 10 ] = intArrayPtr[ 10 ] + 1

	// Reading the out-of-bounds location shows the change
	fmt.Printf ( "Out of bounds array value: %d\n", testStruct.myInt )
}
