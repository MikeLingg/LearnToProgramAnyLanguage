package main

import (
	"fmt"
	"math"
)

func main() {
	// This signifies a number of basic concepts that would be included in one program, 
	// except some of these concepts will prevent compilation or crash the program. 
	// So this program will be broken up as specific languages require it.

	var falseBoolean bool = true
	// Note colon equal allows implicit variable typing, I'm not going to use this.
	trueBoolean := false
	
	fmt.Printf ( "Boolean range: %t %t\n", falseBoolean, trueBoolean )
	
	var minSigned8 int8 = -128
	var maxSigned8 int8 = 127
	var minUnsigned8 uint8 = 0
	var maxUnsigned8 uint8 = 255
	
	fmt.Printf ( "8 bit signed int range: %d %d\n", minSigned8, maxSigned8 )
	fmt.Printf ( "8 bit unsigned int range: %d %d\n", minUnsigned8, maxUnsigned8 )
	
	var minSigned16 int16 = -32768
	var maxSigned16 int16 = 32767
	var minUnsigned16 uint16 = 0
	var maxUnsigned16 uint16 = 65535
	
	fmt.Printf ( "16 bit signed int range: %d %d\n", minSigned16, maxSigned16 )
	fmt.Printf ( "16 bit unsigned int range: %d %d\n", minUnsigned16, maxUnsigned16 )
	
	var minSigned32 int32 = -2147483648
	var maxSigned32 int32 = 2147483647
	var minUnsigned32 uint32 = 0
	var maxUnsigned32 uint32 = 4294967295
	
	fmt.Printf ( "32 bit signed int range: %d %d\n", minSigned32, maxSigned32 )
	fmt.Printf ( "32 bit unsigned int range: %d %d\n", minUnsigned32, maxUnsigned32 )
	
	fmt.Printf ( "Note: Go handles large integers within specified bit ranges.\n" )
	var minSigned64 int64 = -9223372036854775808
	var maxSigned64 int64 = 9223372036854775807
	var minUnsigned64 uint64 = 0
	var maxUnsigned64 uint64 = 18446744073709551615
	
	fmt.Printf ( "64 bit signed int range: %d %d\n", minSigned64, maxSigned64 )
	fmt.Printf ( "64 bit unsigned int range: %d %d\n", minUnsigned64, maxUnsigned64 )
	
	var floatMax float32 = float32 ( math.MaxFloat32 )
	var floatMin float32 = float32 ( math.SmallestNonzeroFloat32 )
	
	fmt.Printf ( "Note that scientific notation must be used to print such a small number.\n" )
	fmt.Printf ( "32 bit float: %e %f\n", floatMin, floatMax )
	
	var zeroPointOne float32 = float32 ( 0.1 )
	var zeroPointTwo float32 = float32 ( 0.2 )
	var zeroPointThree float32 = float32 ( 0.3 )
	
	// So let's look at how far off the actual floating point value is from the value it was set to.
	fmt.Printf ( "Floating point 0.1, 0.2, 0.3 -> %.17f and %.17f and %.17f\n", zeroPointOne, zeroPointTwo, zeroPointThree )
	
	var doubleMax float64 = math.MaxFloat64
	var doubleMin float64 = math.SmallestNonzeroFloat64
	
	fmt.Printf ( "Note that scientific notation must be used to print such a small number.\n" )
	fmt.Printf ( "64 bit float range: %e %f\n", doubleMin, doubleMax )
	
	// Shows the basics of ASCII characters, including special ones.
	// This should print 1 followed by a tab followed by 2, then on the next line print 3.
	var charOne byte = '1'
	var charTab byte = '\t'
	var charTwo byte = '2'
	var charNewLine byte = '\n'
	var charThree byte = '3'
	fmt.Printf ( "Characters: %c%c%c%c%c\n", charOne, charTab, charTwo, charNewLine, charThree )
	
	// Show how printing as an integer, not a character, can be confusing
	fmt.Printf ( "charOne as an integer: %d\n", charOne )
	
	// Note some languages may not compile this code so it will be in a separate file, others may throw warnings.
	// Go will not allow implicit conversion from int to bool
	var outOfRangeBoolean bool = true
	
	fmt.Printf ( "Out of range Boolean: %t\n", outOfRangeBoolean )
	
	// Note some languages may not compile this code so it will be in a separate file, others may throw warnings.
	// Go will detect overflow at compile time or runtime
	var outOfRange int16 = 32767
	
	fmt.Printf ( "Out of range value: %d\n", outOfRange )
	
	fmt.Printf ( "Note that go's compiler will block this overflow, will work with using inf.\n" )
	var outOfRangeFloat float32 = float32 ( math.MaxFloat32 ) + float32 ( math.MaxFloat32 ) // float32 ( math.Inf ( 1 ) )
	var outOfRangeDouble float64 = math.MaxFloat64 + math.MaxFloat64 // 
	
	fmt.Printf ( "Out of range float and double: %f %f\n", outOfRangeFloat, outOfRangeDouble )

	// Go uses runes (int32) for characters, can handle larger values
	var outOfRangeChar byte = byte ( 257 ) // Go does not allow this, use rune

	fmt.Printf ( "Out of range char: %c\n", outOfRangeChar )
}
