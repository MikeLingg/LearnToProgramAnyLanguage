package main

import (
	"fmt"
	"math"
)

const (
	colorReset = "\x1b[0m"
	colorBold  = "\x1b[1m"
)

func printBgRGB(r, g, b uint8, text string) {
	fmt.Printf("\x1b[48;2;%d;%d;%dm%s%s", r, g, b, text, colorReset)
}

func valueToColor(normalized float64) (uint8, uint8, uint8) {
	var r, g, b uint8
	if normalized < 0.5 {
		// Blue to Green
		t := normalized * 2.0
		r = 0
		g = uint8(t * 255.0)
		b = uint8((1.0 - t) * 255.0)
	} else {
		// Green to Red
		t := (normalized - 0.5) * 2.0
		r = uint8(t * 255.0)
		g = uint8((1.0 - t) * 255.0)
		b = 0
	}
	return r, g, b
}

func demonstrate8bit() {
	fmt.Printf("\n%s=== 8-BIT UNSIGNED (0-255) ===%s\n", colorBold, colorReset)
	fmt.Println("256 possible values - Coarse granularity\n")

	values := []uint16{10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 127, 255}

	for _, val := range values {
		normalized := float64(val) / 255.0
		r, g, b := valueToColor(normalized)

		fmt.Printf("  %3d: ", val)
		printBgRGB(r, g, b, "    ")
		fmt.Printf(" RGB(%3d,%3d,%3d)\n", r, g, b)
	}
}

func demonstrate16bit() {
	fmt.Printf("\n%s=== 16-BIT UNSIGNED (0-65535) ===%s\n", colorBold, colorReset)
	fmt.Println("65,536 possible values - 256x finer than 8-bit\n")

	values := []uint32{10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 3000, 6000, 9000, 12000, 15000, 18000, 21000, 24000, 27000, 32767, 65535}

	for _, val := range values {
		normalized := float64(val) / 65535.0
		r, g, b := valueToColor(normalized)

		fmt.Printf("  %5d: ", val)
		printBgRGB(r, g, b, "    ")
		fmt.Printf(" RGB(%3d,%3d,%3d)\n", r, g, b)
	}
}

func demonstrate32bit() {
	fmt.Printf("\n%s=== 32-BIT UNSIGNED (0-4294967295) ===%s\n", colorBold, colorReset)
	fmt.Println("4,294,967,296 possible values - 65,536x finer than 16-bit\n")

	values := []uint64{10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 250000000, 500000000, 1000000000, 1500000000, 2000000000,
		2147483647, 4294967295}

	for _, val := range values {
		normalized := float64(val) / 4294967295.0
		r, g, b := valueToColor(normalized)

		fmt.Printf("  %10d: ", val)
		printBgRGB(r, g, b, "    ")
		fmt.Printf(" RGB(%3d,%3d,%3d)\n", r, g, b)
	}
}

func demonstrate64bit() {
	fmt.Printf("\n%s=== 64-BIT UNSIGNED (0-18446744073709551615) ===%s\n", colorBold, colorReset)
	fmt.Println("18,446,744,073,709,551,616 possible values - 4,294,967,296x finer than 32-bit\n")

	values := []uint64{10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 625000000000000000, 1250000000000000000, 2500000000000000000, 5000000000000000000,
		9223372036854775807, 18446744073709551615}

	for _, val := range values {
		normalized := float64(val) / 18446744073709551615.0
		r, g, b := valueToColor(normalized)

		fmt.Printf("  %20d: ", val)
		printBgRGB(r, g, b, "    ")
		fmt.Printf(" RGB(%3d,%3d,%3d)\n", r, g, b)
	}
}

func main() {
	// This signifies a number of basic concepts that would be included in one program,
	// except some of these concepts will prevent compilation or crash the program.
	// So this program will be broken up as specific languages require it.

	var falseBoolean bool = true
	// Note colon equal allows implicit variable typing, I'm not going to use this.
	trueBoolean := false

	fmt.Printf("Boolean range: %t %t\n", falseBoolean, trueBoolean)

	var minSigned8 int8 = -128
	var maxSigned8 int8 = 127
	var minUnsigned8 uint8 = 0
	var maxUnsigned8 uint8 = 255

	fmt.Printf("8 bit signed int range: %d %d\n", minSigned8, maxSigned8)
	fmt.Printf("8 bit unsigned int range: %d %d\n", minUnsigned8, maxUnsigned8)

	var minSigned16 int16 = -32768
	var maxSigned16 int16 = 32767
	var minUnsigned16 uint16 = 0
	var maxUnsigned16 uint16 = 65535

	fmt.Printf("16 bit signed int range: %d %d\n", minSigned16, maxSigned16)
	fmt.Printf("16 bit unsigned int range: %d %d\n", minUnsigned16, maxUnsigned16)

	var minSigned32 int32 = -2147483648
	var maxSigned32 int32 = 2147483647
	var minUnsigned32 uint32 = 0
	var maxUnsigned32 uint32 = 4294967295

	fmt.Printf("32 bit signed int range: %d %d\n", minSigned32, maxSigned32)
	fmt.Printf("32 bit unsigned int range: %d %d\n", minUnsigned32, maxUnsigned32)

	var minSigned64 int64 = -9223372036854775808
	var maxSigned64 int64 = 9223372036854775807
	var minUnsigned64 uint64 = 0
	var maxUnsigned64 uint64 = 18446744073709551615

	fmt.Printf("64 bit signed int range: %d %d\n", minSigned64, maxSigned64)
	fmt.Printf("64 bit unsigned int range: %d %d\n", minUnsigned64, maxUnsigned64)

	fmt.Printf("\n")

	demonstrate8bit()
	demonstrate16bit()
	demonstrate32bit()
	demonstrate64bit()

	fmt.Printf("\n")

	var floatMax float32 = float32(math.MaxFloat32)
	var floatMin float32 = float32(math.SmallestNonzeroFloat32)

	fmt.Printf("Note that scientific notation must be used to print such a small number.\n")
	fmt.Printf("32 bit float: %e %f\n", floatMin, floatMax)

	var zeroPointOne float32 = float32(0.1)
	var zeroPointTwo float32 = float32(0.2)
	var zeroPointThree float32 = float32(0.3)

	// So let's look at how far off the actual floating point value is from the value it was set to.
	fmt.Printf("Floating point 0.1, 0.2, 0.3 -> %.17f and %.17f and %.17f\n", zeroPointOne, zeroPointTwo, zeroPointThree)

	fmt.Printf("\n")

	var doubleMax float64 = math.MaxFloat64
	var doubleMin float64 = math.SmallestNonzeroFloat64

	fmt.Printf("Note that scientific notation must be used to print such a small number.\n")
	fmt.Printf("64 bit float range: %e %f\n", doubleMin, doubleMax)

	fmt.Printf("\n")

	// Shows the basics of ASCII characters, including special ones.
	// This should print 1 followed by a tab followed by 2, then on the next line print 3.
	var charOne byte = '1'
	var charTab byte = '\t'
	var singleQuotes byte = '\''
	var charNewLine byte = '\n'
	var doubleQuotes byte = '"'
	fmt.Printf("Characters: %c%c%c%c%c\n", charOne, charTab, singleQuotes, charNewLine, doubleQuotes)

	// Show how printing as an integer, not a character, can be confusing
	fmt.Printf("charOne as an integer: %d\n", charOne)

	// Note some languages may not compile this code so it will be in a separate file, others may throw warnings.
	// Go will not allow implicit conversion from int to bool
	var outOfRangeBoolean bool = true

	fmt.Printf("\n")

	fmt.Printf("Out of range Boolean: %t\n", outOfRangeBoolean)

	// Note some languages may not compile this code so it will be in a separate file, others may throw warnings.
	// Go will detect overflow at compile time or runtime
	var outOfRange int16 = 32767

	fmt.Printf("Out of range value: %d\n", outOfRange)

	fmt.Printf("Note that go's compiler will block this overflow, will work with using inf.\n")
	var outOfRangeFloat float32 = float32(math.MaxFloat32)// float32(math.MaxFloat32) + float32(math.MaxFloat32)
	var outOfRangeDouble float64 = math.MaxFloat64// math.MaxFloat64 + math.MaxFloat64

	fmt.Printf("Out of range float and double: %f %f\n", outOfRangeFloat, outOfRangeDouble)

	fmt.Printf("\n")

	// Go uses runes (int32) for characters, can handle larger values
	var outOfRangeChar byte = byte(65)//byte(257) // Go does not allow this, use rune

	fmt.Printf("Out of range char: %c\n", outOfRangeChar)
}
