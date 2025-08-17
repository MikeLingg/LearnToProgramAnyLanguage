package main

import "fmt"

func main() {
	// Most programs allow this operation, but where is the result? We can't use it!
	// Go will not allow this to compile
	5 + 3

	// Simple assignment of a constant and a variable to another variable:
	a := 1
	b := a
	fmt.Printf("Our two variables %d and %d\n", a, b)

	// Assignment errors - This code will likely go in its own separate program.
	var c int
	a = c
	c = c + 1
	// Go will not allow this to compile
	1 = a
	fmt.Printf("Our incorrectly assigned variables %d and %d\n", a, c)

	// Now let's look at assigning the result of basic operations
	// Assign the result of a mathematical operation
	value1 := 5
	value2 := 6
	sumValue := value1 + value2
	fmt.Printf("First value ( %d ) plus second value ( %d ) is %d\n", value1, value2, sumValue)

	// Assign the result of a comparison operation
	value3 := 7
	value4 := 8
	comparisonValue := (value3 >= value4)
	fmt.Printf("First value ( %d ) greater than or equal to second value ( %d ) is %t\n", value3, value4, comparisonValue)

	// Assign the result of a logical operation
	value5 := true
	value6 := false
	logicalValue := value5 || value6
	fmt.Printf("First value ( %t ) ORed with second value ( %t ) is %t\n", value5, value6, logicalValue)

	// Storing a basic complex operation, multiple additions
	sumValue = 1 + 2 + 3
	fmt.Printf("The sum of 1 + 2 + 3 is %d\n", sumValue)

	// Complex logical operation with differing operation priorities
	loyaltyMember := false
	purchased5Coffees := true
	haveCoupon := true
	couponNotExpired := true
	freeCoffee := loyaltyMember && purchased5Coffees || haveCoupon && couponNotExpired
	fmt.Printf("Customer gets a free coffee %t\n", freeCoffee)

	// The code above is basically the same as the following, except the extra variables
	// Sometimes breaking apart complex statements can help to self document
	loyaltyAchieved := loyaltyMember && purchased5Coffees
	haveValidCoupon := haveCoupon && couponNotExpired
	freeCoffee = loyaltyAchieved && haveValidCoupon
	fmt.Printf("Customer gets a free coffee %t\n", freeCoffee)

	// This code shows how a value will be computed incorrectly
	// with default operator precedence
	itemPrice := 99.99
	itemShipping := 9.99
	purchaseQuantity := 5
	totalCost := itemPrice + itemShipping*float64(purchaseQuantity)
	fmt.Printf("Total item cost is %.2f\n", totalCost)

	// We can correct this with parenthesis which force operations to complete first
	totalCost = (itemPrice + itemShipping) * float64(purchaseQuantity)
	fmt.Printf("Total item cost is %.2f\n", totalCost)

	// Parenthesis can be used to clarify precedence without
	// having to know what the actual operator precedence is.
	freeCoffee = ((loyaltyMember == true) && (purchased5Coffees == true)) || ((haveCoupon == true) && (couponNotExpired == true))

	// Some common errors, I can't really write these as a structured
	// language program, so these are more pseudo code examples.
	// Do you want to assign 5 to a and b, or do you want to check if b is equal to 5?
	// Go does not allow a = b = 5, so it is somewhat less confusing.
	b = 5
	a = b
	// vs
	a = ( b == 5 )

	// Spot the incorrect operator.
	// Go does not allow this mistake to compile
	freeCoffee = ( ( loyaltyMember = true ) && ( purchased5Coffees == true ) ) || ( ( haveCoupon == true ) && ( couponNotExpired == true ) )

	// This will not be isWeekend is true if day is either Saturday or Sunday
	SATURDAY := 6
	SUNDAY := 7
	day := 6
	// This statement will cause unhappiness in Go
	isWeekend := (day == SATURDAY || SUNDAY)

	// This is really what happens:
	isWeekend = (day == SATURDAY)
	isWeekend = isWeekend || SUNDAY
	// SUNDAY is non zero, so is always true.

	// This is what you should do
	isWeekend := (day == SATURDAY) || (day == SUNDAY)

	fmt.Printf("Is this the weekend %t\n", isWeekend)
}
