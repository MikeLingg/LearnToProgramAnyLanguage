# Most programs allow this operation, but where is the result? We can't use it!
5 + 3

# Simple assignment of a constant and a variable to another variable:
a = 1
b = a
print( f"Our two variables {a} and {b}" )

# Assignment errors.
# c not assigned any value
# This whole block leads to runtime errors in python
a = c
c = c + 1
1 = a
print( f"Our incorrectly assigned variables {a} and {c}" )

# Now let's look at assigning the result of basic operations
# Assign the result of a mathematical operation
value1 = 5
value2 = 6
sumValue = value1 + value2
print( f"First value ( {value1} ) plus second value ( {value2} ) is {sumValue}" )

# Assign the result of a comparison operation
value3 = 7
value4 = 8
comparisonValue = ( value3 >= value4 )
print( f"First value ( {value3} ) greater than or equal to second value ( {value4} ) is {comparisonValue}" )

# Assign the result of a logical operation
value5 = True
value6 = False
logicalValue = value5 or value6
print( f"First value ( {value5} ) ORed with second value ( {value6} ) is {logicalValue}" )

# Storing a basic complex operation, multiple additions
sumValue = 1 + 2 + 3
print( f"The sum of 1 + 2 + 3 is {sumValue}" )

# Complex logical operation with differing operation priorities
loyaltyMember = False
purchased5Coffees = True
haveCoupon = True
couponNotExpired = True
freeCoffee = loyaltyMember and purchased5Coffees or haveCoupon and couponNotExpired
print( f"Customer gets a free coffee {freeCoffee}" )

# The code above is basically the same as the following, except the extra variables
# Sometimes breaking apart complex statements can help to self document
loyaltyAchieved = loyaltyMember and purchased5Coffees
haveValidCoupon = haveCoupon and couponNotExpired
freeCoffee = loyaltyAchieved and haveValidCoupon
print( f"Customer gets a free coffee {freeCoffee}" )

# This code shows how a value will be computed incorrectly 
# with default operator precedence
itemPrice = 99.99
itemShipping = 9.99
purchaseQuantity = 5
totalCost = itemPrice + itemShipping * purchaseQuantity
print( f"Total item cost is {totalCost:.2f}" )

# We can correct this with parenthesis which force operations to complete first
totalCost = ( itemPrice + itemShipping ) * purchaseQuantity
print( f"Total item cost is {totalCost:.2f}" )

# Parenthesis can be used to clarify precedence without 
# having to know what the actual operator precedence is.
freeCoffee = ( ( loyaltyMember == True ) and ( purchased5Coffees == True ) ) or ( ( haveCoupon == True ) and ( couponNotExpired == True ) )

# Some common errors, I can't really write these as a structured 
# language program, so these are more pseudo code examples.
# Do you want to assign 5 to a and b, or do you want to check if b is equal to 5?
a = b = 5
# vs
a = ( b == 5 )

# Spot the incorrect operator, python does not allow this and will throw a runtime error.
freeCoffee = ( ( loyaltyMember = True ) and ( purchased5Coffees == True ) ) or ( ( haveCoupon == True ) and ( couponNotExpired == True ) )

# This will not be isWeekend is true if day is either Saturday or Sunday
SATURDAY = 6
SUNDAY = 7
day = 6
isWeekend = ( day == SATURDAY or SUNDAY )

# This is really what happens:
isWeekend = ( day == SATURDAY )
isWeekend = isWeekend or SUNDAY
# SUNDAY is non zero, so is always true.

# This is what you should do
isWeekend = ( day == SATURDAY ) or ( day == SUNDAY )
