#include <stdio.h>
#include <stdbool.h>

int main()
{
    // Most programs allow this operation, but where is the result? We can't use it!
    5 + 3;
    
    // Simple assignment of a constant and a variable to another variable:
    int a = 1;
    int b = a;
    printf( "Our two variables %d and %d\n", a, b );
    
    // Assignment errors - This code will likely go in its own separate program.
    int c;
    a = c;
    c = c + 1;
    // Note: The next line will prevent compiling.
    1 = a;
    printf( "Our incorrectly assigned variables %d and %d\n", a, c );

    // Now let's look at assigning the result of basic operations
    // Assign the result of a mathematical operation
    int value1 = 5;
    int value2 = 6;
    int sumValue = value1 + value2;
    printf( "First value ( %d ) plus second value ( %d ) is %d\n", value1, value2, sumValue );
    
    // Assign the result of a comparison operation
    int value3 = 7;
    int value4 = 8;
    int comparisonValue = ( value3 >= value4 );
    printf( "First value ( %d ) greater than or equal to second value ( %d ) is %d\n", value3, value4, comparisonValue );
    
    // Assign the result of a logical operation
    int value5 = 1;
    int value6 = 0;
    int logicalValue = value5 || value6;
    printf( "First value ( %d ) ORed with second value ( %d ) is %d\n", value5, value6, logicalValue );
    
    // Storing a basic complex operation, multiple additions
    sumValue = 1 + 2 + 3;
    printf( "The sum of 1 + 2 + 3 is %d\n", sumValue );
    
    // Complex logical operation with differing operation priorities
    bool loyaltyMember = false;
    bool purchased5Coffees = true;
    bool haveCoupon = true;
    bool couponNotExpired = true;
    bool freeCoffee = loyaltyMember && purchased5Coffees || haveCoupon && couponNotExpired;
    printf( "Customer gets a free coffee %d\n", freeCoffee );
    
    // The code above is basically the same as the following, except the extra variables
    // Sometimes breaking apart complex statements can help to self document
    bool loyaltyAchieved = loyaltyMember && purchased5Coffees;
    bool haveValidCoupon = haveCoupon && couponNotExpired;
    freeCoffee = loyaltyAchieved && haveValidCoupon;
    printf( "Customer gets a free coffee %d\n", freeCoffee );
    
    // This code shows how a value will be computed incorrectly 
    // with default operator precedence
    float itemPrice = 99.99f;
    float itemShipping = 9.99f;
    int purchaseQuantity = 5;
    float totalCost = itemPrice + itemShipping * purchaseQuantity;
    printf( "Total item cost is %.2f\n", totalCost );
    
    // We can correct this with parenthesis which force operations to complete first
    totalCost = ( itemPrice + itemShipping ) * purchaseQuantity;
    printf( "Total item cost is %.2f\n", totalCost );
    
    // Parenthesis can be used to clarify precedence without 
    // having to know what the actual operator precedence is.
    freeCoffee = ( ( loyaltyMember == 1 ) && ( purchased5Coffees == 1 ) ) ||
                 ( ( haveCoupon == 1 ) && ( couponNotExpired == 1 ) );
    
    // Some common errors, I can't really write these as a structured 
    // language program, so these are more pseudo code examples.
    // Do you want to assign 5 to a and b, or do you want to check if b is equal to 5?
    a = b = 5;
    // vs
    a = ( b == 5 );
    
    // Spot the incorrect operator.
    freeCoffee = ( ( loyaltyMember = 1 ) && ( purchased5Coffees == 1 ) ) ||
                 ( ( haveCoupon == 1 ) && ( couponNotExpired == 1 ) );
    
    // This will not be isWeekend is true if day is either Saturday or Sunday
    int SATURDAY = 6;
    int SUNDAY = 7;
    int day = 6;
    int isWeekend = ( day == SATURDAY || SUNDAY );
    
    // This is really what happens:
    isWeekend = ( day == SATURDAY );
    isWeekend = isWeekend || SUNDAY;
    // SUNDAY is non zero, so is always true.
    
    // This is what you should do
    isWeekend = ( day == SATURDAY ) || ( day == SUNDAY );
    
    return 0;
}
