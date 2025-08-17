using System;

class Program
{
    static void Main()
    {
        // Most programs allow this operation, but where is the result? We can't use it!
        // The C# compiler prevents this.
        5 + 3;

        // Simple assignment of a constant and a variable to another variable:
        int a = 1;
        int b = a;
        Console.WriteLine($"Our two variables {a} and {b}");

        // Assignment errors - This code will likely go in its own separate program.
        int c;
        a = c;
        c = c + 1;
        // The C# compiler prevents this.
        1 = a;
        Console.WriteLine($"Our incorrectly assigned variables {a} and {c}");

        // Now let's look at assigning the result of basic operations
        // Assign the result of a mathematical operation
        int value1 = 5;
        int value2 = 6;
        int sumValue = value1 + value2;
        Console.WriteLine($"First value ( {value1} ) plus second value ( {value2} ) is {sumValue}");

        // Assign the result of a comparison operation
        int value3 = 7;
        int value4 = 8;
        bool comparisonValue = (value3 >= value4);
        Console.WriteLine($"First value ( {value3} ) greater than or equal to second value ( {value4} ) is {comparisonValue}");

        // Assign the result of a logical operation
        bool value5 = true;
        bool value6 = false;
        bool logicalValue = value5 || value6;
        Console.WriteLine($"First value ( {value5} ) ORed with second value ( {value6} ) is {logicalValue}");

        // Storing a basic complex operation, multiple additions
        sumValue = 1 + 2 + 3;
        Console.WriteLine($"The sum of 1 + 2 + 3 is {sumValue}");

        // Complex logical operation with differing operation priorities
        bool loyaltyMember = false;
        bool purchased5Coffees = true;
        bool haveCoupon = true;
        bool couponNotExpired = true;
        bool freeCoffee = loyaltyMember && purchased5Coffees || haveCoupon && couponNotExpired;
        Console.WriteLine($"Customer gets a free coffee {freeCoffee}");

        // The code above is basically the same as the following, except the extra variables
        // Sometimes breaking apart complex statements can help to self document
        bool loyaltyAchieved = loyaltyMember && purchased5Coffees;
        bool haveValidCoupon = haveCoupon && couponNotExpired;
        freeCoffee = loyaltyAchieved && haveValidCoupon;
        Console.WriteLine($"Customer gets a free coffee {freeCoffee}");

        // This code shows how a value will be computed incorrectly 
        // with default operator precedence
        double itemPrice = 99.99;
        double itemShipping = 9.99;
        int purchaseQuantity = 5;
        double totalCost = itemPrice + itemShipping * purchaseQuantity;
        Console.WriteLine($"Total item cost is {totalCost:F2}");

        // We can correct this with parenthesis which force operations to complete first
        totalCost = (itemPrice + itemShipping) * purchaseQuantity;
        Console.WriteLine($"Total item cost is {totalCost:F2}");

        // Parenthesis can be used to clarify precedence without 
        // having to know what the actual operator precedence is.
        freeCoffee = ((loyaltyMember == true) && (purchased5Coffees == true)) || ((haveCoupon == true) && (couponNotExpired == true));

        // Some common errors, I can't really write these as a structured 
        // language program, so these are more pseudo code examples.
        // Do you want to assign 5 to a and b, or do you want to check if b is equal to 5?
        // C# does not allow a boolean result assigned to an int, so this is less confusing.
        a = b = 5;
        // vs
        bool aBool;
        aBool = (b == 5);

        // Spot the incorrect operator.
        freeCoffee = ((loyaltyMember = true) && (purchased5Coffees == true)) || ((haveCoupon == true) && (couponNotExpired == true));

        // This will not be isWeekend is true if day is either Saturday or Sunday
        int SATURDAY = 6;
        int SUNDAY = 7;
        int day = 6;
        // C# compiler prevents this code.
        bool isWeekend = (day == SATURDAY || SUNDAY);

        // This is what you should do
        isWeekend = (day == SATURDAY) || (day == SUNDAY);
    }
}
