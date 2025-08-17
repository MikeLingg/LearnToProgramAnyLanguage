fn main() {
    // Most programs allow this operation, but where is the result? We can't use it!
    5 + 3;

    // Simple assignment of a constant and a variable to another variable:
    let a = 1;
    let b = a;
    println!( "Our two variables {} and {}", a, b );

    // Assignment errors - This code will likely go in its own separate program.
    // Rust's compiler will not allow this whole block.
    let c;
    let a = c;
    c = c + 1;
    1 = a;
    println!( "Our incorrectly assigned variables {} and {}", a, c );

    // Now let's look at assigning the result of basic operations
    // Assign the result of a mathematical operation
    let value1 = 5;
    let value2 = 6;
    let sum_value = value1 + value2;
    println!( "First value ( {} ) plus second value ( {} ) is {}", value1, value2, sum_value );

    // Assign the result of a comparison operation
    let value3 = 7;
    let value4 = 8;
    let comparison_value = value3 >= value4;
    println!( "First value ( {} ) greater than or equal to second value ( {} ) is {}", value3, value4, comparison_value );

    // Assign the result of a logical operation
    let value5 = true;
    let value6 = false;
    let logical_value = value5 || value6;
    println!( "First value ( {} ) ORed with second value ( {} ) is {}", value5, value6, logical_value );

    // Storing a basic complex operation, multiple additions
    let sum_value = 1 + 2 + 3;
    println!( "The sum of 1 + 2 + 3 is {}", sum_value );

    // Complex logical operation with differing operation priorities
    let loyalty_member = false;
    let purchased5_coffees = true;
    let have_coupon = true;
    let coupon_not_expired = true;
    let free_coffee = loyalty_member && purchased5_coffees || have_coupon && coupon_not_expired;
    println!( "Customer gets a free coffee {}", free_coffee );

    // The code above is basically the same as the following, except the extra variables
    // Sometimes breaking apart complex statements can help to self document
    let loyalty_achieved = loyalty_member && purchased5_coffees;
    let have_valid_coupon = have_coupon && coupon_not_expired;
    let free_coffee = loyalty_achieved && have_valid_coupon;
    println!( "Customer gets a free coffee {}", free_coffee );

    // This code shows how a value will be computed incorrectly 
    // with default operator precedence
    let item_price = 99.99;
    let item_shipping = 9.99;
    let purchase_quantity = 5;
    let total_cost = item_price + item_shipping * purchase_quantity as f64;
    println!( "Total item cost is {:.2}", total_cost );

    // We can correct this with parenthesis which force operations to complete first
    let total_cost = (item_price + item_shipping) * purchase_quantity as f64;
    println!( "Total item cost is {:.2}", total_cost );

    // Parenthesis can be used to clarify precedence without 
    // having to know what the actual operator precedence is.
    let free_coffee = ((loyalty_member == true) && (purchased5_coffees == true)) || ((have_coupon == true) && (coupon_not_expired == true));

    // Some common errors, I can't really write these as a structured 
    // language program, so these are more pseudo code examples.
    // Do you want to assign 5 to a and b, or do you want to check if b is equal to 5?
    // Rust also does not allow a = b = 5, so this is less of a concern.
    let b = 5;
    let a = b;
    // vs
    // Rust throws a warning for these parenthesis, which I don't like as they clarify the statement.
    let aBool = (b == 5);

    // Spot the incorrect operator.
    // Rust's compiler catches this.
    free_coffee = ((loyalty_member = true) && (purchased5_coffees == true)) || ((have_coupon == true) && (coupon_not_expired == true));

    // This will not be isWeekend is true if day is either Saturday or Sunday
    let SATURDAY = 6;
    let SUNDAY = 7;
    let day = 6;
    // Rust will not compile with this, but not for the reason you might think.
    let is_weekend = (day == SATURDAY || SUNDAY);

    // This is what you should do
    let is_weekend = (day == SATURDAY) || (day == SUNDAY);
}
