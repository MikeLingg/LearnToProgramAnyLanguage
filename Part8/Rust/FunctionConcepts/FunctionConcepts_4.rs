// Pizza example with input parameters
// Pizza example with input parameters
// Prints the total price of a pizza based on size, toppings and delivery.
// size_par: a string as either small, medium or large.
// topping_count_par: integer 0+
// delivery_requested_par: Delivery has been requested if true.

const TOPPING_PRICE: f64 = 1.50;
const DELIVERY_FEE: f64 = 3.00;

fn print_pizza_cost ( size_par: &str, topping_count_par: i32, delivery_requested_par: bool )
{
    let mut base_price = 0.0;
    
    if size_par == "small"
    {
        base_price = 12.99;
    }
    else if size_par == "medium"
    {
        base_price = 15.99;
    }
    else
    {
        base_price = 18.99;
    }
    
    let topping_cost = topping_count_par as f64 * TOPPING_PRICE;
    
    let mut delivery_fee = 0.0;
    if delivery_requested_par == true
    {
        delivery_fee = DELIVERY_FEE;
    }
    
    let total_price = base_price + topping_cost + delivery_fee;
    
    println! ( "Pizza order:" );
    println! ( "\tBase price: {:.2}", base_price );
    println! ( "\tTopping price: {:.2}", topping_cost );
    println! ( "\tDelivery price: {:.2}", delivery_fee );
    println! ( "\t\tTotal price: {:.2}", total_price );
}

fn main ()
{
    print_pizza_cost ( "small", 2, true );
}
