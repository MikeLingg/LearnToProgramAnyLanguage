// Alternate implementation of get_pizza_cost

// Returns the total price of a pizza based on size, toppings and delivery.
// size_par: a string as either small, medium or large.
// topping_count_par: integer 0+
// delivery_requested_par: Delivery has been requested if true.
// Return: Total pizza cost as a f64.

const TOPPING_PRICE: f64 = 1.50;
const DELIVERY_FEE: f64 = 3.00;
const SIZE_COST: f64 = 3.00;

fn get_pizza_cost ( size_par: &str, topping_count_par: i32, delivery_requested_par: bool ) -> f64
{
    let mut total_price = 0.0;
    
    let mut base_price = 12.99;
    
    if size_par == "medium"
    {
        base_price = base_price + SIZE_COST;
    }
    if size_par == "large"
    {
        base_price = base_price + SIZE_COST;
    }
    
    let mut delivery_fee = 0.0;
    let topping_cost = topping_count_par as f64 * TOPPING_PRICE;
    if delivery_requested_par == true
    {
        delivery_fee = DELIVERY_FEE;
    }
    
    total_price = base_price + topping_cost + delivery_fee;
    
    return total_price;
}

fn main ()
{
    let total_price = get_pizza_cost ( "small", 2, true );
    println! ( "Total pizza price is: {:.2}", total_price );
}
