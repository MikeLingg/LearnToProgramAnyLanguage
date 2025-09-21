// Example of getPizzaCost returning the total cost of a pizza

// Returns the total price of a pizza based on size, toppings and delivery.
// Size_Par: a string as either small, medium or large.
// ToppingCount_Par: integer 0+
// DeliveryRequested_Par: Delivery has been requested if true.
// Return: Total pizza cost as a float.
#include <iostream>
#include <cstdio>
#include <string>
using namespace std;

const double TOPPING_PRICE = 1.50;
const double DELIVERY_FEE = 3.00;

double getPizzaCost ( string Size_Par, int ToppingCount_Par, bool DeliveryRequested_Par )
{
    double totalPrice = 0.0;
    
    double basePrice = 0.0;
    
    if ( Size_Par == "small" )
    {
        basePrice = 12.99;
    }
    else if ( Size_Par == "medium" )
    {
        basePrice = 15.99;
    }
    else
    {
        basePrice = 18.99;
    }
    
    double deliveryFee = 0;
    double toppingCost = ToppingCount_Par * TOPPING_PRICE;
    if ( DeliveryRequested_Par == true )
    {
        deliveryFee = DELIVERY_FEE;
    }
    
    totalPrice = basePrice + toppingCost + deliveryFee;
    
    return totalPrice;
}

int main ()
{
    double totalPrice = getPizzaCost ( "small", 2, true );
    printf ( "Total pizza price is: %.2f\n", totalPrice );
    return 0;
}
