// Pizza example with input parameters
// Prints the total price of a pizza based on size, toppings and delivery.
// Size_Par: a string as either small, medium or large.
// ToppingCount_Par: integer 0+
// DeliveryRequested_Par: Delivery has been requested if true.
#include <stdio.h>
#include <string.h>
#include <stdbool.h>

const double TOPPING_PRICE = 1.50;
const double DELIVERY_FEE = 3.00;

void printPizzaCost ( char* Size_Par, int ToppingCount_Par, bool DeliveryRequested_Par )
{
    double basePrice = 0.0;
    
    if ( strcmp ( Size_Par, "small" ) == 0 )
    {
        basePrice = 12.99;
    }
    else if ( strcmp ( Size_Par, "medium" ) == 0 )
    {
        basePrice = 15.99;
    }
    else
    {
        basePrice = 18.99;
    }
    
    double toppingCost = ToppingCount_Par * TOPPING_PRICE;
    
    double deliveryFee = 0;
    if ( DeliveryRequested_Par == true )
    {
        deliveryFee = DELIVERY_FEE;
    }
    
    double totalPrice = basePrice + toppingCost + deliveryFee;
    
    printf ( "Pizza order:\n" );
    printf ( "\tBase price: %.2f\n", basePrice );
    printf ( "\tTopping price: %.2f\n", toppingCost );
    printf ( "\tDelivery price: %.2f\n", deliveryFee );
    printf ( "\t\tTotal price: %.2f\n", totalPrice );
}

int main ()
{
    printPizzaCost ( "small", 2, true );
    return 0;
}
