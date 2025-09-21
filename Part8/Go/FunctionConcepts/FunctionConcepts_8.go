// Example of getPizzaCost returning the total cost of a pizza

// Returns the total price of a pizza based on size, toppings and delivery.
// Size_Par: a string as either small, medium or large.
// ToppingCount_Par: integer 0+
// DeliveryRequested_Par: Delivery has been requested if true.
// Return: Total pizza cost as a float64.
package main

import "fmt"

const TOPPING_PRICE = 1.50
const DELIVERY_FEE = 3.00

func getPizzaCost ( Size_Par string, ToppingCount_Par int, DeliveryRequested_Par bool ) float64 {
    var totalPrice float64 = 0.0
    
    var basePrice float64 = 0.0
    
    if Size_Par == "small" {
        basePrice = 12.99
    } else if Size_Par == "medium" {
        basePrice = 15.99
    } else {
        basePrice = 18.99
    }
    
    var deliveryFee float64 = 0
    toppingCost := float64 ( ToppingCount_Par ) * TOPPING_PRICE
    if DeliveryRequested_Par == true {
        deliveryFee = DELIVERY_FEE
    }
    
    totalPrice = basePrice + toppingCost + deliveryFee
    
    return totalPrice
}

func main() {
    totalPrice := getPizzaCost ( "small", 2, true )
    fmt.Printf ( "Total pizza price is: %.2f\n", totalPrice )
}
