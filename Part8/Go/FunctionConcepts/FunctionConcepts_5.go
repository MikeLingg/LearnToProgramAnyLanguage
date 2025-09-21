// Print pizza cost with named parameters, in languages that allow this
package main

import "fmt"

const TOPPING_PRICE = 1.50
const DELIVERY_FEE = 3.00

func printPizzaCost ( Size_Par string, ToppingCount_Par int, DeliveryRequested_Par bool ) {
    var basePrice float64 = 0.0
    
    if Size_Par == "small" {
        basePrice = 12.99
    } else if Size_Par == "medium" {
        basePrice = 15.99
    } else {
        basePrice = 18.99
    }
    
    toppingCost := float64 ( ToppingCount_Par ) * TOPPING_PRICE
    
    var deliveryFee float64 = 0
    if DeliveryRequested_Par == true {
        deliveryFee = DELIVERY_FEE
    }
    
    totalPrice := basePrice + toppingCost + deliveryFee
    
    fmt.Println ( "Pizza order:" )
    fmt.Printf ( "\tBase price: %.2f\n", basePrice )
    fmt.Printf ( "\tTopping price: %.2f\n", toppingCost )
    fmt.Printf ( "\tDelivery price: %.2f\n", deliveryFee )
    fmt.Printf ( "\t\tTotal price: %.2f\n", totalPrice )
}

func main() {
    printPizzaCost ( "small", 2, true )
    
    nextPizzaSize := "medium"
    nextToppingCount := 1
    nextPizzaIsDelivery := false
    printPizzaCost ( nextPizzaSize, nextToppingCount, nextPizzaIsDelivery )
}
