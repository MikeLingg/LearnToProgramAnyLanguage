# Example of getPizzaCost returning the total cost of a pizza

# Returns the total price of a pizza based on size, toppings and delivery.
# Size_Par: a string as either small, medium or large.
# ToppingCount_Par: integer 0+
# DeliveryRequested_Par: Delivery has been requested if true.
# Return: Total pizza cost as a float.

TOPPING_PRICE = 1.50
DELIVERY_FEE = 3.00

def getPizzaCost ( Size_Par, ToppingCount_Par, DeliveryRequested_Par ):
    totalPrice = 0.0
    
    basePrice = 0.0
    
    if Size_Par == "small":
        basePrice = 12.99
    elif Size_Par == "medium":
        basePrice = 15.99
    else:
        basePrice = 18.99
    
    deliveryFee = 0
    toppingCost = ToppingCount_Par * TOPPING_PRICE
    if DeliveryRequested_Par == True:
        deliveryFee = DELIVERY_FEE
    
    totalPrice = basePrice + toppingCost + deliveryFee
    
    return totalPrice

def main():
    totalPrice = getPizzaCost ( Size_Par="small", ToppingCount_Par=2, DeliveryRequested_Par=True )
    print ( f"Total pizza price is: {totalPrice:.2f}" )

if __name__ == "__main__":
    main()
