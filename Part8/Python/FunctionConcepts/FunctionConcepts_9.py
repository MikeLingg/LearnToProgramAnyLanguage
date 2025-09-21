# Alternate implementation of getPizzaCost

# Returns the total price of a pizza based on size, toppings and delivery.
# Size_Par: a string as either small, medium or large.
# ToppingCount_Par: integer 0+
# DeliveryRequested_Par: Delivery has been requested if true.
# Return: Total pizza cost as a float.

TOPPING_PRICE = 1.50
DELIVERY_FEE = 3.00
SIZE_COST = 3.00

def getPizzaCost ( Size_Par, ToppingCount_Par, DeliveryRequested_Par ):
    totalPrice = 0.0
    
    basePrice = 12.99
    
    if Size_Par == "medium":
        basePrice = basePrice + SIZE_COST
    if Size_Par == "large":
        basePrice = basePrice + SIZE_COST
    
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
