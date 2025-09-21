# Pizza example with input parameters
# Prints the total price of a pizza based on size, toppings and delivery.
# Size_Par: a string as either small, medium or large.
# ToppingCount_Par: integer 0+
# DeliveryRequested_Par: Delivery has been requested if true.

TOPPING_PRICE = 1.50
DELIVERY_FEE = 3.00

def printPizzaCost ( Size_Par, ToppingCount_Par, DeliveryRequested_Par ):
    basePrice = 0.0
    
    if Size_Par == "small":
        basePrice = 12.99
    elif Size_Par == "medium":
        basePrice = 15.99
    else:
        basePrice = 18.99
    
    toppingCost = ToppingCount_Par * TOPPING_PRICE
    
    deliveryFee = 0
    if DeliveryRequested_Par == True:
        deliveryFee = DELIVERY_FEE
    
    totalPrice = basePrice + toppingCost + deliveryFee
    
    print ( "Pizza order:" )
    print ( f"\tBase price: {basePrice:.2f}" )
    print ( f"\tTopping price: {toppingCost:.2f}" )
    print ( f"\tDelivery price: {deliveryFee:.2f}" )
    print ( f"\t\tTotal price: {totalPrice:.2f}" )

def main():
    printPizzaCost ( "small", 2, True )

if __name__ == "__main__":
    main()
