// Print pizza cost with named parameters, in languages that allow this
using System;

class Program
{
    const double TOPPING_PRICE = 1.50;
    const double DELIVERY_FEE = 3.00;

    static void PrintPizzaCost ( string Size_Par, int ToppingCount_Par, bool DeliveryRequested_Par )
    {
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
        
        double toppingCost = ToppingCount_Par * TOPPING_PRICE;
        
        double deliveryFee = 0;
        if ( DeliveryRequested_Par == true )
        {
            deliveryFee = DELIVERY_FEE;
        }
        
        double totalPrice = basePrice + toppingCost + deliveryFee;
        
        Console.WriteLine ( "Pizza order:" );
        Console.WriteLine ( "\tBase price: {0:F2}", basePrice );
        Console.WriteLine ( "\tTopping price: {0:F2}", toppingCost );
        Console.WriteLine ( "\tDelivery price: {0:F2}", deliveryFee );
        Console.WriteLine ( "\t\tTotal price: {0:F2}", totalPrice );
    }

    static void Main ()
    {
        PrintPizzaCost ( Size_Par: "small", ToppingCount_Par: 2, DeliveryRequested_Par: true );
        
        string nextPizzaSize = "medium";
        int nextToppingCount = 1;
        bool nextPizzaIsDelivery = false;
        PrintPizzaCost ( Size_Par: nextPizzaSize, ToppingCount_Par: nextToppingCount, DeliveryRequested_Par: nextPizzaIsDelivery );
    }
}
