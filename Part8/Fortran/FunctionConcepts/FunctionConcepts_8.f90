! Alternate implementation of get_pizza_cost

! Returns the total price of a pizza based on size, toppings and delivery.
! Size_Par: a string as either small, medium or large.
! Topping_Count_Par: integer 0+
! Delivery_Requested_Par: Delivery has been requested if true.
! Return: Total pizza cost as a real.
program main
    real, parameter :: TOPPING_PRICE = 1.50
    real, parameter :: DELIVERY_FEE = 3.00
    real, parameter :: SIZE_COST = 3.00
    
    real :: Total_Price
    
    Total_Price = get_pizza_cost ( "small", 2, .true. )
    print *, "Total pizza price is:", Total_Price
    
contains
    function get_pizza_cost ( Size_Par, Topping_Count_Par, Delivery_Requested_Par ) result ( Total_Price )
        character(len=*), intent(in) :: Size_Par
        integer, intent(in) :: Topping_Count_Par
        logical, intent(in) :: Delivery_Requested_Par
        real :: Total_Price
        
        real :: Base_Price = 12.99
        real :: Delivery_Fee = 0.0
        real :: Topping_Cost
        
        if ( Size_Par == "medium" ) then
            Base_Price = Base_Price + SIZE_COST
        end if
        
        if ( Size_Par == "large" ) then
            Base_Price = Base_Price + SIZE_COST
        end if
        
        Topping_Cost = real ( Topping_Count_Par ) * TOPPING_PRICE
        
        if ( Delivery_Requested_Par .eqv. .true. ) then
            Delivery_Fee = DELIVERY_FEE
        end if
        
        Total_Price = Base_Price + Topping_Cost + Delivery_Fee
    end function get_pizza_cost
end program main
