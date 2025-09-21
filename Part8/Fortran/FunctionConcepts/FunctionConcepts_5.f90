! Print pizza cost with named parameters, in languages that allow this
program main
    real, parameter :: TOPPING_PRICE = 1.50
    real, parameter :: DELIVERY_FEE = 3.00
    
    character(len=6) :: Next_Pizza_Size = "medium"
    integer :: Next_Topping_Count = 1
    logical :: Next_Pizza_Is_Delivery = .false.
    
    call print_pizza_cost ( "small", 2, .true. )
    call print_pizza_cost ( Next_Pizza_Size, Next_Topping_Count, Next_Pizza_Is_Delivery )
    
contains
    subroutine print_pizza_cost ( Size_Par, Topping_Count_Par, Delivery_Requested_Par )
        character(len=*), intent(in) :: Size_Par
        integer, intent(in) :: Topping_Count_Par
        logical, intent(in) :: Delivery_Requested_Par
        
        real :: Base_Price = 0.0
        real :: Topping_Cost
        real :: Delivery_Fee = 0.0
        real :: Total_Price
        
        if ( Size_Par == "small" ) then
            Base_Price = 12.99
        else if ( Size_Par == "medium" ) then
            Base_Price = 15.99
        else
            Base_Price = 18.99
        end if
        
        Topping_Cost = real ( Topping_Count_Par ) * TOPPING_PRICE
        
        if ( Delivery_Requested_Par .eqv. .true. ) then
            Delivery_Fee = DELIVERY_FEE
        end if
        
        Total_Price = Base_Price + Topping_Cost + Delivery_Fee
        
        print *, "Pizza order:"
        print *, char(9), "Base price:", Base_Price
        print *, char(9), "Topping price:", Topping_Cost
        print *, char(9), "Delivery price:", Delivery_Fee
        print *, char(9), char(9), "Total price:", Total_Price
    end subroutine print_pizza_cost
end program main
