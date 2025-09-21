-- Example of Get_Pizza_Cost returning the total cost of a pizza

-- Returns the total price of a pizza based on size, toppings and delivery.
-- Size_Par: a string as either small, medium or large.
-- Topping_Count_Par: integer 0+
-- Delivery_Requested_Par: Delivery has been requested if true.
-- Return: Total pizza cost as a Float.
with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
   TOPPING_PRICE : constant Float := 1.50;
   DELIVERY_FEE : constant Float := 3.00;
   
   function Get_Pizza_Cost ( Size_Par : String; Topping_Count_Par : Integer; Delivery_Requested_Par : Boolean ) return Float is
      Total_Price : Float := 0.0;
      Base_Price : Float := 0.0;
      Delivery_Fee : Float := 0.0;
      Topping_Cost : Float;
   begin
      if Size_Par = "small" then
         Base_Price := 12.99;
      elsif Size_Par = "medium" then
         Base_Price := 15.99;
      else
         Base_Price := 18.99;
      end if;
      
      Topping_Cost := Float ( Topping_Count_Par ) * TOPPING_PRICE;
      
      if Delivery_Requested_Par = True then
         Delivery_Fee := DELIVERY_FEE;
      end if;
      
      Total_Price := Base_Price + Topping_Cost + Delivery_Fee;
      
      return Total_Price;
   end Get_Pizza_Cost;
   
   Total_Price : Float;
begin
   Total_Price := Get_Pizza_Cost ( Size_Par => "small", Topping_Count_Par => 2, Delivery_Requested_Par => True );
   Put_Line ( "Total pizza price is:" & Float'Image ( Total_Price ) );
end Main;
