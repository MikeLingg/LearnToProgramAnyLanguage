-- Pizza example with input parameters
-- Prints the total price of a pizza based on size, toppings and delivery.
-- Size_Par: a string as either small, medium or large.
-- Topping_Count_Par: integer 0+
-- Delivery_Requested_Par: Delivery has been requested if true.
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Main is
   TOPPING_PRICE : constant Float := 1.50;
   DELIVERY_FEE : constant Float := 3.00;
   
   procedure Print_Pizza_Cost ( Size_Par : String; Topping_Count_Par : Integer; Delivery_Requested_Par : Boolean ) is
      Base_Price : Float := 0.0;
      Topping_Cost : Float;
      Delivery_Fee : Float := 0.0;
      Total_Price : Float;
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
      
      Put_Line ( "Pizza order:" );
      Put_Line ( ASCII.HT & "Base price:" & Float'Image ( Base_Price ) );
      Put_Line ( ASCII.HT & "Topping price:" & Float'Image ( Topping_Cost ) );
      Put_Line ( ASCII.HT & "Delivery price:" & Float'Image ( Delivery_Fee ) );
      Put_Line ( ASCII.HT & ASCII.HT & "Total price:" & Float'Image ( Total_Price ) );
   end Print_Pizza_Cost;
   
begin
   Print_Pizza_Cost ( "small", 2, True );
end Main;
