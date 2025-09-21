-- Print pizza cost with named parameters, in languages that allow this
with Ada.Text_IO; use Ada.Text_IO;

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
   
   Next_Pizza_Size : String := "medium";
   Next_Topping_Count : Integer := 1;
   Next_Pizza_Is_Delivery : Boolean := False;
begin
   Print_Pizza_Cost ( Size_Par => "small", Topping_Count_Par => 2, Delivery_Requested_Par => True );
   Print_Pizza_Cost ( Size_Par => Next_Pizza_Size, Topping_Count_Par => Next_Topping_Count, Delivery_Requested_Par => Next_Pizza_Is_Delivery );
end Main;
