with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
   package Int_IO is new Ada.Text_IO.Integer_IO(Integer);
   package Float_IO is new Ada.Text_IO.Float_IO(Float);
   use Int_IO, Float_IO;

   -- Variable declarations
   a : Integer := 1;
   b : Integer;
   c : Integer;
   value1 : Integer := 5;
   value2 : Integer := 6;
   sumValue : Integer;
   value3 : Integer := 7;
   value4 : Integer := 8;
   comparisonValue : Boolean;
   StringOne   : String := "Hello One!";
   StringTwo   : String := "Hello one!";
   StringThree : String := "Hello One!";
   comparisonOne : Boolean;
   comparisonTwo : Boolean;
   value5 : Boolean := True;
   value6 : Boolean := False;
   logicalValue : Boolean;
   loyaltyMember : Boolean := False;
   purchased5Coffees : Boolean := True;
   haveCoupon : Boolean := True;
   couponNotExpired : Boolean := True;
   freeCoffee : Boolean;
   loyaltyAchieved : Boolean;
   haveValidCoupon : Boolean;
   itemPrice : Float := 99.99;
   itemShipping : Float := 9.99;
   purchaseQuantity : Integer := 5;
   totalCost : Float;
   SATURDAY : Integer := 6;
   SUNDAY : Integer := 7;
   day : Integer := 6;
   isWeekend : Boolean;

begin
   -- Most programs allow this operation, but where is the result? We can't use it!
   -- Ada compiler will not allow this.
   5 + 3;

   -- Simple assignment of a constant and a variable to another variable:
   b := a;
   Put( "Our two variables " );
   Put( a ); Put( " and " ); Put( b ); New_Line;

   -- Assignment errors - This code will likely go in its own separate program.
   a := c;
   c := c + 1;
   -- Ada compiler will not allow this.
   1 := a;
   Put( "Our incorrectly assigned variables " );
   Put( a ); Put( " and " ); Put( c ); New_Line;

   -- Now let's look at assigning the result of basic operations
   -- Assign the result of a mathematical operation
   sumValue := value1 + value2;
   Put( "First value ( " ); Put( value1 ); Put( " ) plus second value ( " );
   Put( value2 ); Put( " ) is " ); Put( sumValue ); New_Line;

   -- Assign the result of a comparison operation
   comparisonValue := ( value3 >= value4 );
   Put( "First value ( " ); Put( value3 ); Put( " ) greater than or equal to second value ( " );
   Put( value4 ); Put( " ) is " ); Put( Boolean'Image( comparisonValue ) ); New_Line;

   -- Ada uses = for string comparison
   comparisonOne := stringOne = stringTwo;
   comparisonTwo := stringOne = StringThree;

   Put_Line ( "Compare strings one and two: " & Boolean'Image ( comparisonOne ) );
   Put_Line ( "Compare strings one and three: " & Boolean'Image ( comparisonTwo ) );

   -- Assign the result of a logical operation
   logicalValue := value5 or value6;
   Put( "First value ( " ); Put( Boolean'Image( value5 ) ); Put( " ) ORed with second value ( " );
   Put( Boolean'Image( value6 ) ); Put( " ) is " ); Put( Boolean'Image( logicalValue ) ); New_Line;

   -- Storing a basic complex operation, multiple additions
   sumValue := 1 + 2 + 3;
   Put( "The sum of 1 + 2 + 3 is " ); Put( sumValue ); New_Line;

   -- Complex logical operation with differing operation priorities
   -- Ada requires parenthesis to clearly delineate logic precidence.
   freeCoffee := ( loyaltyMember and purchased5Coffees ) or ( haveCoupon and couponNotExpired );
   Put( "Customer gets a free coffee " ); Put( Boolean'Image( freeCoffee ) ); New_Line;

   -- The code above is basically the same as the following, except the extra variables
   -- Sometimes breaking apart complex statements can help to self document
   loyaltyAchieved := loyaltyMember and purchased5Coffees;
   haveValidCoupon := haveCoupon and couponNotExpired;
   freeCoffee := loyaltyAchieved and haveValidCoupon;
   Put( "Customer gets a free coffee " ); Put( Boolean'Image( freeCoffee ) ); New_Line;

   -- This code shows how a value will be computed incorrectly 
   -- with default operator precedence
   totalCost := itemPrice + itemShipping * Float( purchaseQuantity );
   Put( "Total item cost is " ); Put( totalCost, Exp=>0, Aft=>2 ); New_Line;

   -- We can correct this with parenthesis which force operations to complete first
   totalCost := ( itemPrice + itemShipping ) * Float( purchaseQuantity );
   Put( "Total item cost is " ); Put( totalCost, Exp=>0, Aft=>2 ); New_Line;

   -- Parenthesis can be used to clarify precedence without 
   -- having to know what the actual operator precedence is.
   -- In other languages this would be an example of how to use parenthesis, 
   -- in ada, the language requires this anyway.
   freeCoffee := ( ( loyaltyMember = True ) and ( purchased5Coffees = True ) ) or ( ( haveCoupon = True ) and ( couponNotExpired = True ) );

   -- Some common errors, I can't really write these as a structured 
   -- language program, so these are more pseudo code examples.
   -- Do you want to assign 5 to a and b, or do you want to check if b is equal to 5?
   -- Ada does not allow a = b = c, so this is less of a possible confusion.
   b := 5;
   a := b;
   -- vs
   a := ( b = 5 );

   -- Spot the incorrect operator.
   -- Ada's compiler will catch this, but not with the error you might expect.
   freeCoffee := ( ( loyaltyMember := True ) and ( purchased5Coffees = True ) ) or ( ( haveCoupon = True ) and ( couponNotExpired = True ) );

   -- This will not be isWeekend is true if day is either Saturday or Sunday
   isWeekend := ( day = SATURDAY or SUNDAY );

   -- This is really what happens:
   isWeekend := ( day = SATURDAY );
   isWeekend := isWeekend or SUNDAY;
   -- SUNDAY is non zero, so is always true.

   -- This is what you should do
   isWeekend := ( day = SATURDAY ) or ( day = SUNDAY );

end Main;
