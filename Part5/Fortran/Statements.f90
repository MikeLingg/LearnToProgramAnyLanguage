program main
    implicit none
    
    ! Variable declarations
    integer :: a = 1
    integer :: b
    integer :: c
    integer :: value1 = 5
    integer :: value2 = 6
    integer :: sumValue
    integer :: value3 = 7
    integer :: value4 = 8
    logical :: comparisonValue
    character(len=10) :: stringOne = "Hello One!"
    character(len=10) :: stringTwo = "Hello one!"
    character(len=10) :: stringThree = "Hello One!"
    integer :: comparisonOne
    integer :: comparisonTwo
    logical :: value5 = .true.
    logical :: value6 = .false.
    logical :: logicalValue
    logical :: loyaltyMember = .false.
    logical :: purchased5Coffees = .true.
    logical :: haveCoupon = .true.
    logical :: couponNotExpired = .true.
    logical :: freeCoffee
    logical :: loyaltyAchieved
    logical :: haveValidCoupon
    logical :: aLogical
    real :: itemPrice = 99.99
    real :: itemShipping = 9.99
    integer :: purchaseQuantity = 5
    real :: totalCost
    integer, parameter :: SATURDAY = 6
    integer, parameter :: SUNDAY = 7
    integer :: day = 6
    logical :: isWeekend
    
    ! Most programs allow this operation, but where is the result? We can't use it!
    ! The fortran compiler will prevent this.
    5 + 3
    
    ! Simple assignment of a constant and a variable to another variable:
    b = a
    write( *, * ) 'Our two variables ', a, ' and ', b
    
    ! Assignment errors - This code will likely go in its own separate program.
    a = c
    c = c + 1
    ! The fortran compiler will prevent this.
    1 = a
    write( *, * ) 'Our incorrectly assigned variables ', a, ' and ', c
    
    ! Now let's look at assigning the result of basic operations
    ! Assign the result of a mathematical operation
    sumValue = value1 + value2
    write( *, * ) 'First value ( ', value1, ' ) plus second value ( ', value2, ' ) is ', sumValue
    
    ! Assign the result of a comparison operation
    comparisonValue = ( value3 >= value4 )
    write( *, * ) 'First value ( ', value3, ' ) greater than or equal to second value ( ', value4, ' ) is ', comparisonValue
    
    ! Fortran only uses == for string comparison
    comparisonOne = stringOne == stringTwo
    comparisonTwo = stringOne == stringThree

    print *, 'Compare strings one and two: ', comparisonOne
    print *, 'Compare strings one and three: ', comparisonTwo

    ! Assign the result of a logical operation
    logicalValue = value5 .or. value6
    write( *, * ) 'First value ( ', value5, ' ) ORed with second value ( ', value6, ' ) is ', logicalValue
    
    ! Storing a basic complex operation, multiple additions
    sumValue = 1 + 2 + 3
    write( *, * ) 'The sum of 1 + 2 + 3 is ', sumValue
    
    ! Complex logical operation with differing operation priorities
    freeCoffee = ( loyaltyMember .and. purchased5Coffees ) .or. ( haveCoupon .and. couponNotExpired )
    write( *, * ) 'Customer gets a free coffee ', freeCoffee
    
    ! The code above is basically the same as the following, except the extra variables
    ! Sometimes breaking apart complex statements can help to self document
    loyaltyAchieved = loyaltyMember .and. purchased5Coffees
    haveValidCoupon = haveCoupon .and. couponNotExpired
    freeCoffee = loyaltyAchieved .and. haveValidCoupon
    write( *, * ) 'Customer gets a free coffee ', freeCoffee
    
    ! This code shows how a value will be computed incorrectly 
    ! with default operator precedence
    totalCost = itemPrice + itemShipping * real( purchaseQuantity )
    write( *, '(A, F0.2)' ) 'Total item cost is ', totalCost
    
    ! We can correct this with parenthesis which force operations to complete first
    totalCost = ( itemPrice + itemShipping ) * real( purchaseQuantity )
    write( *, '(A, F0.2)' ) 'Total item cost is ', totalCost
    
    ! Parenthesis can be used to clarify precedence without 
    ! having to know what the actual operator precedence is.
    freeCoffee = ( ( loyaltyMember .eqv. .true. ) .and. ( purchased5Coffees .eqv. .true. ) ) .or. &
                 ( ( haveCoupon .eqv. .true. ) .and. ( couponNotExpired .eqv. .true. ) )
    
    ! Some common errors, I can't really write these as a structured 
    ! language program, so these are more pseudo code examples.
    ! Do you want to assign 5 to a and b, or do you want to check if b is equal to 5?
    a = b = 5
    ! vs
    aLogical = ( b == 5 )
    
    ! Spot the incorrect operator.
    ! Fortran's compiler catches this for you.
    freeCoffee = ( ( loyaltyMember = .true. ) .and. ( purchased5Coffees == .true. ) ) .or. &
                   ( ( haveCoupon == .true. ) .and. ( couponNotExpired == .true. ) )
    
    ! This will not be isWeekend is true if day is either Saturday or Sunday
    ! Fortran does not allow oring a logical with an integer, so less of a problem.
    isWeekend = ( day == SATURDAY .or. SUNDAY )
    
    ! This is what you should do
    isWeekend = ( day == SATURDAY ) .or. ( day == SUNDAY )
    
end program main
