use std::io;

// Stubbed out functions
fn user_input() -> i32
{
    3
}

//fn some_function_result() -> bool {
//    true
//}

fn main()
{
    // Basic if statements with hard coded conditions
    println!( "Before Conditions" );
    if true
    {
        println!( "Branch Executed" );
    }

    if false
    {
        println!( "Branch Not Executed" );
    }

    println!( "After Conditions" );

    // If with hard coded assignment - Rust doesn't allow assignment as condition
    //let mut my_variable: bool;
    //if my_variable = true {
    //    println!( "Branch Executed" );
    //}

    // If with variable assignment (using function result) - Rust doesn't allow assignment as condition
    //if my_variable = some_function_result() {
    //    println!( "Branch Executed" );
    //}

    // More proper conditional branch
    let temperature = 64;

    let mut heater_on = false;
    // If I put parenthesis around the comparison, this will not compile.
    let temperature_cold = temperature < 70;
    if temperature_cold == true
    {
        heater_on = true;
    }

    println!( "Heater is on: {}", heater_on );

    // Alternate code to evaluate a conditional branch
    heater_on = false;
    if temperature < 70
    {
        heater_on = true;
    }

    println!( "Heater is on: {}", heater_on );

    // Using a code block that executes if a condition is true.
    let budget = 5000.00;
    let mut buffer = 500.00;
    let mut estimated_cost = 4750.00;

    let mut budget_overrun = false;
    let overrun_amount: f64;

    if ( estimated_cost + buffer ) > budget
    {
        overrun_amount = ( estimated_cost + buffer ) - budget;
        println!( "Overrun Amount: {:.2}", overrun_amount );
        buffer = 50.0;
        estimated_cost = budget - buffer;
        budget_overrun = true;
    }

    println!( "Budget overrun occurred: {} {}", budget_overrun, estimated_cost );

    // Variable scope in code blocks.
    // Many languages will fail trying to access innerVariable outside of the block.
    // In most languages I can even take the if True line out and just have the code block.
    /*
    let outer_variable = 10;
    if true ) {
        let inner_variable = 20;
    }

    println!( "Variables: {} : {}", outer_variable, inner_variable );
    */

    // Variable scope: Shadowed variables
    let variable = 10;
    if true
    {
        let variable = 20; // This shadows the outer variable
        println!( "Variable inside: {}", variable );
    }

    println!( "Variable outside: {}", variable );

    // Error of line being outside of if block, note the lack of begin/end block.
    // Different languages handle this differently.
    // Rust requires braces for if statements
    //if false
    //    println!( "Statement One" );
    //    println!( "Statement Two" );

    if false
    {
        println!( "Statement Three" );
        println!( "Statement Four" );
    }

    // Rust prefers no parenthesis and does not like brackets on the same indent.
    // I feel like this should not be language enforced, but meh.
    // Good in Rust
    if true
    {
        println!( "Good branch!" );
    }

    // Bad in Rust
    //if ( true ) {
    //    println!( "Bad branch!" );
    //}

    // Bad in Rust
    if true
    {
        println!( "Bad branch!" );
    }

    // If disconnected from the block, note the semicolon.
    // This behavior is going to vary from language to language
    // Rust prevents this pattern - it requires proper if statement syntax
    /*
    if false ); {
        println!( "Hello" );
    }
    */

    // Multiple separate if statements with overlapping conditions
    let score = 85;

    if score >= 90 && score <= 100
    {
        println!( "You got an A" );
    }
    if score >= 80 && score < 90
    {
        println!( "You got a B" );
    }
    if score >= 70 && score < 80
    {
        println!( "You got a C" );
    }
    if score >= 60 && score < 70
    {
        println!( "You got a D" );
    }
    if score < 60
    {
        println!( "You failed the test" );
    }

    // Else if example
    if score >= 90
    {
        println!( "You got an A" );
    }
    else if score >= 80
    {
        println!( "You got a B" );
    }
    else if score >= 70
    {
        println!( "You got a C" );
    }
    else if score >= 60
    {
        println!( "You got a D" );
    }
    else if score < 60
    {
        println!( "You failed the test" );
    }

    // Forgetting the else on an else if
    if score >= 90
    {
        println!( "You got an A" );
    }
    if score >= 80
    {
        println!( "You got a B" );
    }
    else if score >= 70
    {
        println!( "You got a C" );
    }

    // Else if without if, this will not compile in most languages.
    /*
    } else if score >= 90 ) {
        println!( "You got an A" );
    */

    // Adding an else to our if/else if
    if score >= 90
    {
        println!( "You got an A" );
    }
    else if score >= 80
    {
        println!( "You got a B" );
    }
    else if score >= 70
    {
        println!( "You got a C" );
    }
    else if score >= 60
    {
        println!( "You got a D" );
    }
    else
    {
        println!( "You failed the test" );
    }

    // Unreachable else, programming languages may not warn about this
    let age = 125;

    if ( age > 0 ) || ( age < 100 )
    {
        println!( "Valid age" );
    }
    else
    {
        println!( "Invalid age" );
    }

    // Else not following If or Else If, very uncompilable.
    /*
    } else {
        println!( "Hello from else!" );
    } else if true {
        println!( "Hello from else if!" );
    if true {
        println!( "Hello from if!" );
    */

    // Example of a complex condition that could be made a nested if
    let is_logged_in = true;
    let role = "admin";
    let method = "POST";
    let is_banned = false;
    let resource_is_available = true;

    if ( is_logged_in == true ) && ( role == "admin" ) && ( method == "POST" ) && ( is_banned == false ) && ( resource_is_available == true )
    {
        println!( "Access granted" );
    }

    // Breaking the complex condition into a nested if
    if is_logged_in == true
    {
        if role == "admin"
        {
            if method == "POST"
            {
                if is_banned == false
                {
                    if resource_is_available == true
                    {
                        println!( "Access granted" );
                    }
                    else
                    {
                        println!( "Resource Unavailable" );
                    }
                }
                else
                {
                    println!( "User is Banned" );
                }
            }
            else
            {
                println!( "Wrong Method" );
            }
        }
        else
        {
            println!( "Wrong User Level" );
        }
    }
    else
    {
        println!( "Please Log In" );
    }

    // Dangling Else - How this is handled will differ in different languages
    // Rust prevents this pattern - it requires braces for all if statements
    let user_exists = true;
    let password_valid = true;

    //if user_exists == true
    //    if password_valid == true
    //        println!( "Access granted" );
    //else
    //    println!( "Retry user name and password" );

    // No dangling else with blocks explicitly defined
    if user_exists == true
    {
        if password_valid == true
        {
            println!( "Access granted" );
        }
        else
        {
            println!( "Retry password" );
        }
    }

    // Basic switch statement (using match in Rust)
    let switch_variable = 2;

    match switch_variable
    {
        1 => println!( "Variable is 1" ),
        2 => println!( "Variable is 2" ),
        _ => println!( "Variable is unexpected value!" ),
    }

    // Switch on user input
    println!( "Main Menu:" );
    println!( "1. Start Game" );
    println!( "2. Load Game" );
    println!( "3. Show Help" );
    println!( "4. Exit" );
    print!( "Enter your choice: " );

    let choice = user_input();

    match choice
    {
        1 => println!( "Starting new game..." ),
        2 => println!( "Loading saved game..." ),
        3 => println!( "Help: Use the number keys to navigate the menu." ),
        4 => println!( "Exiting program. Goodbye!" ),
        _ => println!( "Invalid choice. Please select a valid option." ),
    }

    // Divide by zero defensive condition
    let time = 0;
    let distance = 100;
    let mut speed = 0;
    if time != 0
    {
        speed = distance / time;
    }
    println!( "Speed: {}", speed );

    // Handling both valid and invalid user inputs converted to booleans
    println!( "Enter a number:" );
    let mut input_string = String::new();
    io::stdin().read_line( &mut input_string ).expect( "Failed to read line" );
    let input_string = input_string.trim();
    
    let read_int = input_string.parse::<i32>();
    if read_int.is_ok()
    {
        println!( "User entered a valid number" );
    }
    else
    {
        println!( "Invalid number entered." );
    }

    // Method 1 of parsing an input string to a boolean
    println!( "Enter a boolean:" );
    let mut input_string = String::new();
    io::stdin().read_line( &mut input_string ).expect( "Failed to read line" );
    let input_string = input_string.trim();
    
    let mut read_valid_bool = false;
    let mut read_bool = false;

    let read_int = input_string.parse::<i32>();
    if read_int.is_ok()
    {
        let int_value = read_int.unwrap();
        read_valid_bool = true;
        if int_value == 0
        {
            read_bool = false;
        }
        else
        {
            read_bool = true;
        }
    }
    else
    {
        if input_string.to_lowercase() == "false"
        {
            read_bool = false;
            read_valid_bool = true;
        }
        else if input_string.to_lowercase() == "true"
        {
            read_bool = true;
            read_valid_bool = true;
        }
        else
        {
            println!( "Invalid boolean entered" );
        }
    }

    println!( "Read Bool {}", read_bool );
    if read_valid_bool == true
    {
        println!( "Entered boolean is valid" );
    }

    // Alternate method of parsing a string to boolean using guard clauses instead of nested conditions
    println!( "Enter a boolean:" );
    let mut input_string = String::new();
    io::stdin().read_line( &mut input_string ).expect( "Failed to read line" );
    let input_string = input_string.trim();
    
    let mut read_valid_bool = false;
    let mut read_bool = false;

    let read_int = input_string.parse::<i32>();
    if read_int.is_ok()
    {
        let int_value = read_int.unwrap();
        read_valid_bool = true;
        if int_value == 0
        {
            read_bool = false;
        }
        else
        {
            read_bool = true;
        }
    }

    if read_valid_bool == false
    {
        if input_string.to_lowercase() == "false"
        {
            read_bool = false;
            read_valid_bool = true;
        }
        else if input_string.to_lowercase() == "true"
        {
            read_bool = true;
            read_valid_bool = true;
        }
    }

    println!( "Read Bool {}", read_bool );
    if read_valid_bool == true
    {
        println!( "Valid boolean entered" );
    }
    else
    {
        println!( "Invalid boolean entered" );
    }

    // Compare two strings, only up to the length of the shortest string
    let false_string = "false";
    
    let input_matches_false = input_string.starts_with( false_string );

    println!( "False Entered {}", input_matches_false );

    // Make sure both strings have the appropriate length
    let false_string = "false";
    let mut sub_string_length = false_string.len();

    if sub_string_length > input_string.len()
    {
        sub_string_length = input_string.len();
    }

    let input_matches_false = &input_string[..sub_string_length] == false_string;

    println!( "False Entered {}", input_matches_false );

    // Float comparison with both positive and negative differences
    let first_float = 0.1;
    let second_float = 0.2;
    let sum = first_float + second_float;
    let third_float = 0.3;

    let tolerance = 0.000001;

    let difference = sum - third_float;

    if ( difference > -tolerance ) && ( difference < tolerance )
    {
        println!( "First float plus second float is equal to third float." );
    }
    else
    {
        println!( "First float plus second float is NOT equal to third float." );
    }

    // Float comparison with condition ensuring positive difference
    let first_float = 0.1;
    let second_float = 0.2;
    let sum = first_float + second_float;
    let third_float = 0.3;

    let tolerance = 0.000001;

    let mut difference = sum - third_float;
    if difference < 0.0
    {
        difference = -difference;
    }

    if difference < tolerance
    {
        println!( "First float plus second float is equal to third float." );
    }
    else
    {
        println!( "First float plus second float is NOT equal to third float." );
    }

    // Float comparison using abs to ensure positive difference
    // Note a let without explicit float type is problematic in Rust
    let first_float: f64 = 0.1;
    let second_float: f64 = 0.2;
    let sum = first_float + second_float;
    let third_float: f64 = 0.3;

    let tolerance = 0.000001;

    let difference = ( sum - third_float ).abs();

    if difference < tolerance
    {
        println!( "First float plus second float is equal to third float." );
    }
    else
    {
        println!( "First float plus second float is NOT equal to third float." );
    }
}
