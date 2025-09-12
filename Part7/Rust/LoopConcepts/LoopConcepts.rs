use std::io;

fn main()
{
    // Basic loop.
    println! ( "\nSimple 100 iteration loop." );
    for loop_index in 0..100
    {
        println! ( "{}", loop_index );
    }
    println! ( "After Loop" );

    // Palindrome checker
    // It is a palindrome until we prove it is not.
    println! ( "\nPalindrome checker." );
    let possible_palindrome = "step on no pets";
    let mut is_palindrome = true;

    let half_length = possible_palindrome.len() / 2;

    for palindrome_index in 0..half_length
    {
        if possible_palindrome.chars().nth ( palindrome_index ) != 
           possible_palindrome.chars().nth ( possible_palindrome.len() - 1 - palindrome_index )
        {
            is_palindrome = false;
            break;
        }
    }

    if is_palindrome == true
    {
        println! ( "{} is a palindrome.", possible_palindrome );
    }
    else
    {
        println! ( "{} is NOT a palindrome.", possible_palindrome );
    }

    // Rust cannot have two loop variables in a for loop, 
    // at least without more complex constructs we will not look at here.
    // Must compute other variables from the primary loop variable, or use a while loop.

    // Two loops continuing a loop variable
    // Rust can do this, but a little differently
    println! ( "\nTwo loops continuing a loop variable" );
    let mut loop_index = 0;
    for i in 0..15
    {
        println! ( "First Loop: {}", i );
        loop_index = i;
    }
    for i in ( loop_index + 1 )..55
    {
        println! ( "Second Loop: {}", i );
    }

    // Rust can NOT perform a for loop with a floating point loop variable

    // Basic range loop
    println! ( "\nLooping over a range of values" );
    for loop_index in 0..100
    {
        println! ( "Loop Index: {}", loop_index );
    }
    println! ( "After Loop" );

    // Range variants
    println! ( "\nDifferent methods of looping over a range." );
    println! ( "\nStart at different index\n" );
    for loop_index in 1..11
    {
        println! ( "Loop Index: {}", loop_index );
    }
    println! ( "After Loop" );

    println! ( "\nDifferent loop increment\n" );
    for loop_index in ( 5..12 ).step_by ( 2 )
    {
        println! ( "Loop Index: {}", loop_index );
    }
    println! ( "After Loop" );

    println! ( "\nReverse increment\n" );
    for loop_index in ( 2..10 ).rev()
    {
        println! ( "Loop Index: {}", loop_index );
    }
    println! ( "After Loop" );

    // Looping over a range of array indices.
    // Good, range provided a number
    println! ( "Looping over a range of indices in an array" );
    let my_list = [1, 2, 3, 4];
    for loop_index in 0..my_list.len()
    {
        println! ( "Loop Index: {}", loop_index );
    }
    println! ( "After Loop" );

    // Bad, my_list is not a number - This will not compile
    println! ( "\nAttempting bad range usage - my_list is not a number" );
    //for loop_index in 0..my_list
    //{
    //    println! ( "Loop Index: {}", loop_index );
    //}
    //println! ( "After Loop" );

    // Initialize all entries in a large array to 0
    println! ( "\nInitialize values in an array" );
    let array_samples = 10000;
    let mut int_array = vec![0; array_samples];

    // This is kind of pointless in Rust as the above code creates a vector
    // of defined size with all values set to 0. The above statement is much faster 
    // than this loop.
    for array_index in 0..array_samples
    {
        int_array[array_index] = 0;
    }

    // Initialize all entries in a large array to index * 2 + 1
    for array_index in 0..array_samples
    {
        int_array[array_index] = array_index * 2 + 1;
    }

    // Example with string validation.
    // Loop through a string and ensure it contains a valid number.
    // Scientific notation is not considered, a single period is valid, no non numeric characters are valid
    println! ( "\nVerify string contains valid number." );
    let my_string = "123.45";
    let mut period_count = 0;
    let mut valid_number = true;
    for array_index in 0..my_string.len()
    {
        let char = my_string.chars().nth ( array_index ).unwrap();
        if char == '.'
        {
            period_count = period_count + 1;
            if period_count > 1
            {
                valid_number = false;
                break;
            }
        }
        else if char < '0' || char > '9'
        {
            valid_number = false;
            break;
        }
    }

    println! ( "String is valid number: {}", valid_number );

    // Basic condition loop
    println! ( "\nSimple conditional loop" );
    let mut int_count = 1000000;
    while true
    {
        // Just print the hello once so the terminal isn't overwhelmed.
        if int_count == 1000000
        {
            println! ( "Hello" );
        }

        // Break added so this isn't an infinite loop
        int_count = int_count - 1;
        if int_count <= 0
        {
            break;
        }
    }

    // While loop executing like our basic counting loop example
    println! ( "\nExample while loop performing basic counting loop" );
    let mut loop_index = 0;
    while loop_index < 100
    {
        println! ( "{}", loop_index );
        loop_index = loop_index + 1;
    }
    println! ( "After loop" );

    // More appropriate use of a while loop, when we don't know how many loops will be performed.
    println! ( "\nBetter while loop with length of loop unknown at start." );
    let data_array = [1, 2, 3, 4, 5, 6, 5, 7, 5, 8, 5, 9, 5, 10, 11, 12, 13, 14, 15];
    let data_array_size = data_array.len();
    let mut loop_index = 0;
    let mut found_count = 0;
    let mut five_elements_found = false;
    while ( loop_index < data_array_size ) && ( five_elements_found == false )
    {
        if data_array[loop_index] < 10
        {
            found_count = found_count + 1;
            if found_count >= 5
            {
                five_elements_found = true;
            }
        }
        loop_index = loop_index + 1;
    }

    // Using a loop to prompt user input until valid
    println! ( "\nConditional loop based on user input." );
    let mut exit_menu = false;
    while exit_menu == false
    {
        println! ( "Main Menu:" );
        println! ( "1. Start Game" );
        println! ( "2. Load Game" );
        println! ( "3. Show Help" );
        println! ( "4. Exit" );
        print! ( "Enter your choice: " );
        
        let mut input = String::new();
        io::stdin().read_line ( &mut input ).expect ( "Failed to read line" );
        let choice: i32 = input.trim().parse().unwrap_or ( 0 );

        // Rust has match expressions similar to switch
        match choice
        {
            1 => 
            {
                println! ( "Starting new game..." );
            },
            2 =>
            {
                println! ( "Loading saved game..." );
            },
            3 =>
            {
                println! ( "Help: Use the number keys to navigate the menu." )
            },
            4 =>
            {
                println! ( "Exiting program. Goodbye!" );
                exit_menu = true;
            },
            _ =>
            {
                println! ( "Invalid choice. Please select a valid option." );
            }
        }
    }

    // Rust has no do-while loop, so we can only show with a while loop or loop with break.
    println! ( "While equivalent example: Hint, entering 42 will exit this loop" );
    let mut number = 0;
    while number != 42
    {
        print! ( "Guess a number: " );
        let mut input = String::new();
        io::stdin().read_line ( &mut input ).expect ( "Failed to read line" );
        number = input.trim().parse().unwrap_or ( 0 );
    }

    // Nested loop for simple 2d array print
    println! ( "\nNested loop, 2d, example." );
    let row_count = 3;
    let column_count = 3;
    let two_d_list = [[1, 2, 3], [4, 5, 6], [7, 8, 9]];
    
    for row_index in 0..row_count
    {
        for column_index in 0..column_count
        {
            print! ( "{} ", two_d_list[row_index][column_index] );
        }
        println! ();
    }

    // Nested loop to print character permutations
    println! ( "\nNested permutations loop." );
    let letters = ['a', 'b', 'c', 'd', 'e'];
    let letters_size = letters.len();
    
    for first_char_index in 0..letters_size
    {
        for second_char_index in 0..letters_size
        {
            println! ( "{}{}", letters[first_char_index], letters[second_char_index] );
        }
    }

    // Nested loop to order a list of numbers
    println! ( "\nNested loop for sorting numbers." );
    let mut my_list2 = [6, 8, 9, 7, 4, 5, 0, 3, 1, 2];
    let my_list2_size = my_list2.len();
    for list_index in 0..my_list2_size
    {
        for swap_index in 0..( my_list2_size - list_index - 1 )
        {
            if my_list2[swap_index] > my_list2[swap_index + 1]
            {
                let swap_value = my_list2[swap_index];
                my_list2[swap_index] = my_list2[swap_index + 1];
                my_list2[swap_index + 1] = swap_value;
            }
        }
    }

    // Break within nested loop
    println! ( "\nUsing a break in a nested loop." );
    let two_d_list2 = [[1, 2], [3, 4]];
    let row_count = 2;
    let column_count = 2;
    
    for row_index in 0..row_count
    {
        for column_index in 0..column_count
        {
            println! ( "{}", two_d_list2[row_index][column_index] );
            break;
        }
    }

    // Off by 1 error - The first loop will exceed the array bounds
    // Rust will panic at runtime with bounds checking
    println! ( "\nOff by 1 loop error for array access." );
    let my_array = [1, 2, 3, 4];
    let array_size = my_array.len();
    println! ( "Loop through array off by 1." );
    //for loop_index in 0..=array_size
    //{
    //    let array_value = my_array[loop_index];
    //    println! ( "{} {}", loop_index, array_value );
    //}

    println! ( "Loop through array correct." );
    for loop_index in 0..array_size
    {
        println! ( "{}", my_array[loop_index] );
    }

    // Off by 1 when performing a partial list sort - Will exceed list bounds
    println! ( "\nSort list with off by 1 error." );
    let mut my_list3 = [4, 3, 2, 1];
    let my_list3_size = my_list3.len();
    //for loop_index in 0..my_list3_size
    //{
    //    if my_list3[loop_index] > my_list3[loop_index + 1]
    //    {
    //        let swap_value = my_list3[loop_index];
    //        my_list3[loop_index] = my_list3[loop_index + 1];
    //        my_list3[loop_index + 1] = swap_value;
    //    }
    //}

    // Off by 1 when performing a partial list sort - Will NOT exceed list bounds
    println! ( "\nSort list without off by 1 error." );
    for loop_index in 0..( my_list3_size - 1 )
    {
        if my_list3[loop_index] > my_list3[loop_index + 1]
        {
            let swap_value = my_list3[loop_index];
            my_list3[loop_index] = my_list3[loop_index + 1];
            my_list3[loop_index + 1] = swap_value;
        }
    }

    // Rust does not allow an empty loop block, will not provide example.

    // Redeclaring loop variable
    println! ( "\nRedeclared loop variable." );
    for loop_index in 0..10
    {
        for loop_index in 0..10
        {
            println! ( "Loop Index: {}", loop_index );
        }
    }

    // Loop condition untrue on entry
    println! ( "\nLoop condition untrue on entry." );
    let mut current_time = 5;
    let next_frame = 5;
    while current_time < next_frame
    {
        println! ( "Processing background tasks" );
        current_time = current_time + 1;
    }

    // Wrong reverse range loop - will not loop
    println! ( "\nReverse range loop failing to loop." );
    let array_size = 100;
    for loop_index in array_size..0
    {
        println! ( "Loop index: {}", loop_index );
    }

    // Wrong reverse range loop for an array
    for loop_index in ( 1..array_size + 1 ).rev()
    {
        println! ( "Loop index: {}", loop_index );
    }

    // Right reverse range loop
    println! ( "Reverse range loop correct." );
    for loop_index in ( 0..array_size ).rev()
    {
        println! ( "Loop index: {}", loop_index );
    }

    // Rust vector - removing elements while iterating through indices - will miss elements
    println! ( "\nVector removing elements while iterating through indices - will miss elements" );
    //let mut my_vec = vec![5, 2, 8, 2, 2, 6, 3, 5];
    //let original_len = my_vec.len();
    //for index in 0..original_len
    //{
    //    if my_vec[index] == 2
    //    {
    //        my_vec.remove ( index );
    //        println! ( "myVec at index {}: {:?}", index, my_vec );
    //    }
    //}

    // Rust vector - reverse for removal (correct approach)
    println! ( "\nVector removing items while iterating in reverse" );
    let mut my_vec2 = vec![5, 2, 8, 2, 2, 6, 3, 5];
    for index in ( 0..my_vec2.len() ).rev()
    {
        if my_vec2[index] == 2
        {
            my_vec2.remove ( index );
            println! ( "myVec at index {}: {:?}", index, my_vec2 );
        }
    }

    // Rust while loop for vector removals
    println! ( "\nRust while loop to remove vector items." );
    let mut my_vec3 = vec![5, 2, 8, 2, 2, 6, 3, 5];
    let mut loop_index = 0;
    while loop_index < my_vec3.len()
    {
        if my_vec3[loop_index] == 2
        {
            my_vec3.remove ( loop_index );
            println! ( "myVec at index {}: {:?}", loop_index, my_vec3 );
        }
        else
        {
            loop_index = loop_index + 1;
        }
    }

    // Rust integers can overflow but will panic in debug mode, wrap in release mode
    println! ( "Loop to very large number." );
    let large_number = usize::MAX / 1000000;
    let mut count = 0;
    for loop_index in 0..large_number
    {
        count = count + 1;
    }

}
