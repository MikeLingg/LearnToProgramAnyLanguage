use std::io;

fn main()
{
    let mut user_input = String::new();
    
    println!( "Type 1 and press enter." );
    io::stdin().read_line( &mut user_input ).expect( "Failed to read line" );
    
    // Function name has wrong capitalization
    let entered_integer: i32 = user_input.trim().Parse::<i32>().unwrap();
    
    println!( "The user entered the integer {}", entered_integer );
}
