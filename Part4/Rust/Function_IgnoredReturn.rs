use std::io;

fn main()
{
    let mut user_input = String::new();
    let entered_integer: i32;
    
    println!( "Type 1 and press enter." );
    io::stdin().read_line( &mut user_input ).expect( "Failed to read line" );
    
    // Function called but return value ignored
    user_input.trim().parse::<i32>();
    
    println!( "The user entered the integer {}", entered_integer );
}
