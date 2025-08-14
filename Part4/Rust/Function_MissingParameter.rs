use std::io;

fn main()
{
    let mut user_input = String::new();
    
    println!( "Type 1 and press enter." );
    io::stdin().read_line( &mut user_input ).expect( "Failed to read line" );
    
    // Function requires type parameter, but we're not providing it
    let entered_integer = user_input.trim().parse().unwrap();
    
    println!( "The user entered the integer {}", entered_integer );
}
