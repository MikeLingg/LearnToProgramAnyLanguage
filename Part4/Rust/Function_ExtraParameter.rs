use std::io;

fn main()
{
    let extra_input = 5;
    let mut user_input = String::new();
    
    println!( "Type 1 and press enter." );
    io::stdin().read_line( &mut user_input ).expect( "Failed to read line" );
    
    // Function only takes 0 parameters (besides self), but we're passing 1
    let entered_integer: i32 = user_input.trim().parse( extra_input ).unwrap();
    
    println!( "The user entered the integer {}", entered_integer );
}
