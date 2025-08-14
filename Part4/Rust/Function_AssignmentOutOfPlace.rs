use std::io;

fn main()
{
    let mut user_input = String::new();
    
    println!( "Type 1 and press enter." );
    io::stdin().read_line( &mut user_input ).expect( "Failed to read line" );
    
    // Assignment operator placed incorrectly in declaration
    let entered_integer parse = user_input.trim()::<i32>().unwrap();
    
    println!( "The user entered the integer {}", entered_integer );
}
