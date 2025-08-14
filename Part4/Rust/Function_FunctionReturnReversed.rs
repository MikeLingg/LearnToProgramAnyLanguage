use std::io;

fn main()
{
    let mut user_input = String::new();
    
    println!( "Type 1 and press enter." );
    io::stdin().read_line( &mut user_input ).expect( "Failed to read line" );
    
    // Trying to assign to method name instead of calling method
    user_input.trim().parse = entered_integer::<i32>( &user_input ).unwrap();
    
    println!( "The user entered the integer {}", entered_integer );
}
