use std::io;

fn main() {
    // Note: For this program to function as expected, the user will have to correctly enter the requested values.

    // Boolean strings of true/false cannot be converted to a bool variable without conditions,
    // so we will discuss how that works in the branches video coming soon.
    // Some languages will not even allow for reading values of 0 or 1 from the terminal as
    // booleans so we will identify which languages this fails with, and revisit how to make 
    // this work in the branches video.

    let mut user_input = String::new();

    // Note: Rust's bool parsing only accepts "true" and "false" exactly
    // Numbers like "1", "0", "11", "-1" all fail and return false via unwrap_or
    println!( "Type true and press enter." );
    io::stdin().read_line( &mut user_input ).expect( "Failed to read line" );
    let parse_result = user_input.trim().parse::<bool>();
    let entered_boolean: bool = parse_result.clone().unwrap_or( false );
    println!( "The user entered the boolean {} (error: {:?})", entered_boolean, parse_result.err() );

    user_input.clear();
    println!( "Type false and press enter." );
    io::stdin().read_line( &mut user_input ).expect( "Failed to read line" );
    let parse_result = user_input.trim().parse::<bool>();
    let entered_boolean: bool = parse_result.clone().unwrap_or( false );
    println!( "The user entered the boolean {} (error: {:?})", entered_boolean, parse_result.err() );

    user_input.clear();
    println!( "Type True and press enter." );
    io::stdin().read_line( &mut user_input ).expect( "Failed to read line" );
    let parse_result = user_input.trim().parse::<bool>();
    let entered_boolean: bool = parse_result.clone().unwrap_or( false );
    println!( "The user entered the boolean {} (error: {:?})", entered_boolean, parse_result.err() );

    user_input.clear();
    println!( "Type 0 and press enter." );
    io::stdin().read_line( &mut user_input ).expect( "Failed to read line" );
    let parse_result = user_input.trim().parse::<bool>();
    let entered_boolean: bool = parse_result.clone().unwrap_or( false );
    println!( "The user entered the boolean {} (error: {:?})", entered_boolean, parse_result.err() );

    user_input.clear();
    println!( "Type 1 and press enter." );
    io::stdin().read_line( &mut user_input ).expect( "Failed to read line" );
    let parse_result = user_input.trim().parse::<bool>();
    let entered_boolean: bool = parse_result.clone().unwrap_or( false );
    println!( "The user entered the boolean {} (error: {:?})", entered_boolean, parse_result.err() );

    user_input.clear();
    println!( "Type 11 and press enter." );
    io::stdin().read_line( &mut user_input ).expect( "Failed to read line" );
    let parse_result = user_input.trim().parse::<bool>();
    let entered_boolean: bool = parse_result.clone().unwrap_or( false );
    println!( "The user entered the boolean {} (error: {:?})", entered_boolean, parse_result.err() );

    user_input.clear();
    println!( "Type -1 and press enter." );
    io::stdin().read_line( &mut user_input ).expect( "Failed to read line" );
    let parse_result = user_input.trim().parse::<bool>();
    let entered_boolean: bool = parse_result.clone().unwrap_or( false );
    println!( "The user entered the boolean {} (error: {:?})", entered_boolean, parse_result.err() );

    user_input.clear();
    println!( "Type 55 and press enter." );
    io::stdin().read_line( &mut user_input ).expect( "Failed to read line" );
    let parse_result = user_input.trim().parse::<i32>();
    let entered_integer: i32 = parse_result.clone().unwrap_or( 0 );
    println!( "The user entered the integer {} (error: {:?})", entered_integer, parse_result.err() );

    user_input.clear();
    println!( "Type 55.5 and press enter." );
    io::stdin().read_line( &mut user_input ).expect( "Failed to read line" );
    let parse_result = user_input.trim().parse::<f32>();
    let entered_float: f32 = parse_result.clone().unwrap_or( 0.0 );
    println!( "The user entered the float {} (error: {:?})", entered_float, parse_result.err() );

    user_input.clear();
    println!( "Type Hello World! and press enter." );
    io::stdin().read_line( &mut user_input ).expect( "Failed to read line" );
    println!( "The user entered the string {}", user_input.trim() );

    user_input.clear();
    println!( "Type 123abc and press enter." );
    io::stdin().read_line( &mut user_input ).expect( "Failed to read line" );
    let parse_result = user_input.trim().parse::<i32>();
    let entered_integer: i32 = parse_result.clone().unwrap_or( 0 );
    println!( "The user entered the integer {} (error: {:?})", entered_integer, parse_result.err() );

    user_input.clear();
    println!( "Type 123.45 and press enter." );
    io::stdin().read_line( &mut user_input ).expect( "Failed to read line" );
    let parse_result = user_input.trim().parse::<i32>();
    let entered_integer: i32 = parse_result.clone().unwrap_or( 0 );
    println!( "The user entered the integer {} (error: {:?})", entered_integer, parse_result.err() );

    user_input.clear();
    println!( "Type abc123 and press enter." );
    io::stdin().read_line( &mut user_input ).expect( "Failed to read line" );
    let parse_result = user_input.trim().parse::<i32>();
    let entered_integer: i32 = parse_result.clone().unwrap_or( 0 );
    println!( "The user entered the integer {} (error: {:?})", entered_integer, parse_result.err() );

    user_input.clear();
    println!( "Type  567 and press enter." );
    io::stdin().read_line( &mut user_input ).expect( "Failed to read line" );
    let parse_result = user_input.trim().parse::<i32>();
    let entered_integer: i32 = parse_result.clone().unwrap_or( 0 );
    println!( "The user entered the integer {} (error: {:?})", entered_integer, parse_result.err() );

    user_input.clear();
    println!( "Type +567 and press enter." );
    io::stdin().read_line( &mut user_input ).expect( "Failed to read line" );
    let parse_result = user_input.trim().parse::<i32>();
    let entered_integer: i32 = parse_result.clone().unwrap_or( 0 );
    println!( "The user entered the integer {} (error: {:?})", entered_integer, parse_result.err() );

    user_input.clear();
    println!( "Type -567 and press enter." );
    io::stdin().read_line( &mut user_input ).expect( "Failed to read line" );
    let parse_result = user_input.trim().parse::<i32>();
    let entered_integer: i32 = parse_result.clone().unwrap_or( 0 );
    println!( "The user entered the integer {} (error: {:?})", entered_integer, parse_result.err() );

    // Rust has abs() method on numeric types
    println!( "Abs of -5 is {}", ( -5i32 ).abs() );
    println!( "Abs of -5.5 is {}", ( -5.5f32 ).abs() );
    println!( "Abs of a is {}", ( 'a' ).abs() );

    println!( "Pow of 2^5 is {}", ( 2.0f32 ).powf( 5.0 ) );
    println!( "Pow of 2.2^5.2 is {}", ( 2.2f32 ).powf( 5.2 ) );
    println!( "Pow of a^b is {}", ( 'a' ).powf( 'b' ) );

    // Note trig functions are almost always in radians, not degrees
    println!( "Sin of 90 is {}", ( 90.0f32 ).sin() );
    println!( "Sin of pi/2 is {}", ( std::f32::consts::PI / 2.0 ).sin() );

    println!( "Cos of 180 is {}", ( 180.0f32 ).cos() );
    println!( "Cos of pi is {}", std::f32::consts::PI.cos() );

    // Rounding type functions are very useful for explicit float to int conversions
    println!( "Floor of 5.5 is {}", ( 5.5f32 ).floor() );
    println!( "Floor of -5.5 is {}", ( -5.5f32 ).floor() );

    println!( "Ceil of 5.5 is {}", ( 5.5f32 ).ceil() );
    println!( "Ceil of -5.5 is {}", ( -5.5f32 ).ceil() );

    println!( "Round of 5.5 is {}", ( 5.5f32 ).round() );
    println!( "Round of -5.5 is {}", ( -5.5f32 ).round() );

    println!( "Trunc of 5.5 is {}", ( 5.5f32 ).trunc() );
    println!( "Trunc of -5.5 is {}", ( -5.5f32 ).trunc() );

    // This will NOT crash in Rust (parse() returns Result, unwrap_or provides default)
    user_input.clear();
    println!( "Type Hello World! and press enter." );
    io::stdin().read_line( &mut user_input ).expect( "Failed to read line" );
    let parse_result = user_input.trim().parse::<i32>();
    let entered_integer: i32 = parse_result.clone().unwrap_or( 0 );
    println!( "The user entered the integer {} (error: {:?})", entered_integer, parse_result.err() );

    // This will NOT crash in Rust (parse() returns Result, unwrap_or provides default)
    user_input.clear();
    println!( "Type abc123 and press enter." );
    io::stdin().read_line( &mut user_input ).expect( "Failed to read line" );
    let parse_result = user_input.trim().parse::<i32>();
    let entered_integer: i32 = parse_result.clone().unwrap_or( 0 );
    println!( "The user entered the integer {} (error: {:?})", entered_integer, parse_result.err() );

}