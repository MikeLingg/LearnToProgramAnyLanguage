// Example with three default parameters
// Rust does not support default parameters, this shows limitation
fn three_default_parameters ( first_parameter_par: i32, second_parameter_par: i32, third_parameter_par: i32 )
{
    println! ( "Parameters: {} {} {}", first_parameter_par, second_parameter_par, third_parameter_par );
}

fn main ()
{
    three_default_parameters ( 20, 25, 30 );
    // Rust does not support default parameters, must provide all arguments
    three_default_parameters ( 5, 10, 15 );
}
