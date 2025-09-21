// Example of function overloading
// Rust does not support function overloading, this will not compile
fn my_function ( int_parameter_par: i32 )
{
    println! ( "Int version of my function called {}", int_parameter_par );
}

fn my_function ( double_parameter_par: f64 )
{
    println! ( "Double version of my function called {:.1}", double_parameter_par );
}

fn main ()
{
    my_function ( 5 );
    my_function ( 5.5 );
}
