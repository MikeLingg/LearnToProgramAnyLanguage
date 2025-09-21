// Recursive function call to compute factorial
fn factorial ( factorial_number_par: i32 ) -> i32
{
    if factorial_number_par <= 1
    {
        return 1;
    }
    
    return factorial_number_par * factorial ( factorial_number_par - 1 );
}

fn main ()
{
    let factorial_result = factorial ( 10 );
    println! ( "Factorial of 10 is: {}", factorial_result );
}
