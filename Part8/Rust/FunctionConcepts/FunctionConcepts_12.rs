// Computing factorial with loop
fn factorial ( factorial_number_par: i32 ) -> i32
{
    let mut total_factorial = 1;
    
    for factorial_number in 1..=factorial_number_par
    {
        total_factorial = total_factorial * factorial_number;
    }
    
    return total_factorial;
}

fn main ()
{
    let factorial_result = factorial ( 10 );
    println! ( "Factorial of 10 is: {}", factorial_result );
}
