// This example will not compile as we cannot call a variable like a function
fn my_function ()
{
    println! ( "Called my_function" );
}

fn main ()
{
    let my_variable = 5;
    my_variable (); // Will cause compile error
}
