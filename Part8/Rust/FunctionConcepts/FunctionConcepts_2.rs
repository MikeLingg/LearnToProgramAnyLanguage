// An example of function variable scope
static mut global_variable_glob: i32 = 15;
static mut global_to_be_shadowed_glob: i32 = 5;

fn my_function ()
{
    let my_variable = 55;
    unsafe
    {
        global_variable_glob = 42;
    }
    let global_to_be_shadowed_glob = 15;
}

fn main ()
{
    unsafe
    {
        println! ( "Global variable: {}", global_variable_glob );
        println! ( "Global shadowed: {}", global_to_be_shadowed_glob );
    }
    my_function ();
    println! ( "Function variable: {}", my_variable );
    unsafe
    {
        println! ( "Global variable: {}", global_variable_glob );
        println! ( "Global shadowed: {}", global_to_be_shadowed_glob );
    }
}
