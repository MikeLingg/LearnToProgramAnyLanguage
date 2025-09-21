// Rust doesn't pass arguments to main, must use env::args.
use std::env;

fn main ()
{
    let args: Vec<String> = env::args ().collect ();
    println! ( "Number of arguments: {}", args.len () );
    println! ( "Arguments:" );
    for ( i, arg ) in args.iter ().enumerate ()
    {
        println! ( "\tArgument {}: {}", i, arg );
    }
}
