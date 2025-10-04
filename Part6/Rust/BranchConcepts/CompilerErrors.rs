fn main()
{
    let score = 85;
    let value = 5;

    // ERROR 1: Missing braces around if body
    if score >= 90
        println!( "You got an A" );

    // ERROR 2: Using 'then' keyword
    if score >= 90 then
    {
        println!( "You got an A" );
    }

    // ERROR 3: Using 'elif' instead of 'else if'
    if score >= 90
    {
        println!( "A" );
    }
    elif score >= 80
    {
        println!( "B" );
    }

    // ERROR 4: Using 'elsif' instead of 'else if'
    if score >= 90
    {
        println!( "A" );
    }
    elsif score >= 80
    {
        println!( "B" );
    }

    // ERROR 5: Using 'elseif' (one word) instead of 'else if'
    if score >= 90
    {
        println!( "A" );
    }
    elseif score >= 80
    {
        println!( "B" );
    }

    // ERROR 6: Using 'switch' instead of 'match'
    switch value
    {
        1 => println!( "One" ),
        _ => println!( "Other" ),
    }

    // ERROR 7: Using 'case' instead of match arms
    match value
    {
        case 1:
            println!( "One" );
        case 2:
            println!( "Two" );
        default:
            println!( "Other" );
    }

    // ERROR 8: Using colon instead of => in match
    match value
    {
        1: println!( "One" ),
        _ : println!( "Other" ),
    }

    // ERROR 9: Missing => in match arm
    match value
    {
        1 println!( "One" ),
        _ println!( "Other" ),
    }

    // ERROR 10: Using 'default' instead of '_' in match
    match value
    {
        1 => println!( "One" ),
        default => println!( "Other" ),
    }

    // ERROR 11: Missing comma after match arm
    match value
    {
        1 => println!( "One" )
        2 => println!( "Two" ),
        _ => println!( "Other" ),
    }

    // ERROR 12: Duplicate match arms
    match value
    {
        1 => println!( "First one" ),
        1 => println!( "Second one" ),
        _ => println!( "Other" ),
    }

    // ERROR 13: Using 'break' in match (not needed/invalid)
    match value
    {
        1 => {
            println!( "One" );
            break;
        },
        _ => println!( "Other" ),
    }

    // ERROR 14: Match arm outside of match
    1 => println!( "One" ),

    // ERROR 15: else if without preceding if
    else if score >= 80
    {
        println!( "B" );
    }

    // ERROR 16: else without preceding if
    else
    {
        println!( "Failed" );
    }

    // ERROR 17: Missing match arms (non-exhaustive)
    match value
    {
        1 => println!( "One" ),
        2 => println!( "Two" ),
    }

    // ERROR 18: Semicolon after if condition
    if score >= 90;
    {
        println!( "A" );
    }
}
