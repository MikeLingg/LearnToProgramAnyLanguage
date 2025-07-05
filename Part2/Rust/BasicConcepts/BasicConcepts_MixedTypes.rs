fn main()
{
    // Do not mix your types in many languages
    // Rust is strongly typed and will not allow implicit type conversions
    // These will cause compile errors due to type mismatches
    let my_int: i32 = 123.45;
    let my_float: f32 = 'a';
    let my_char: char = 543.21;
    
    println!("myInt: {}", my_int);
    println!("myFloat: {}", my_float);
    println!("myChar: {}", my_char);
}
