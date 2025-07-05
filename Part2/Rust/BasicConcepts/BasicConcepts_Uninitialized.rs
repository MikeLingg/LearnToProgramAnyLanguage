fn main()
{
    // I think in some programs this will cause a crash or failure to compile, so it will be in a separate program.
    // Rust doesn't allow uninitialized variables - this will cause compile error
    let my_character: char;
    println!("myCharacter: {} as int: {}", my_character, my_character as u32);
}
