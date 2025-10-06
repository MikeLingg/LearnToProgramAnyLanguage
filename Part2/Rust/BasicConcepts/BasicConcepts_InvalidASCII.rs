fn main()
{
    // So the ASCII table shows the tab symbol as TAB, but this doesn't work in programming.
    // I think in some programs this will crash, so it will be in a separate program.
    // Rust doesn't allow multi-character literals - this will cause compile error
    let char_invalid: char = 'TAB';
    println!("Invalid char: {}", char_invalid);
}
