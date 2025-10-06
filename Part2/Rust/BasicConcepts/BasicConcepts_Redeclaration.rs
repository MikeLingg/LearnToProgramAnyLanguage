use std::char;
use std::i8;
use std::f32;

fn main() {
    // Do not redeclare variable names in most languages:
    let duplicateCharacter: char = 'a';
    let duplicateCharacter: char = 'b';
    println!("duplicateCharacter: {}", duplicateCharacter);
}