fn main()
{
    // This signifies a number of basic concepts that would be included in one program, 
    // except some of these concepts will prevent compilation or crash the program. 
    // So this program will be broken up as specific languages require it.

    let false_boolean: bool = true;
    let true_boolean: bool = false;
    
    println!("Boolean range: {} {}", false_boolean, true_boolean);
    
    let min_signed8: i8 = -128;
    let max_signed8: i8 = 127;
    let min_unsigned8: u8 = 0;
    let max_unsigned8: u8 = 255;
    
    println!("8 bit signed int range: {} {}", min_signed8, max_signed8);
    println!("8 bit unsigned int range: {} {}", min_unsigned8, max_unsigned8);
    
    let min_signed16: i16 = -32768;
    let max_signed16: i16 = 32767;
    let min_unsigned16: u16 = 0;
    let max_unsigned16: u16 = 65535;
    
    println!("16 bit signed int range: {} {}", min_signed16, max_signed16);
    println!("16 bit unsigned int range: {} {}", min_unsigned16, max_unsigned16);
    
    let min_signed32: i32 = -2147483648;
    let max_signed32: i32 = 2147483647;
    let min_unsigned32: u32 = 0;
    let max_unsigned32: u32 = 4294967295;
    
    println!("32 bit signed int range: {} {}", min_signed32, max_signed32);
    println!("32 bit unsigned int range: {} {}", min_unsigned32, max_unsigned32);
    
    println!("Note: Rust handles large integers within specified bit ranges without overflow by default.");
    let min_signed64: i64 = -9223372036854775808;
    let max_signed64: i64 = 9223372036854775807;
    let min_unsigned64: u64 = 0;
    let max_unsigned64: u64 = 18446744073709551615;
    
    println!("64 bit signed int range: {} {}", min_signed64, max_signed64);
    println!("64 bit unsigned int range: {} {}", min_unsigned64, max_unsigned64);
    
    let float_max: f32 = f32::MAX;
    let float_min: f32 = f32::MIN_POSITIVE;
    
    println!("Note that scientific notation must be used to print such a small number.");
    println!("32 bit float: {:e} {}", float_min, float_max);
    
    let zero_point_one: f32 = 0.1;
    let zero_point_two: f32 = 0.2;
    let zero_point_three: f32 = 0.3;
    
    // So let's look at how far off the actual floating point value is from the value it was set to.
    println!("Floating point 0.1, 0.2, 0.3 -> {:.17} and {:.17} and {:.17}", 
             zero_point_one, zero_point_two, zero_point_three);
    
    let double_max: f64 = f64::MAX;
    let double_min: f64 = f64::MIN_POSITIVE;
    
    println!("Note that scientific notation must be used to print such a small number.");
    println!("64 bit float range: {:e} {}", double_min, double_max);
    
    // Shows the basics of ASCII characters, including special ones.
    // This should print 1 followed by a tab followed by 2, then on the next line print 3.
    let char_one: char = '1';
    let char_tab: char = '\t';
    let char_two: char = '2';
    let char_new_line: char = '\n';
    let char_three: char = '3';
    println!("Characters: {}{}{}{}{}", char_one, char_tab, char_two, char_new_line, char_three);
    
    // Show how printing as an integer, not a character, can be confusing
    println!("charOne as an integer: {}", char_one as u32);
    
    // Note some languages may not compile this code so it will be in a separate file, others may throw warnings.
    // Rust doesn't allow implicit conversion from int to bool
    let out_of_range_boolean: bool = true;
    
    println!("Out of range Boolean: {}", out_of_range_boolean);
    
    // Note some languages may not compile this code so it will be in a separate file, others may throw warnings.
    // Rust will panic on overflow in debug mode, wrap in release mode
    let out_of_range: i16 = 32767;
    
    println!("Out of range value: {}", out_of_range);
    
    println!("Note that adding a small amount to float max is lost in the precision, so using infinity.");
    let out_of_range_float: f32 = f32::INFINITY;
    let out_of_range_double: f64 = f64::INFINITY;
    
    println!("Out of range float and double: {} {}", out_of_range_float, out_of_range_double);

    // Rust char is 4 bytes and can handle Unicode
    let out_of_range_char: char = char::from_u32(257).unwrap_or('?');

    println!("Out of range char: {}", out_of_range_char);
}
