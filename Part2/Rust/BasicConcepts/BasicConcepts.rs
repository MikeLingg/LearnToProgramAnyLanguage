const COLOR_RESET: &str = "\x1b[0m";
const COLOR_BOLD: &str = "\x1b[1m";

fn print_bg_rgb(r: u8, g: u8, b: u8, text: &str) {
    print!("\x1b[48;2;{};{};{}m{}{}", r, g, b, text, COLOR_RESET);
}

fn value_to_color(normalized: f64) -> (u8, u8, u8) {
    if normalized < 0.5 {
        // Blue to Green
        let t = normalized * 2.0;
        (0, (t * 255.0) as u8, ((1.0 - t) * 255.0) as u8)
    } else {
        // Green to Red
        let t = (normalized - 0.5) * 2.0;
        ((t * 255.0) as u8, ((1.0 - t) * 255.0) as u8, 0)
    }
}

fn demonstrate_8bit() {
    println!("\n{COLOR_BOLD}=== 8-BIT UNSIGNED (0-255) ==={COLOR_RESET}");
    println!("256 possible values - Coarse granularity\n");
    
    let values: [u16; 12] = [10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 127, 255];
    
    for val in values.iter() {
        let normalized = *val as f64 / 255.0;
        let (r, g, b) = value_to_color(normalized);
        
        print!("  {:3}: ", val);
        print_bg_rgb(r, g, b, "    ");
        println!(" RGB({:3},{:3},{:3})", r, g, b);
    }
}

fn demonstrate_16bit() {
    println!("\n{COLOR_BOLD}=== 16-BIT UNSIGNED (0-65535) ==={COLOR_RESET}");
    println!("65,536 possible values - 256x finer than 8-bit\n");
    
    let values: [u32; 21] = [10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 3000, 6000, 9000, 12000, 15000, 18000, 21000, 24000, 27000, 32767, 65535];
    
    for val in values.iter() {
        let normalized = *val as f64 / 65535.0;
        let (r, g, b) = value_to_color(normalized);
        
        print!("  {:5}: ", val);
        print_bg_rgb(r, g, b, "    ");
        println!(" RGB({:3},{:3},{:3})", r, g, b);
    }
}

fn demonstrate_32bit() {
    println!("\n{COLOR_BOLD}=== 32-BIT UNSIGNED (0-4294967295) ==={COLOR_RESET}");
    println!("4,294,967,296 possible values - 65,536x finer than 16-bit\n");
    
    let values: [u64; 17] = [10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 250000000, 500000000, 1000000000, 1500000000, 2000000000,
                             2147483647, 4294967295];
    
    for val in values.iter() {
        let normalized = *val as f64 / 4294967295.0;
        let (r, g, b) = value_to_color(normalized);
        
        print!("  {:10}: ", val);
        print_bg_rgb(r, g, b, "    ");
        println!(" RGB({:3},{:3},{:3})", r, g, b);
    }
}

fn demonstrate_64bit() {
    println!("\n{COLOR_BOLD}=== 64-BIT UNSIGNED (0-18446744073709551615) ==={COLOR_RESET}");
    println!("18,446,744,073,709,551,616 possible values - 4,294,967,296x finer than 32-bit\n");
    
    let values: [u64; 16] = [10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 625000000000000000, 1250000000000000000, 2500000000000000000, 5000000000000000000,
                             9223372036854775807, 18446744073709551615];
    
    for val in values.iter() {
        let normalized = *val as f64 / 18446744073709551615.0;
        let (r, g, b) = value_to_color(normalized);
        
        print!("  {:20}: ", val);
        print_bg_rgb(r, g, b, "    ");
        println!(" RGB({:3},{:3},{:3})", r, g, b);
    }
}


fn main()
{
    // This signifies a number of basic concepts that would be included in one program, 
    // except some of these concepts will prevent compilation or crash the program. 
    // So this program will be broken up as specific languages require it.

    let false_boolean: bool = false;
    let true_boolean: bool = true;
    
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
    
    let min_signed64: i64 = -9223372036854775808;
    let max_signed64: i64 = 9223372036854775807;
    let min_unsigned64: u64 = 0;
    let max_unsigned64: u64 = 18446744073709551615;
    
    println!("64 bit signed int range: {} {}", min_signed64, max_signed64);
    println!("64 bit unsigned int range: {} {}", min_unsigned64, max_unsigned64);

    demonstrate_8bit();
    demonstrate_16bit();
    demonstrate_32bit();
    demonstrate_64bit();
    
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
    let single_quotes: char = '\'';
    let char_new_line: char = '\n';
    let double_quotes: char = '\"';
    println!("Characters: {}{}{}{}{}", char_one, char_tab, single_quotes, char_new_line, double_quotes);
    
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
    
    println!("Note that adding a small amount to float max is lost in the precision, so we doubled MAX.");
    let out_of_range_float: f32 = f32::MAX + f32::MAX;
    let out_of_range_double: f64 = -f64::MAX - f64::MAX;
    
    println!("Out of range float and double: {} {}", out_of_range_float, out_of_range_double);

    // Rust char is 4 bytes and can handle Unicode
    let out_of_range_char: char = char::from_u32(257).unwrap_or('?');

    println!("Out of range char: {}", out_of_range_char);
}
