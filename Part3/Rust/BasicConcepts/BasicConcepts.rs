fn main() {
    // Note the structured example is assuming zero based indexing.
    // One based index languages will differ.
    // Also note how some references are array location, with ranges from first to last, 
    // while indexes being zero based are 0 to size - 1.
    
    let mut temperatures = [ 55, 58, 60, 65, 70, 73, 76, 79, 81, 83, 84, 85, 85, 84, 83, 81, 78, 75, 72, 69, 65, 62, 59, 57 ];
    let mut test_scores = [ 95, 75, 86, 86, 78, 94 ];
    let mut book_number = [ 12495, 35786, 15863, 84962, 42697 ];
    
    // 10th entry (0-based index 9)
    println!( "Temperature at tenth hour is {}", temperatures[ 9 ] );
    // 4th entry (0-based index 3)
    println!( "Fourth student grade is {}", test_scores[ 3 ] );
    
    // 2nd entry (0-based index 1)
    let book_two_index = 1;
    println!( "Second book index is {}", book_number[ book_two_index ] );
    
    // First and last 0 based indexes, 0 and array size - 1.
    let hour_count = 24;
    let first_temperature_index = 0;
    let last_temperature_index = hour_count - 1;
    println!( "First temperature is {}", temperatures[ first_temperature_index ] );
    println!( "Last temperature is {}", temperatures[ last_temperature_index ] );
    
    // set temperature first entry to 65
    temperatures[ 0 ] = 65;
    println!( "First temperature is now {}", temperatures[ 0 ] );
    
    // set test_scores fourth entry to 99
    test_scores[ 3 ] = 99;
    println!( "Fourth test score is now {}", test_scores[ 3 ] );
    
    // set book_number at index third entry to 75681
    let book_index = 2;
    book_number[ book_index ] = 75681;
    println!( "Third book number is now {}", book_number[ book_index ] );
    
    // Large arrays - Rust requires explicit initialization
    let large_array_size = 10000;
    let mut large_array = [ false; 10000 ];
    let mut large_array1 = [ 0; 1000 ];
    let mut large_array2 = [ 0.0; 5000 ];
    
    println!( "First large array first and last initial values: {} {}", large_array[ 0 ], large_array[ large_array_size - 1 ] );
    println!( "Second large array first and last initial values: {} {}", large_array1[ 0 ], large_array1[ 999 ] );
    println!( "Third large array first and last initial values: {} {}", large_array2[ 0 ], large_array2[ 4999 ] );
    
    // set large_array first entry to true
    large_array[ 0 ] = true;
    // set large_array last entry to false
    large_array[ large_array_size - 1 ] = false;
    println!( "First large array first and last values: {} {}", large_array[ 0 ], large_array[ large_array_size - 1 ] );
    
    // set large_array1 first entry to 25
    large_array1[ 0 ] = 25;
    // set large_array1 last entry to 55
    large_array1[ 999 ] = 55;
    println!( "Second large array first and last values: {} {}", large_array1[ 0 ], large_array1[ 999 ] );
    
    // set large_array2 first entry to 27.5
    large_array2[ 0 ] = 27.5;
    // set large_array2 last entry to 58.25
    large_array2[ 4999 ] = 58.25;
    println!( "Third large array first and last values: {:.1} {:.2}", large_array2[ 0 ], large_array2[ 4999 ] );
    
    // Character array (Rust has both strings and char arrays)
    let mut my_string = [ 0u8; 100 ];
    my_string[ 0 ] = b'H';
    my_string[ 1 ] = b'e';
    my_string[ 2 ] = b'l';
    my_string[ 3 ] = b'l';
    my_string[ 4 ] = b'o';
    my_string[ 5 ] = b' ';
    my_string[ 6 ] = b'W';
    my_string[ 7 ] = b'o';
    my_string[ 8 ] = b'r';
    my_string[ 9 ] = b'l';
    my_string[ 10 ] = b'd';
    my_string[ 11 ] = b'.';
    my_string[ 12 ] = 0;
    // Convert byte array to string, stopping at null terminator
    let null_index = my_string.iter().position( |&x| x == 0 ).unwrap_or( my_string.len() );
    println!( "{}", std::str::from_utf8( &my_string[ ..null_index ] ).unwrap() );
    
    let my_string1 = [ b'H', b'e', b'l', b'l', b'o', b' ', b'W', b'o', b'r', b'l', b'd', b'.', 0u8 ];
    let null_index1 = my_string1.iter().position( |&x| x == 0 ).unwrap_or( my_string1.len() );
    println!( "{}", std::str::from_utf8( &my_string1[ ..null_index1 ] ).unwrap() );
    
    // Rust strings are UTF-8 and handled differently
    let my_string2 = "Hello World.";
    println!( "{}", my_string2 );

    // 2D Array
    let mut two_d_array = [ [ '\0'; 4 ]; 4 ];
    two_d_array[ 0 ][ 0 ] = '0';
    two_d_array[ 0 ][ 1 ] = '1';
    two_d_array[ 0 ][ 2 ] = '2';
    two_d_array[ 0 ][ 3 ] = '3';
    two_d_array[ 1 ][ 0 ] = '4';
    two_d_array[ 1 ][ 1 ] = '5';
    two_d_array[ 1 ][ 2 ] = '6';
    two_d_array[ 1 ][ 3 ] = '7';
    two_d_array[ 2 ][ 0 ] = '8';
    two_d_array[ 2 ][ 1 ] = '9';
    two_d_array[ 2 ][ 2 ] = 'A';
    two_d_array[ 2 ][ 3 ] = 'B';
    two_d_array[ 3 ][ 0 ] = 'C';
    two_d_array[ 3 ][ 1 ] = 'D';
    two_d_array[ 3 ][ 2 ] = 'E';
    two_d_array[ 3 ][ 3 ] = 'F';
    
    // Note: the actual implementation of this code will use some advanced 
    // techniques that will not be described, only the results of the code observed.
    print!( "twoDArray memory location as flat data: " );
    for i in 0..4 {
        for j in 0..4 {
            print!( "{}", two_d_array[ i ][ j ] );
        }
    }
    println!();
    
    // Note these are not defined as constant, but the capital naming 
    // indicates the values should not change.
    const RED: usize = 0;
    const GREEN: usize = 1;
    const BLUE: usize = 2;
    const YELLOW: usize = 3;
    const CYAN: usize = 4;
    const MAGENTA: usize = 5;
    const WHITE: usize = 6;
    
    // Columns: Red Intensity, Green Intensity, Blue Intensity
    let color_table = [
        [ 255, 0,   0   ],  // Red
        [ 0,   255, 0   ],  // Green
        [ 0,   0,   255 ],  // Blue
        [ 255, 255, 0   ],  // Yellow = Red + Green
        [ 0,   255, 255 ],  // Cyan   = Green + Blue
        [ 255, 0,   255 ],  // Magenta = Red + Blue
        [ 255, 255, 255 ]   // White = Red + Green + Blue
    ];
    
    println!( "CYAN color values: {} {} {}", color_table[ CYAN ][ 0 ], 
              color_table[ CYAN ][ 1 ], color_table[ CYAN ][ 2 ] );
}
