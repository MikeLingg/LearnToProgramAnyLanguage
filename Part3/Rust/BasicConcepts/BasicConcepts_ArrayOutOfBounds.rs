fn main() {
    // Struct to demonstrate buffer overflow concepts
    struct TestStruct {
        int_array: [ i32; 10 ],
        my_int: i32,
    }

    let mut test_struct = TestStruct {
        int_array: [ 0; 10 ],
        my_int: 0,
    };

    // In Rust, buffer overflows are prevented by bounds checking in safe code
    // This code demonstrates what would happen in unsafe code
    // The following would panic in safe Rust:
    // Calculate out of bounds index to prevent compiler optimization
    let out_of_bounds_index = test_struct.int_array.len();  // This equals 10
    test_struct.int_array[ out_of_bounds_index ] = 55;  // This will panic with index out of bounds
    
    // my_int is separate from int_array in Rust, so this won't show corruption like C/C++
    println!( "myInt value: {}", test_struct.my_int );

    test_struct.my_int = test_struct.my_int + 1;

    // This will also panic since we're accessing out of bounds
    println!( "Out of bounds array value: {}", test_struct.int_array[ out_of_bounds_index ] );

}
