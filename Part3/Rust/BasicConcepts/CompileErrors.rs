fn main() {
    
    // Error 1: Using parentheses instead of square brackets for array declaration
    let temperatures: ( 24 )i32;
    
    // Error 2: Missing array size in declaration
    let test_scores: [ ]i32;
    
    // Error 3: Wrong syntax for array initialization
    let book_numbers = [ 5; i32 ]{ 1, 2, 3, 4, 5 };
    
    // Error 4: Missing type annotation for array
    let values = [ ; 5 ];
    
    // Error 5: Wrong syntax for array access
    let temp = temperatures( 0 );
    
    // Error 6: Missing square brackets for array access
    let score = test_scores 2;
    
    // Error 7: Missing comma in array literal
    let bad_array = [ 1 2 3 ];
    
    // Error 8: Missing closing bracket
    let incomplete = [ 1, 2, 3;
    
    // Error 9: Wrong syntax for array repetition
    let repeated = [ 0 * 10 ];  // Should be [0; 10]
    
    // Error 10: Wrong syntax for multidimensional array declaration
    let matrix: [ [ i32 ] ] = [ [ 1, 2 ], [ 3, 4 ] ];
    
    // Error 11: Missing semicolon in array repetition syntax
    let zeros = [ 0 10 ];  // Should be [0; 10]
    
    // Error 12: Using curly braces instead of square brackets
    let array = { 1, 2, 3 };
    
    println!( "{:?}", temperatures );
}
