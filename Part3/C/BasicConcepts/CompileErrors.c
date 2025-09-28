#include <stdio.h>
#include <stdbool.h>

int main() {
    
    // Error 1: Using parentheses instead of square brackets for array declaration
    int temperatures( 24 );
    
    // Error 2: Missing array size with no initializer
    int testScores[ ];
    
    // Error 3: Using curly braces without assignment operator
    int values1[ 3 ] { 10, 20, 30 };
    
    // Error 4: Missing semicolon after array declaration
    int values2[ 5 ] = { 1, 2, 3, 4, 5 }
    
    // Error 5: Using parentheses instead of square brackets for array access
    int temp = temperatures( 0 );
    
    // Error 6: Missing square brackets for array access
    int score = testScores 2;
    
    // Error 7: Using assignment operator instead of initialization in declaration
    int values3[ 3 ] == { 7, 8, 9 };
    
    // Error 8: Declaring variable length array with non-const in global scope
    int size = 10;
    int dynamicArray[ size ];
    
    // Error 9: Wrong syntax for 2D array initialization
    int matrix[ 2 ][ 2 ] = [ [ 1, 2 ], [ 3, 4 ] ];
    
    // Error 10: Missing commas in array initializer
    int values4[ 4 ] = { 10 20 30 40 };
    
    // Error 11: Using single quotes for string instead of double quotes
    char message[ ] = 'Hello World';
    
    // Error 12: Using double quotes for single character
    char ch = "A";
    
    // Error 13: Missing null terminator awareness (logical error that compiles)
    char badString[ 5 ] = { 'H', 'e', 'l', 'l', 'o' };
    
    // Error 14: Wrong array bounds in loop (off-by-one)
    int numbers[ 5 ] = { 1, 2, 3, 4, 5 };
    for( int i = 0; i <= 5; i++ ) {
        numbers[ i ] = i * 2;
    }
    
    // Error 15: Trying to assign to array name (arrays are not assignable)
    int source[ 3 ] = { 1, 2, 3 };
    int dest[ 3 ];
    dest = source;
    
    // Error 16: Using wrong format specifier for bool
    bool flag = true;
    printf( "Flag value: %d\n", flag );
    
    // Error 17: Incomplete struct declaration
    struct {
        int data[ 10 ];
        int count;
    } incomplete
    
    // Error 18: Using non-integer array index
    float index = 2.5;
    int value = numbers[ index ];
    
    // Error 19: Negative array size
    int negativeArray[ -5 ];
    
    // Error 20: Variable length array in struct (not allowed in C89/90)
    struct VLAStruct {
        int n;
        int data[ n ];
    };
    
    // Error 21: Excess elements in array initializer
    char shortArray[ 3 ] = { 'A', 'B', 'C', 'D', 'E' };
    
    // Error 22: Using const in wrong context for array size
    const int ARRAY_SIZE;
    int constArray[ ARRAY_SIZE ];
    
    // Error 23: Missing closing brace in initializer
    int bracketError[ 3 ] = { 1, 2, 3 ;
    
    return 0;
}
