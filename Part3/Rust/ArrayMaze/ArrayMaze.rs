fn main() {
    // This maze program does not necessarily progress the program, 
    // but provides a construct we will use eventually in the program.
    let maze_walls = [
        [ '|', ' ', '\0' ],
        [ ' ', ' ', ' ' ],
        [ ' ', '|', '\0' ],
        [ ' ', '-', ' ' ],
        [ '|', ' ', '\0' ]
    ];
    
    const LEFT: usize = 0;
    const UP: usize = 1;
    const RIGHT: usize = 2;
    const DOWN: usize = 3;
    
    let neighbor_lookup = [
        [ -1, -1, -1, 3 ],  // Cell 0
        [ -1, -1, 2, 4 ],   // Cell 1
        [ 1, -1, -1, 5 ],   // Cell 2
        [ -1, 0, 4, 6 ],    // Cell 3
        [ 3, 1, -1, -1 ],   // Cell 4
        [ -1, 2, -1, 8 ],   // Cell 5
        [ -1, 3, -1, -1 ],  // Cell 6
        [ -1, -1, 8, -1 ],  // Cell 7
        [ 7, 5, -1, -1 ]    // Cell 8
    ];
    
    let top_bottom_row = [ '+', '-', '+', '-', '+', '-', '+' ];
    
    // This sort of code will look better when we have loops and functions
    // We see how a 3x3 maze is created from the array of data, and using the 
    // neighbor lookup provides -1 where a wall is in that direction, otherwise 
    // provides the cell number in that direction.
    print!( "{}", top_bottom_row[ 0 ] );
    print!( "{}", top_bottom_row[ 1 ] );
    print!( "{}", top_bottom_row[ 2 ] );
    print!( "{}", top_bottom_row[ 3 ] );
    print!( "{}", top_bottom_row[ 4 ] );
    print!( "{}", top_bottom_row[ 5 ] );
    print!( "{}", top_bottom_row[ 6 ] );
    println!();
    
    print!( "|0" );
    print!( "{}", maze_walls[ 0 ][ 0 ] );
    print!( "1" );
    print!( "{}", maze_walls[ 0 ][ 1 ] );
    print!( "2|" );
    println!();
    
    print!( "+" );
    print!( "{}", maze_walls[ 1 ][ 0 ] );
    print!( "+" );
    print!( "{}", maze_walls[ 1 ][ 1 ] );
    print!( "+" );
    print!( "{}", maze_walls[ 1 ][ 2 ] );
    print!( "+" );
    println!();
    
    print!( "|3" );
    print!( "{}", maze_walls[ 2 ][ 0 ] );
    print!( "4" );
    print!( "{}", maze_walls[ 2 ][ 1 ] );
    print!( "5|" );
    println!();
    
    print!( "+" );
    print!( "{}", maze_walls[ 3 ][ 0 ] );
    print!( "+" );
    print!( "{}", maze_walls[ 3 ][ 1 ] );
    print!( "+" );
    print!( "{}", maze_walls[ 3 ][ 2 ] );
    print!( "+" );
    println!();
    
    print!( "|6" );
    print!( "{}", maze_walls[ 4 ][ 0 ] );
    print!( "7" );
    print!( "{}", maze_walls[ 4 ][ 1 ] );
    print!( "8|" );
    println!();
    
    print!( "{}", top_bottom_row[ 0 ] );
    print!( "{}", top_bottom_row[ 1 ] );
    print!( "{}", top_bottom_row[ 2 ] );
    print!( "{}", top_bottom_row[ 3 ] );
    print!( "{}", top_bottom_row[ 4 ] );
    print!( "{}", top_bottom_row[ 5 ] );
    print!( "{}", top_bottom_row[ 6 ] );
    println!();
    
    // So take cell number 4 and see what rooms are around it, 
    // cell 3 is to the left and cell 1 is up, but walls are right and down.
    println!( "Cell to left of cell 4 is: {}", neighbor_lookup[ 4 ][ LEFT ] );
    println!( "Cell to up of cell 4 is: {}", neighbor_lookup[ 4 ][ UP ] );
    println!( "Cell to right of cell 4 is: {}", neighbor_lookup[ 4 ][ RIGHT ] );
    println!( "Cell to down of cell 4 is: {}", neighbor_lookup[ 4 ][ DOWN ] );
}
