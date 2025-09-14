use std::thread;
use std::time::Duration;

fn main( ) 
{
    // Define directions
    // const LEFT: usize = 0;
    // const UP: usize = 1;
    // const RIGHT: usize = 2;
    // const DOWN: usize = 3;

    const FIRST_CELL: usize = 0;
    const SECOND_CELL: usize = 1;
    
    const NO_CELL: i32 = -1;
    
    const INTERIOR_WALL_COUNT: usize = 12;

    // For each cell 0-8, indicate if a wall is exterior and cannot be removed ( -1 ) or its interior index for each of
    // the four directions, LEFT, UP, RIGHT, DOWN.
    /*
    let _cell_to_wall_lut: [[i32; 4]; 9] = [
        [-1, -1, 0, 2],  [0, -1, 1, 3],   [1, -1, -1, 4],
        [-1, 2, 5, 7],   [5, 3, 6, 8],    [6, 4, -1, 9],
        [-1, 7, 10, -1], [10, 8, 11, -1], [11, 9, -1, -1]
    ];
    */

    // 12 interior walls in a 3x3 maze. Start with all the walls up.
    let mut walls_up = [true; 12];

    // Identify the cells each wall connects.
    let wall_connections: [[usize; 2]; 12] = [
        [0, 1], [1, 2], [0, 3], [1, 4], [2, 5], [3, 4], 
        [4, 5], [3, 6], [4, 7], [5, 8], [6, 7], [7, 8]
    ];

    // Identify which group each cell is a part of.
    let mut cell_to_group = [0, 1, 2, 3, 4, 5, 6, 7, 8];

    // Identify which cells are a part of each group
    let mut group_cells: [[i32; 9]; 9] = [
        [0, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL],
        [1, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL],
        [2, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL],
        [3, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL],
        [4, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL],
        [5, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL],
        [6, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL],
        [7, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL],
        [8, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL, NO_CELL]
    ];

    // Print maze code:
    // Print out the maze, this is a less painful copy/paste job without functions, but better with loops.
    let mut current_interior_wall = 0;

    // Print the horizontal walls above row 1 - All are exterior walls, no conditions.
    // +-+-+-+
    // One initial cell with + followed by 3 cells with -+
    print!( "+" );
    for _cell_index in 0..3 
    {
        print!( "-" );
        print!( "+" );
    }
    println!( );

    for row_index in 0..3 
    {
        // Vertical walls and cells row 1.
        // The left and right vertical walls are exterior, always up.
        // | | | |
        // Or print one |, followed by 3 cells of <space>| where the |
        // may be down ( <space> ).
        print!( "|" );
        for cell_index in 0..3 
        {
            print!( " " );

            // Always print the right most vertical wall,
            // if interior wall, print if the wall is up.
            if cell_index == 2 || walls_up[current_interior_wall] == true 
            {
                print!( "|" );
            }
            else 
            {
                print!( " " );
            }
            if cell_index < 2 
            {
                current_interior_wall = current_interior_wall + 1;
            }
        }
        println!( );

        // One fewer horizontal wall than vertical
        if row_index < 2 
        {
            // Horizontal walls above row row_index
            // +-+-+-+
            print!( "+" );
            for _cell_index in 0..3 
            {
                if walls_up[current_interior_wall] == true 
                {
                    print!( "-" );
                }
                else 
                {
                    print!( " " );
                }
                print!( "+" );
            }
            println!( );
        }

        current_interior_wall = current_interior_wall + 1;
    }

    // Horizontal walls below row 3 - All are exterior walls, no conditions.
    // +-+-+-+
    print!( "+" );
    for _cell_index in 0..3 
    {
        print!( "-" );
        print!( "+" );
    }
    println!( );

    // Create and randomize wall removal list
    let mut wall_remove_list: [usize; 12] = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11];
    
    // Manual Fisher-Yates shuffle using simple LCG for randomization
    let mut seed: u32 = std::ptr::addr_of!( wall_remove_list ) as u32;
    
    for shuffle_index in ( 1..12 ).rev( )
    {
        // Simple Linear Congruential Generator for randomization
        seed = seed.wrapping_mul( 1103515245 ).wrapping_add( 12345 );
        let other_index = ( seed % ( shuffle_index + 1 ) as u32 ) as usize;
        
        // Swap wall_remove_list[shuffle_index] with wall_remove_list[other_index]
        let temp = wall_remove_list[shuffle_index];
        wall_remove_list[shuffle_index] = wall_remove_list[other_index];
        wall_remove_list[other_index] = temp;
    }

    let mut maze_complete = false;

    // Remove wall code:
    // Now that we have loops we can implement Kruskal's algorithm.
    // The simple description of the algorithm is first place each 
    // cell in its own group.  Then process all walls in random order,
    // if the cells on either side of the wall are in separate groups, 
    // remove the wall and merge the groups.  Repeat until all 
    // cells are now in the same group.
    for remove_wall_index in 0..INTERIOR_WALL_COUNT 
    {
        let next_wall_to_check = wall_remove_list[remove_wall_index];

        // If the two cells connected to this wall are not part 
        // of the same group, remove the wall and merge the 
        // groups.
        let first_cell = wall_connections[next_wall_to_check][FIRST_CELL];
        let first_cell_group_index = cell_to_group[first_cell];
        let second_cell = wall_connections[next_wall_to_check][SECOND_CELL];
        let second_cell_group_index = cell_to_group[second_cell];
        
        if first_cell_group_index != second_cell_group_index 
        {
            walls_up[next_wall_to_check] = false;

            // Loop through the indices of all cells in the first 
            // group until we find a NO_CELL indicating no cell here.
            let mut next_empty_first_group_index = 0;
            for cell_index in 0..9 
            {
                if group_cells[first_cell_group_index][cell_index] == NO_CELL 
                {
                    next_empty_first_group_index = cell_index;
                    break;
                }
            }

            // Loop through the indices of all cells in the second group,
            // move each cell to the first group, and set that cell's 
            // group to the first group index.
            for group_cell_index in ( 0..9 ).rev( ) 
            {
                // Skip until we reach valid cells
                if group_cells[second_cell_group_index][group_cell_index] != NO_CELL 
                {
                    // Get the id number of the cell to move from 
                    // the second group to the first group
                    let cell_to_move = group_cells[second_cell_group_index][group_cell_index];

                    // Move the cell number from the second group 
                    // to the first group
                    group_cells[first_cell_group_index][next_empty_first_group_index] = cell_to_move;
                    // Move our empty index to the next cell in this array.
                    next_empty_first_group_index = next_empty_first_group_index + 1;
                    // Mark this cell as part of the first group.
                    cell_to_group[cell_to_move as usize] = first_cell_group_index;
                    // Remove the cell from the second group ( set the
                    // array entry to NO_CELL )
                    group_cells[second_cell_group_index][group_cell_index] = NO_CELL;
                    
                    if next_empty_first_group_index >= 9 
                    {
                        maze_complete = true;
                    }
                }
            }

            // Print maze code ( copied from above ):
            thread::sleep( Duration::from_millis( 500 ) ); // Sleep for 500 milliseconds
            current_interior_wall = 0;

            println!( );
            print!( "+" );
            for _cell_index in 0..3 
            {
                print!( "-" );
                print!( "+" );
            }
            println!( );

            for row_index in 0..3 
            {
                print!( "|" );
                for cell_index in 0..3 
                {
                    print!( " " );

                    if cell_index == 2 || walls_up[current_interior_wall] == true 
                    {
                        print!( "|" );
                    }
                    else 
                    {
                        print!( " " );
                    }
                    if cell_index < 2 
                    {
                        current_interior_wall = current_interior_wall + 1;
                    }
                }
                println!( );

                if row_index < 2 
                {
                    print!( "+" );
                    for _cell_index in 0..3 
                    {
                        if walls_up[current_interior_wall] == true 
                        {
                            print!( "-" );
                        }
                        else 
                        {
                            print!( " " );
                        }
                        print!( "+" );

                        current_interior_wall = current_interior_wall + 1;
                    }
                    println!( );
                }
            }

            print!( "+" );
            for _cell_index in 0..3 
            {
                print!( "-" );
                print!( "+" );
            }
            println!( );
        }

        if maze_complete == true 
        {
            break;
        }
    }
}
