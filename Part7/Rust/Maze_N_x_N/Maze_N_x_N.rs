use std::io::{self, Write};
use std::thread;
use std::time::Duration;

fn main ( )
{
    const FIRST_CELL: usize = 0;
    const SECOND_CELL: usize = 1;
    const NO_CELL: i32 = -1;
    
    // Prompt the user for maze size
    let mut maze_columns = 0;
    while maze_columns <= 0
    {
        print! ( "Please enter number of columns for maze, must be greater than 1: " );
        io::stdout ( ).flush ( ).unwrap ( );
        
        let mut input = String::new ( );
        io::stdin ( ).read_line ( &mut input ).unwrap ( );
        
        if let Ok ( num ) = input.trim ( ).parse::<usize> ( )
        {
            maze_columns = num;
        }
    }
    
    let mut maze_rows = 0;
    while maze_rows <= 0
    {
        print! ( "Please enter number of rows for maze, must be greater than 1: " );
        io::stdout ( ).flush ( ).unwrap ( );
        
        let mut input = String::new ( );
        io::stdin ( ).read_line ( &mut input ).unwrap ( );
        
        if let Ok ( num ) = input.trim ( ).parse::<usize> ( )
        {
            maze_rows = num;
        }
    }
    
    let interior_wall_count = maze_rows * ( maze_columns - 1 ) + ( maze_rows - 1 ) * maze_columns;
    
    // Start with all the walls up
    let mut walls_up = vec! [ true; interior_wall_count ];
    
    // Identify the cells each wall connects
    let mut wall_connections = vec! [ [ 0usize; 2 ]; interior_wall_count ];
    
    let mut wall_index = 0;
    for row_index in 0..maze_rows
    {
        let first_cell_in_row = row_index * maze_columns;
        
        // Vertical walls
        for vertical_wall_index in 0..maze_columns - 1
        {
            let left_cell = first_cell_in_row + vertical_wall_index;
            let right_cell = left_cell + 1;
            wall_connections[ wall_index ][ FIRST_CELL ] = left_cell;
            wall_connections[ wall_index ][ SECOND_CELL ] = right_cell;
            wall_index += 1;
        }
        
        // Horizontal walls
        if wall_index < interior_wall_count
        {
            for horizontal_wall_index in 0..maze_columns
            {
                let upper_cell = first_cell_in_row + horizontal_wall_index;
                let lower_cell = upper_cell + maze_columns;
                wall_connections[ wall_index ][ FIRST_CELL ] = upper_cell;
                wall_connections[ wall_index ][ SECOND_CELL ] = lower_cell;
                wall_index += 1;
            }
        }
    }
    
    // Identify which group each cell is a part of
    let mut cell_to_group: Vec<usize> = ( 0..maze_rows * maze_columns ).collect ( );
    
    // Identify which cells are a part of each group
    let mut group_cells = vec! [ vec! [ NO_CELL; maze_rows * maze_columns ]; maze_rows * maze_columns ];
    for cell_index in 0..maze_rows * maze_columns
    {
        group_cells[ cell_index ][ 0 ] = cell_index as i32;
    }
    
    // Print initial maze
    let mut current_interior_wall = 0;
    
    // Print top border
    print! ( "+" );
    for _ in 0..maze_columns
    {
        print! ( "-+" );
    }
    println! ( );
    
    for row_index in 0..maze_rows
    {
        // Print vertical walls and cells
        print! ( "|" );
        for column_index in 0..maze_columns
        {
            print! ( " " );
            
            if column_index == maze_columns - 1 || walls_up[ current_interior_wall ]
            {
                print! ( "|" );
            }
            else
            {
                print! ( " " );
            }
            
            if column_index < maze_columns - 1
            {
                current_interior_wall += 1;
            }
        }
        println! ( );
        
        // Print horizontal walls
        if row_index < maze_rows - 1
        {
            print! ( "+" );
            for _ in 0..maze_columns
            {
                if walls_up[ current_interior_wall ]
                {
                    print! ( "-" );
                }
                else
                {
                    print! ( " " );
                }
                print! ( "+" );
                current_interior_wall += 1;
            }
            println! ( );
        }
    }
    
    // Print bottom border
    print! ( "+" );
    for _ in 0..maze_columns
    {
        print! ( "-+" );
    }
    println! ( );
    
    // Create wall removal list
    let mut wall_remove_list: Vec<usize> = ( 0..interior_wall_count ).collect ( );
    
    // Simple pseudo-random number generator using multiple time sources
    let now = std::time::SystemTime::now ( ).duration_since ( std::time::UNIX_EPOCH ).unwrap ( );
    let mut seed = now.as_secs ( ).wrapping_mul ( 1000000000 ).wrapping_add ( now.subsec_nanos ( ) as u64 );
    
    // Mix in more entropy
    seed = seed.wrapping_mul ( 6364136223846793005 ).wrapping_add ( 1442695040888963407 );
    
    // Fisher-Yates shuffle algorithm
    for shuffle_index in ( 1..interior_wall_count ).rev ( )
    {
        // Better LCG with different constants
        seed = seed.wrapping_mul ( 6364136223846793005 ).wrapping_add ( 1442695040888963407 );
        let other_index = ( seed as usize ) % ( shuffle_index + 1 );
        let temp = wall_remove_list[ shuffle_index ];
        wall_remove_list[ shuffle_index ] = wall_remove_list[ other_index ];
        wall_remove_list[ other_index ] = temp;
    }
    
    let mut maze_complete = false;
    
    // Kruskal's algorithm
    for remove_wall_index in 0..interior_wall_count
    {
        let next_wall_to_check = wall_remove_list[ remove_wall_index ];
        
        let first_cell = wall_connections[ next_wall_to_check ][ FIRST_CELL ];
        let first_cell_group_index = cell_to_group[ first_cell ];
        let second_cell = wall_connections[ next_wall_to_check ][ SECOND_CELL ];
        let second_cell_group_index = cell_to_group[ second_cell ];
        
        if first_cell_group_index != second_cell_group_index
        {
            walls_up[ next_wall_to_check ] = false;
            
            // Find next empty position in first group
            let mut next_empty_first_group_index = 0;
            for cell_index in 0..maze_columns * maze_rows
            {
                if group_cells[ first_cell_group_index ][ cell_index ] == NO_CELL
                {
                    next_empty_first_group_index = cell_index;
                    break;
                }
            }
            
            // Move all cells from second group to first group
            for group_cell_index in ( 0..maze_columns * maze_rows ).rev ( )
            {
                if group_cells[ second_cell_group_index ][ group_cell_index ] != NO_CELL
                {
                    let cell_to_move = group_cells[ second_cell_group_index ][ group_cell_index ];
                    
                    group_cells[ first_cell_group_index ][ next_empty_first_group_index ] = cell_to_move;
                    next_empty_first_group_index += 1;
                    cell_to_group[ cell_to_move as usize ] = first_cell_group_index;
                    group_cells[ second_cell_group_index ][ group_cell_index ] = NO_CELL;
                    
                    if next_empty_first_group_index >= maze_columns * maze_rows
                    {
                        maze_complete = true;
                    }
                }
            }
            
            thread::sleep ( Duration::from_millis ( 500 ) );
            
            // Copy in Print maze code from above again here:
            current_interior_wall = 0;
            
            println! ( );
            print! ( "+" );
            for _ in 0..maze_columns
            {
                print! ( "-+" );
            }
            println! ( );
            
            for row_index in 0..maze_rows
            {
                print! ( "|" );
                for column_index in 0..maze_columns
                {
                    print! ( " " );
                    
                    if column_index == maze_columns - 1 || walls_up[ current_interior_wall ]
                    {
                        print! ( "|" );
                    }
                    else
                    {
                        print! ( " " );
                    }
                    
                    if column_index < maze_columns - 1
                    {
                        current_interior_wall += 1;
                    }
                }
                println! ( );
                
                if row_index < maze_rows - 1
                {
                    print! ( "+" );
                    for _ in 0..maze_columns
                    {
                        if walls_up[ current_interior_wall ]
                        {
                            print! ( "-" );
                        }
                        else
                        {
                            print! ( " " );
                        }
                        print! ( "+" );
                        current_interior_wall += 1;
                    }
                    println! ( );
                }
            }
            
            print! ( "+" );
            for _ in 0..maze_columns
            {
                print! ( "-+" );
            }
            println! ( );
            
            if maze_complete
            {
                break;
            }
        }
    }
}
