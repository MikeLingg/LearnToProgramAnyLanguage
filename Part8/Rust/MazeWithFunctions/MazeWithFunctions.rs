use std::io::Write;
use std::thread;
use std::time::Duration;
use std::time::{SystemTime, UNIX_EPOCH};



// Simple linear congruential generator for random numbers
struct SimpleRng
{
    state: u64,
}

impl SimpleRng
{
    fn new() -> Self
    {
        let seed = SystemTime::now()
            .duration_since ( UNIX_EPOCH )
            .unwrap()
            .as_nanos() as u64;
        SimpleRng { state: seed }
    }

    fn next ( &mut self ) -> u32
    {
        self.state = self.state.wrapping_mul ( 1103515245 ).wrapping_add ( 12345 );
        ( self.state >> 16 ) as u32
    }

    fn gen_range ( &mut self, min: i32, max: i32 ) -> i32
    {
        let range = ( max - min + 1 ) as u32;
        ( self.next() % range ) as i32 + min
    }
}



// Define directions
//const LEFT: i32 = 0;
//const UP: i32 = 1;
//const RIGHT: i32 = 2;
//const DOWN: i32 = 3;

const FIRST_CELL: usize = 0;
const SECOND_CELL: usize = 1;

const NO_CELL: i32 = -1;



// Global variables
static mut ALL_HORIZONTAL_WALLS_UP_GLOB: Vec<bool> = Vec::new();
static mut WALLS_UP_GLOB: Vec<bool> = Vec::new();
static mut INTERIOR_WALL_COUNT_GLOB: i32 = 0;
static mut MAZE_ROWS_GLOB: i32 = 0;
static mut MAZE_COLUMNS_GLOB: i32 = 0;



// +-+-+-+
// Prints a horizontal wall, similar to the example above, 
// with walls down based on the provided parameters.
// horizontal_walls_up_par: Boolean vector of size maze_columns, 
//     True indicates the wall should be printed as up.
unsafe fn print_horizontal_walls ( horizontal_walls_up_par: &Vec<bool> )
{
    print! ( "+" );
    for horizontal_wall_index in 0..MAZE_COLUMNS_GLOB
    {
        if horizontal_walls_up_par[horizontal_wall_index as usize] == true
        {
            print! ( "-" );
        }
        else
        {
            print! ( " " );
        }
        print! ( "+" );
    }
    println! ();
}



// +-+-+-+
// Prints a horizontal wall, similar to the example above, with all walls up.
unsafe fn print_horizontal_walls_all()
{
    print_horizontal_walls ( &ALL_HORIZONTAL_WALLS_UP_GLOB );
}



// | | | |
// Prints a vertical wall, similar to the example above, 
// with walls down based on the provided parameters.
// vertical_walls_up_par: Boolean vector of size maze_columns - 1, 
//     True indicates the wall should be printed as up.
unsafe fn print_vertical_walls ( vertical_walls_up_par: &Vec<bool> )
{
    // First wall is an exterior wall, always up.
    print! ( "|" );
    for vertical_wall_index in 0..MAZE_COLUMNS_GLOB - 1
    {
        print! ( " " );
        if vertical_walls_up_par[vertical_wall_index as usize] == true
        {
            print! ( "|" );
        }
        else
        {
            print! ( " " );
        }
    }
    // Last wall exterior, always up.
    print! ( " " );
    print! ( "|" );
    println! ();
}



// Loop through the rows of the maze and print the maze 
// based on WALLS_UP_GLOB
unsafe fn print_maze()
{
    let mut interior_wall_index = 0;

    // First row is exterior walls
    print_horizontal_walls_all();
    for row_index in 0..MAZE_ROWS_GLOB
    {
        let mut vertical_walls_up = vec! [false; ( MAZE_COLUMNS_GLOB - 1 ) as usize];
        for column_index in 0..MAZE_COLUMNS_GLOB - 1
        {
            vertical_walls_up[column_index as usize] = WALLS_UP_GLOB[interior_wall_index as usize];
            interior_wall_index = interior_wall_index + 1;
        }

        print_vertical_walls ( &vertical_walls_up );

        if row_index == MAZE_ROWS_GLOB - 1
        {
            print_horizontal_walls_all();
        }
        else
        {
            let mut horizontal_walls_up = vec! [false; MAZE_COLUMNS_GLOB as usize];
            for column_index in 0..MAZE_COLUMNS_GLOB
            {
                horizontal_walls_up[column_index as usize] = WALLS_UP_GLOB[interior_wall_index as usize];
                interior_wall_index = interior_wall_index + 1;
            }

            print_horizontal_walls ( &horizontal_walls_up );
        }
    }

    println! ();
}



// Simple sleep function
fn sleep_half_second()
{
    thread::sleep ( Duration::from_millis ( 500 ) );
}



// Kruskal's algorithm.
// The simple description of the algorithm is first place each 
// cell in its own group.  Then process all walls in random order,
// if the cells on either side of the wall are in separate groups, 
// remove the wall and merge the groups.  Repeat until all 
// cells are now in the same group.
unsafe fn build_maze_kruskal()
{
    // Identify the cells each wall connects.
    let mut wall_connections = vec! [vec! [0; 2]; INTERIOR_WALL_COUNT_GLOB as usize];
    let mut wall_remove_list = vec! [0; INTERIOR_WALL_COUNT_GLOB as usize];
    let mut cell_to_group = vec! [0; ( MAZE_ROWS_GLOB * MAZE_COLUMNS_GLOB ) as usize];
    let mut group_cells = vec! [vec! [NO_CELL; ( MAZE_ROWS_GLOB * MAZE_COLUMNS_GLOB ) as usize]; ( MAZE_ROWS_GLOB * MAZE_COLUMNS_GLOB ) as usize];

    let mut wall_index = 0;
    for row_index in 0..MAZE_ROWS_GLOB
    {
        // Track the first cell in the current row
        let first_cell_in_row = row_index * MAZE_COLUMNS_GLOB;

        // Note the 0..maze_columns minus 2, one less vertical wall
        // than the number of columns.
        for vertical_wall_index in 0..MAZE_COLUMNS_GLOB - 1
        {
            let left_cell = first_cell_in_row + vertical_wall_index;
            let right_cell = left_cell + 1;
            wall_connections[wall_index as usize][FIRST_CELL] = left_cell;
            wall_connections[wall_index as usize][SECOND_CELL] = right_cell;
            wall_index = wall_index + 1;
        }

        // The last row will have no interior horizontal walls below
        // it, so will be skipped.
        if wall_index < INTERIOR_WALL_COUNT_GLOB
        {
            for horizontal_wall_index in 0..MAZE_COLUMNS_GLOB
            {
                let upper_cell = first_cell_in_row + horizontal_wall_index;
                let lower_cell = upper_cell + MAZE_COLUMNS_GLOB;
                wall_connections[wall_index as usize][FIRST_CELL] = upper_cell;
                wall_connections[wall_index as usize][SECOND_CELL] = lower_cell;
                wall_index = wall_index + 1;
            }
        }
    }

    for cell_index in 0..MAZE_ROWS_GLOB * MAZE_COLUMNS_GLOB
    {
        cell_to_group[cell_index as usize] = cell_index;

        for inner_cell_index in 0..MAZE_ROWS_GLOB * MAZE_COLUMNS_GLOB
        {
            if inner_cell_index == 0
            {
                group_cells[cell_index as usize][inner_cell_index as usize] = cell_index;
            }
            else
            {
                group_cells[cell_index as usize][inner_cell_index as usize] = NO_CELL;
            }
        }
    }

    let mut maze_complete = false;

    for wall_index_loop in 0..INTERIOR_WALL_COUNT_GLOB
    {
        wall_remove_list[wall_index_loop as usize] = wall_index_loop;
    }

    // Fisher-Yates shuffle
    let mut rng = SimpleRng::new();
    for i in ( 1..INTERIOR_WALL_COUNT_GLOB ).rev()
    {
        let j = rng.gen_range ( 0, i );
        let temp = wall_remove_list[i as usize];
        wall_remove_list[i as usize] = wall_remove_list[j as usize];
        wall_remove_list[j as usize] = temp;
    }

    // Perform Kruskal's algorithm.
    for remove_wall_index in 0..INTERIOR_WALL_COUNT_GLOB
    {
        let next_wall_to_check = wall_remove_list[remove_wall_index as usize];

        // If the two cells connected to this wall are not part 
        // of the same group, remove the wall and merge the 
        // groups.
        let first_cell = wall_connections[next_wall_to_check as usize][FIRST_CELL];
        let first_cell_group_index = cell_to_group[first_cell as usize];
        let second_cell = wall_connections[next_wall_to_check as usize][SECOND_CELL];
        let second_cell_group_index = cell_to_group[second_cell as usize];
        if first_cell_group_index != second_cell_group_index
        {
            WALLS_UP_GLOB[next_wall_to_check as usize] = false;

            // Loop through the indices of all cells in the first 
            // group until we find a NO_CELL indicating no cell here.
            let mut next_empty_first_group_index = 0;
            for cell_index in 0..MAZE_COLUMNS_GLOB * MAZE_ROWS_GLOB
            {
                if group_cells[first_cell_group_index as usize][cell_index as usize] == NO_CELL
                {
                    next_empty_first_group_index = cell_index;
                    break;
                }
            }

            // Loop through the indices of all cells in the second group,
            // move each cell to the first group, and set that cell's 
            // group to the first group index.
            for group_cell_index in ( 0..MAZE_COLUMNS_GLOB * MAZE_ROWS_GLOB ).rev()
            {
                // Skip until we reach valid cells
                if group_cells[second_cell_group_index as usize][group_cell_index as usize] != NO_CELL
                {
                    // Get the id number of the cell to move from 
                    // the second group to the first group
                    let cell_to_move = group_cells[second_cell_group_index as usize][group_cell_index as usize];

                    // Move the cell number from the second group 
                    // to the first group
                    group_cells[first_cell_group_index as usize][next_empty_first_group_index as usize] = cell_to_move;
                    // Move our empty index to the next cell in this array.
                    next_empty_first_group_index = next_empty_first_group_index + 1;
                    // Mark this cell as part of the first group.
                    cell_to_group[cell_to_move as usize] = first_cell_group_index;
                    // Remove the cell from the second group (set the
                    // array entry to NO_CELL)
                    group_cells[second_cell_group_index as usize][group_cell_index as usize] = NO_CELL;

                    if next_empty_first_group_index >= MAZE_COLUMNS_GLOB * MAZE_ROWS_GLOB
                    {
                        maze_complete = true;
                    }
                }
            }

            sleep_half_second();

            print_maze();

            if maze_complete == true
            {
                break;
            }
        }
    }
}



// Note we could use command line arguments instead of user 
// prompts to get the maze size.
fn main()
{
    unsafe
    {
        // Prompt the user for maze size
        MAZE_COLUMNS_GLOB = 0;
        while MAZE_COLUMNS_GLOB <= 0
        {
            print! ( "Please enter number of columns for maze, must be greater than 1: " );
            Write::flush ( &mut std::io::stdout() ).unwrap();
            
            let mut user_input = String::new();
            std::io::stdin().read_line ( &mut user_input ).unwrap();
            
            match user_input.trim().parse::<i32>()
            {
                Ok ( temp_value ) =>
                {
                    if temp_value > 1
                    {
                        MAZE_COLUMNS_GLOB = temp_value;
                    }
                }
                Err ( _ ) => {}
            }
        }

        MAZE_ROWS_GLOB = 0;
        while MAZE_ROWS_GLOB <= 0
        {
            print! ( "Please enter number of rows for maze, must be greater than 1: " );
            Write::flush ( &mut std::io::stdout() ).unwrap();
            
            let mut user_input = String::new();
            std::io::stdin().read_line ( &mut user_input ).unwrap();
            
            match user_input.trim().parse::<i32>()
            {
                Ok ( temp_value ) =>
                {
                    if temp_value > 1
                    {
                        MAZE_ROWS_GLOB = temp_value;
                    }
                }
                Err ( _ ) => {}
            }
        }

        // Setup maze datastructures for the user entered size.
        ALL_HORIZONTAL_WALLS_UP_GLOB = vec! [true; MAZE_COLUMNS_GLOB as usize];

        INTERIOR_WALL_COUNT_GLOB = MAZE_ROWS_GLOB * ( MAZE_COLUMNS_GLOB - 1 ) + ( MAZE_ROWS_GLOB - 1 ) * MAZE_COLUMNS_GLOB;

        WALLS_UP_GLOB = vec! [true; INTERIOR_WALL_COUNT_GLOB as usize];

        build_maze_kruskal();
    }
}
