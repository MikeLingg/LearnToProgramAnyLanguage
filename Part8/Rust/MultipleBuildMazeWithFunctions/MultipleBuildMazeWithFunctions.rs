use std::io::{self, Write};
use std::thread;
use std::time::{Duration, SystemTime, UNIX_EPOCH};



// Simple random number generator using system time as seed
static mut RNG_STATE: u64 = 0;

fn seed_rng ()
{
    unsafe
    {
        RNG_STATE = SystemTime::now ().duration_since ( UNIX_EPOCH ).unwrap ().as_nanos () as u64;
    }
}

fn random_range ( min: usize, max: usize ) -> usize
{
    unsafe
    {
        RNG_STATE = RNG_STATE.wrapping_mul ( 1103515245 ).wrapping_add ( 12345 );
        ( RNG_STATE / 65536 ) as usize % ( max - min ) + min
    }
}



// Define directions
const LEFT: usize = 0;
const UP: usize = 1;
const RIGHT: usize = 2;
const DOWN: usize = 3;

const FIRST_CELL: usize = 0;
const SECOND_CELL: usize = 1;

const NO_CELL: isize = -1;

// Define algorithm types
const KRUSKAL_ALGORITHM: i32 = 1;
const PRIM_ALGORITHM: i32 = 2;
const DEPTH_FIRST_ALGORITHM: i32 = 3;
const BINARY_TREE_ALGORITHM: i32 = 4;
const RECURSIVE_DIVISION_ALGORITHM: i32 = 5;



// Global variables using static mut (unsafe but needed for educational consistency)
static mut ALL_HORIZONTAL_WALLS_UP_GLOB: Vec<bool> = Vec::new();
static mut WALLS_UP_GLOB: Vec<bool> = Vec::new();
static mut INTERIOR_WALL_COUNT_GLOB: usize = 0;
static mut MAZE_ROWS_GLOB: usize = 0;
static mut MAZE_COLUMNS_GLOB: usize = 0;

// Additional globals for algorithm state sharing
static mut CELL_VISITED_GLOB: Vec<bool> = Vec::new();
static mut FRONTIER_WALLS_GLOB: Vec<usize> = Vec::new();
static mut FRONTIER_WALL_COUNT_GLOB: usize = 0;
static mut CELL_IN_MAZE_GLOB: Vec<bool> = Vec::new();

// Lookup tables for wall/cell relationships
static mut WALL_TO_CELLS_GLOB: Vec<Vec<usize>> = Vec::new();
static mut CELL_TO_WALLS_GLOB: Vec<Vec<isize>> = Vec::new();



// +-+-+-+
// Prints a horizontal wall, similar to the example above, 
// with walls down based on the provided parameters.
// horizontal_walls_up_par: Boolean array of size maze_columns_glob, 
//     True indicates the wall should be printed as up.
fn print_horizontal_walls ( horizontal_walls_up_par: &[bool] )
{
    print! ( "+" );
    for horizontal_wall_index in 0..unsafe { MAZE_COLUMNS_GLOB }
    {
        if horizontal_walls_up_par[horizontal_wall_index] == true
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
fn print_all_horizontal_walls ()
{
    unsafe
    {
        print_horizontal_walls ( &ALL_HORIZONTAL_WALLS_UP_GLOB );
    }
}



// | | | |
// Prints a vertical wall, similar to the example above, 
// with walls down based on the provided parameters.
// vertical_walls_up_par: Boolean array of size maze_columns_glob - 1, 
//     True indicates the wall should be printed as up.
fn print_vertical_walls ( vertical_walls_up_par: &[bool] )
{
    // First wall is an exterior wall, always up.
    print! ( "|" );
    for vertical_wall_index in 0..unsafe { MAZE_COLUMNS_GLOB } - 1
    {
        print! ( " " );
        if vertical_walls_up_par[vertical_wall_index] == true
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
// based on walls_up_glob
fn print_maze ()
{
    let mut interior_wall_index = 0;

    // First row is exterior walls
    print_all_horizontal_walls ();
    for row_index in 0..unsafe { MAZE_ROWS_GLOB }
    {
        let mut vertical_walls_up = vec![false; unsafe { MAZE_COLUMNS_GLOB } - 1];
        for column_index in 0..unsafe { MAZE_COLUMNS_GLOB } - 1
        {
            vertical_walls_up[column_index] = unsafe { WALLS_UP_GLOB[interior_wall_index] };
            interior_wall_index = interior_wall_index + 1;
        }

        print_vertical_walls ( &vertical_walls_up );

        if row_index == unsafe { MAZE_ROWS_GLOB } - 1
        {
            print_all_horizontal_walls ();
        }
        else
        {
            let mut horizontal_walls_up = vec![false; unsafe { MAZE_COLUMNS_GLOB }];
            for column_index in 0..unsafe { MAZE_COLUMNS_GLOB }
            {
                horizontal_walls_up[column_index] = unsafe { WALLS_UP_GLOB[interior_wall_index] };
                interior_wall_index = interior_wall_index + 1;
            }

            print_horizontal_walls ( &horizontal_walls_up );
        }
    }

    println! ();
}



// Simple sleep function
fn sleep_half_second ()
{
    thread::sleep ( Duration::from_millis ( 500 ) );
}



// Initialize lookup tables for wall/cell relationships.
// algorithm_type_par: Algorithm constant to determine which tables to build
// Must be called after maze dimensions are set.
fn initialize_lookup_tables ( algorithm_type_par: i32 )
{
    unsafe
    {
        // Build wall_to_cells_glob for algorithms that need wall-to-cell lookups
        if algorithm_type_par == KRUSKAL_ALGORITHM || algorithm_type_par == PRIM_ALGORITHM || 
           algorithm_type_par == DEPTH_FIRST_ALGORITHM || algorithm_type_par == BINARY_TREE_ALGORITHM || 
           algorithm_type_par == RECURSIVE_DIVISION_ALGORITHM
        {
            WALL_TO_CELLS_GLOB = vec![vec![0; 2]; INTERIOR_WALL_COUNT_GLOB];
            
            let mut wall_index = 0;
            for row_index in 0..MAZE_ROWS_GLOB
            {
                // Track the first cell in the current row
                let first_cell_in_row = row_index * MAZE_COLUMNS_GLOB;

                // Note the 0..maze_columns_glob minus 2, one less vertical wall
                // than the number of columns.
                for vertical_wall_index in 0..MAZE_COLUMNS_GLOB - 1
                {
                    let left_cell = first_cell_in_row + vertical_wall_index;
                    let right_cell = left_cell + 1;
                    WALL_TO_CELLS_GLOB[wall_index][FIRST_CELL] = left_cell;
                    WALL_TO_CELLS_GLOB[wall_index][SECOND_CELL] = right_cell;
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
                        WALL_TO_CELLS_GLOB[wall_index][FIRST_CELL] = upper_cell;
                        WALL_TO_CELLS_GLOB[wall_index][SECOND_CELL] = lower_cell;
                        wall_index = wall_index + 1;
                    }
                }
            }
        }

        // Build cell_to_walls_glob for algorithms that need cell-to-wall lookups
        if algorithm_type_par == PRIM_ALGORITHM || algorithm_type_par == DEPTH_FIRST_ALGORITHM || 
           algorithm_type_par == BINARY_TREE_ALGORITHM || algorithm_type_par == RECURSIVE_DIVISION_ALGORITHM
        {
            CELL_TO_WALLS_GLOB = vec![vec![NO_CELL; 4]; MAZE_ROWS_GLOB * MAZE_COLUMNS_GLOB];
            
            for cell_index in 0..MAZE_ROWS_GLOB * MAZE_COLUMNS_GLOB
            {
                let cell_row = cell_index / MAZE_COLUMNS_GLOB;
                let cell_col = cell_index % MAZE_COLUMNS_GLOB;
                
                // Initialize all directions to NO_CELL (invalid wall)
                CELL_TO_WALLS_GLOB[cell_index][LEFT] = NO_CELL;
                CELL_TO_WALLS_GLOB[cell_index][UP] = NO_CELL;
                CELL_TO_WALLS_GLOB[cell_index][RIGHT] = NO_CELL;
                CELL_TO_WALLS_GLOB[cell_index][DOWN] = NO_CELL;
                
                // Find walls by checking which walls connect to this cell
                for wall_index in 0..INTERIOR_WALL_COUNT_GLOB
                {
                    let first_cell_index = WALL_TO_CELLS_GLOB[wall_index][FIRST_CELL];
                    let second_cell_index = WALL_TO_CELLS_GLOB[wall_index][SECOND_CELL];
                    
                    if first_cell_index == cell_index
                    {
                        // This wall connects from our cell to second_cell
                        let second_row = second_cell_index / MAZE_COLUMNS_GLOB;
                        let second_col = second_cell_index % MAZE_COLUMNS_GLOB;
                        
                        if second_row == cell_row && second_col == cell_col + 1
                        {
                            // Wall goes RIGHT
                            CELL_TO_WALLS_GLOB[cell_index][RIGHT] = wall_index as isize;
                        }
                        else if second_row == cell_row + 1 && second_col == cell_col
                        {
                            // Wall goes DOWN
                            CELL_TO_WALLS_GLOB[cell_index][DOWN] = wall_index as isize;
                        }
                    }
                    else if second_cell_index == cell_index
                    {
                        // This wall connects from first_cell_index to our cell
                        let first_row = first_cell_index / MAZE_COLUMNS_GLOB;
                        let first_col = first_cell_index % MAZE_COLUMNS_GLOB;
                        
                        if first_row == cell_row && first_col == cell_col - 1
                        {
                            // Wall comes from LEFT
                            CELL_TO_WALLS_GLOB[cell_index][LEFT] = wall_index as isize;
                        }
                        else if first_row == cell_row - 1 && first_col == cell_col
                        {
                            // Wall comes from UP
                            CELL_TO_WALLS_GLOB[cell_index][UP] = wall_index as isize;
                        }
                    }
                }
            }
        }
    }
}



// Add a wall to frontier if not already there.
// wall_index_par: Index of wall to add to frontier, must be 0 to interior_wall_count_glob-1
fn add_wall_to_frontier ( wall_index_par: usize )
{
    unsafe
    {
        // Check if wall already in frontier
        let mut already_in_frontier = false;
        for wall_index in 0..FRONTIER_WALL_COUNT_GLOB
        {
            if FRONTIER_WALLS_GLOB[wall_index] == wall_index_par
            {
                already_in_frontier = true;
                break;
            }
        }

        if already_in_frontier == false
        {
            FRONTIER_WALLS_GLOB[FRONTIER_WALL_COUNT_GLOB] = wall_index_par;
            FRONTIER_WALL_COUNT_GLOB = FRONTIER_WALL_COUNT_GLOB + 1;
        }
    }
}



// Add all walls adjacent to a cell to the frontier list.
// cell_index_par: Index of the cell whose adjacent walls should be added to frontier: 0 .. ( maze_rows_glob * maze_columns_glob - 1 )
fn add_cell_walls_to_frontier ( cell_index_par: usize )
{
    unsafe
    {
        // Check all four directions
        if CELL_TO_WALLS_GLOB[cell_index_par][UP] != NO_CELL
        {
            add_wall_to_frontier ( CELL_TO_WALLS_GLOB[cell_index_par][UP] as usize );
        }
        if CELL_TO_WALLS_GLOB[cell_index_par][DOWN] != NO_CELL
        {
            add_wall_to_frontier ( CELL_TO_WALLS_GLOB[cell_index_par][DOWN] as usize );
        }
        if CELL_TO_WALLS_GLOB[cell_index_par][LEFT] != NO_CELL
        {
            add_wall_to_frontier ( CELL_TO_WALLS_GLOB[cell_index_par][LEFT] as usize );
        }
        if CELL_TO_WALLS_GLOB[cell_index_par][RIGHT] != NO_CELL
        {
            add_wall_to_frontier ( CELL_TO_WALLS_GLOB[cell_index_par][RIGHT] as usize );
        }
    }
}



// Recursively divide an area with walls.
// start_row_par: Starting row of area to divide, must be 0 to maze_rows_glob-1
// end_row_par: Ending row of area to divide, must be start_row_par to maze_rows_glob-1
// start_col_par: Starting column of area to divide, must be 0 to maze_columns_glob-1
// end_col_par: Ending column of area to divide, must be start_col_par to maze_columns_glob-1
fn divide_area ( start_row_par: usize, end_row_par: usize, start_col_par: usize, end_col_par: usize )
{
    let height = end_row_par - start_row_par + 1;
    let width = end_col_par - start_col_par + 1;

    // Base case - area too small to divide
    if height >= 2 || width >= 2
    {
        // Choose whether to divide horizontally or vertically
        let mut divide_horizontally = false;
        if height > width
        {
            divide_horizontally = true;
        }
        else if width > height
        {
            divide_horizontally = false;
        }
        else
        {
            // Square area - choose randomly
            divide_horizontally = random_range ( 0, 2 ) == 0;
        }

        if divide_horizontally == true && height > 1
        {
            // Choose random row to divide on
            let divide_row = start_row_par + random_range ( 0, end_row_par - start_row_par );

            // Add horizontal wall
            for wall_col in start_col_par..=end_col_par
            {
                let cell_index = divide_row * unsafe { MAZE_COLUMNS_GLOB } + wall_col;
                if cell_index < unsafe { MAZE_ROWS_GLOB * MAZE_COLUMNS_GLOB }
                {
                    let wall_index = unsafe { CELL_TO_WALLS_GLOB[cell_index][DOWN] };
                    if wall_index != NO_CELL
                    {
                        unsafe { WALLS_UP_GLOB[wall_index as usize] = true; }
                    }
                }
            }

            // Choose random gap in the wall
            let gap_col = start_col_par + random_range ( 0, end_col_par - start_col_par + 1 );
            let gap_cell_index = divide_row * unsafe { MAZE_COLUMNS_GLOB } + gap_col;
            if gap_cell_index < unsafe { MAZE_ROWS_GLOB * MAZE_COLUMNS_GLOB }
            {
                let gap_wall_index = unsafe { CELL_TO_WALLS_GLOB[gap_cell_index][DOWN] };
                if gap_wall_index != NO_CELL
                {
                    unsafe { WALLS_UP_GLOB[gap_wall_index as usize] = false; }
                }
            }

            sleep_half_second ();
            print_maze ();

            // Recursively divide the two areas
            divide_area ( start_row_par, divide_row, start_col_par, end_col_par );
            divide_area ( divide_row + 1, end_row_par, start_col_par, end_col_par );
        }
        else if divide_horizontally == false && width > 1
        {
            // Choose random column to divide on
            let divide_col = start_col_par + random_range ( 0, end_col_par - start_col_par );

            // Add vertical wall
            for cell_row in start_row_par..=end_row_par
            {
                let cell_index = cell_row * unsafe { MAZE_COLUMNS_GLOB } + divide_col;
                if cell_index < unsafe { MAZE_ROWS_GLOB * MAZE_COLUMNS_GLOB }
                {
                    let wall_index = unsafe { CELL_TO_WALLS_GLOB[cell_index][RIGHT] };
                    if wall_index != NO_CELL
                    {
                        unsafe { WALLS_UP_GLOB[wall_index as usize] = true; }
                    }
                }
            }

            // Choose random gap in the wall
            let gap_row = start_row_par + random_range ( 0, end_row_par - start_row_par + 1 );
            let gap_cell_index = gap_row * unsafe { MAZE_COLUMNS_GLOB } + divide_col;
            if gap_cell_index < unsafe { MAZE_ROWS_GLOB * MAZE_COLUMNS_GLOB }
            {
                let gap_wall_index = unsafe { CELL_TO_WALLS_GLOB[gap_cell_index][RIGHT] };
                if gap_wall_index != NO_CELL
                {
                    unsafe { WALLS_UP_GLOB[gap_wall_index as usize] = false; }
                }
            }

            sleep_half_second ();
            print_maze ();

            // Recursively divide the two areas
            divide_area ( start_row_par, end_row_par, start_col_par, divide_col );
            divide_area ( start_row_par, end_row_par, divide_col + 1, end_col_par );
        }
    }
}



// Kruskal's algorithm.
// The simple description of the algorithm is first place each 
// cell in its own group.  Then process all walls in random order,
// if the cells on either side of the wall are in separate groups, 
// remove the wall and merge the groups.  Repeat until all 
// cells are now in the same group.
fn build_maze_kruskal ()
{
    unsafe
    {
        // Identify which cells are a part of each group
        // Note each array is large enough to fit all cells,
        // NO_CELL indicates no cell is assigned to this index.
        // Languages like python that make adding/removing 
        // array items easy will not need this, for C arrays
        // we will discuss reallocation or linked list in 
        // later videos for covering changing array sizes.
        let mut cell_to_group = vec![0; MAZE_ROWS_GLOB * MAZE_COLUMNS_GLOB];
        let mut group_cells = vec![vec![NO_CELL as isize; MAZE_ROWS_GLOB * MAZE_COLUMNS_GLOB]; MAZE_ROWS_GLOB * MAZE_COLUMNS_GLOB];

        for cell_index in 0..MAZE_ROWS_GLOB * MAZE_COLUMNS_GLOB
        {
            cell_to_group[cell_index] = cell_index;

            for group_cell_index in 0..MAZE_ROWS_GLOB * MAZE_COLUMNS_GLOB
            {
                if group_cell_index == 0
                {
                    group_cells[cell_index][group_cell_index] = cell_index as isize;
                }
                else
                {
                    group_cells[cell_index][group_cell_index] = NO_CELL;
                }
            }
        }

        let mut maze_complete = false;

        let mut wall_remove_list = vec![0; INTERIOR_WALL_COUNT_GLOB];
        for wall_index in 0..INTERIOR_WALL_COUNT_GLOB
        {
            wall_remove_list[wall_index] = wall_index;
        }
        
        // Fisher-Yates shuffle
        for shuffle_index in ( 1..INTERIOR_WALL_COUNT_GLOB ).rev ()
        {
            let random_index = random_range ( 0, shuffle_index + 1 );
            let temp = wall_remove_list[shuffle_index];
            wall_remove_list[shuffle_index] = wall_remove_list[random_index];
            wall_remove_list[random_index] = temp;
        }

        // Perform Kruskal's algorithm.
        for remove_wall_index in 0..INTERIOR_WALL_COUNT_GLOB
        {
            let next_wall_to_check = wall_remove_list[remove_wall_index];

            // If the two cells connected to this wall are not part 
            // of the same group, remove the wall and merge the 
            // groups.
            let first_cell_index = WALL_TO_CELLS_GLOB[next_wall_to_check][FIRST_CELL];
            let first_cell_group_index = cell_to_group[first_cell_index];
            let second_cell_index = WALL_TO_CELLS_GLOB[next_wall_to_check][SECOND_CELL];
            let second_cell_group_index = cell_to_group[second_cell_index];
            if first_cell_group_index != second_cell_group_index
            {
                WALLS_UP_GLOB[next_wall_to_check] = false;

                // Loop through the indices of all cells in the first 
                // group until we find a NO_CELL indicating no cell here.
                let mut next_empty_first_group_index = 0;
                for cell_index in 0..MAZE_COLUMNS_GLOB * MAZE_ROWS_GLOB
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
                for group_cell_index in ( 0..MAZE_COLUMNS_GLOB * MAZE_ROWS_GLOB ).rev ()
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
                        // Remove the cell from the second group (set the
                        // array entry to NO_CELL)
                        group_cells[second_cell_group_index][group_cell_index] = NO_CELL;

                        if next_empty_first_group_index >= MAZE_COLUMNS_GLOB * MAZE_ROWS_GLOB
                        {
                            maze_complete = true;
                        }
                    }
                }

                sleep_half_second ();
                print_maze ();

                if maze_complete == true
                {
                    break;
                }
            }
        }
    }
}



// Prim's algorithm for maze generation.
// Start with a random cell, mark it as part of the maze.
// Repeatedly pick a random wall from cells in the maze that 
// connects to a cell not in the maze, remove the wall and 
// add the new cell to the maze.
fn build_maze_prim ()
{
    unsafe
    {
        // Initialize algorithm state
        FRONTIER_WALL_COUNT_GLOB = 0;

        // Track how many cells have been added to the maze
        let mut cells_in_maze = 0;

        // Start with a random cell
        let start_cell = random_range ( 0, MAZE_ROWS_GLOB * MAZE_COLUMNS_GLOB );
        CELL_IN_MAZE_GLOB[start_cell] = true;
        cells_in_maze = cells_in_maze + 1;

        // Add all walls adjacent to the start cell to frontier
        add_cell_walls_to_frontier ( start_cell );

        // Continue until all cells are in the maze
        while cells_in_maze < MAZE_ROWS_GLOB * MAZE_COLUMNS_GLOB
        {
            // Pick a random wall from frontier
            let random_wall_index = random_range ( 0, FRONTIER_WALL_COUNT_GLOB );
            let wall_to_check = FRONTIER_WALLS_GLOB[random_wall_index];

            // Remove this wall from frontier list by replacing 
            // it with the last wall in the list
            FRONTIER_WALLS_GLOB[random_wall_index] = FRONTIER_WALLS_GLOB[FRONTIER_WALL_COUNT_GLOB - 1];
            FRONTIER_WALL_COUNT_GLOB = FRONTIER_WALL_COUNT_GLOB - 1;

            // Get the two cells this wall connects
            let first_cell_index = WALL_TO_CELLS_GLOB[wall_to_check][FIRST_CELL];
            let second_cell_index = WALL_TO_CELLS_GLOB[wall_to_check][SECOND_CELL];

            // If one cell is already in the maze and the 
            // other is not, remove the wall to connect the 
            // outside cell to the maze
            if CELL_IN_MAZE_GLOB[first_cell_index] != CELL_IN_MAZE_GLOB[second_cell_index]
            {
                WALLS_UP_GLOB[wall_to_check] = false;

                let outer_cell_index;

                // Add the outside cell to the maze
                if CELL_IN_MAZE_GLOB[first_cell_index] == false
                {
                    outer_cell_index = first_cell_index;
                }
                else
                {
                    outer_cell_index = second_cell_index;
                }

                CELL_IN_MAZE_GLOB[outer_cell_index] = true;
                cells_in_maze = cells_in_maze + 1;
                add_cell_walls_to_frontier ( outer_cell_index );

                sleep_half_second ();
                print_maze ();
            }
        }
    }
}



// Depth-first search maze generation using recursive backtracking.
// Start at a random cell, randomly walk to neighbors outside 
// the maze,removing walls as you go. 
// When all neighbors are in the maze, backtrack to a cell 
// with neighbors outside the maze.
fn build_maze_depth_first ()
{
    unsafe
    {
        // Stack for backtracking - store cell indices
        let mut cell_stack = vec![0; MAZE_ROWS_GLOB * MAZE_COLUMNS_GLOB];
        let mut stack_size = 0;

        // Start with random cell
        let mut current_cell_index = random_range ( 0, MAZE_ROWS_GLOB * MAZE_COLUMNS_GLOB );
        CELL_VISITED_GLOB[current_cell_index] = true;

        // Push starting cell onto stack
        cell_stack[stack_size] = current_cell_index;
        stack_size = stack_size + 1;

        while stack_size > 0
        {
            // Create randomized direction list
            let mut randomized_directions = [LEFT, UP, RIGHT, DOWN];
            
            // Fisher-Yates shuffle the directions
            for shuffle_index in ( 1..4 ).rev ()
            {
                let random_index = random_range ( 0, shuffle_index + 1 );
                let temp = randomized_directions[shuffle_index];
                randomized_directions[shuffle_index] = randomized_directions[random_index];
                randomized_directions[random_index] = temp;
            }

            let mut found_neighbor = false;
            let mut next_cell_index = 0;
            let mut wall_index = NO_CELL as isize;

            // Check directions in random order until we find an unvisited neighbor
            for direction_index in 0..4
            {
                let direction = randomized_directions[direction_index];
                wall_index = CELL_TO_WALLS_GLOB[current_cell_index][direction];
                if wall_index != NO_CELL
                {
                    // Find the cell on the other side of this wall
                    let first_cell_index = WALL_TO_CELLS_GLOB[wall_index as usize][FIRST_CELL];
                    let second_cell_index = WALL_TO_CELLS_GLOB[wall_index as usize][SECOND_CELL];
                    
                    if first_cell_index == current_cell_index
                    {
                        next_cell_index = second_cell_index;
                    }
                    else
                    {
                        next_cell_index = first_cell_index;
                    }
                    
                    // Check if neighbor is unvisited
                    if CELL_VISITED_GLOB[next_cell_index] == false
                    {
                        found_neighbor = true;
                        break;
                    }
                }
            }

            if found_neighbor == true
            {
                // Find wall between current and next cell using lookup tables
                wall_index = NO_CELL;
                for direction in 0..4
                {
                    let wall_candidate = CELL_TO_WALLS_GLOB[current_cell_index][direction];
                    if wall_candidate != NO_CELL
                    {
                        let first_cell_index = WALL_TO_CELLS_GLOB[wall_candidate as usize][FIRST_CELL];
                        let second_cell_index = WALL_TO_CELLS_GLOB[wall_candidate as usize][SECOND_CELL];
                        if ( first_cell_index == current_cell_index && second_cell_index == next_cell_index ) || 
                           ( first_cell_index == next_cell_index && second_cell_index == current_cell_index )
                        {
                            wall_index = wall_candidate;
                            break;
                        }
                    }
                }
                WALLS_UP_GLOB[wall_index as usize] = false;

                // Mark next cell as visited
                CELL_VISITED_GLOB[next_cell_index] = true;

                // Push next cell onto stack
                cell_stack[stack_size] = next_cell_index;
                stack_size = stack_size + 1;

                current_cell_index = next_cell_index;

                sleep_half_second ();
                print_maze ();
            }
            else
            {
                // Backtrack - pop from stack
                stack_size = stack_size - 1;
                if stack_size > 0
                {
                    current_cell_index = cell_stack[stack_size - 1];
                }
            }
        }
    }
}



// Binary Tree maze generation algorithm.
// For each cell, randomly choose to either remove the wall 
// to the north or the wall to the east (if they exist).
// This creates a maze with a distinctive bias.
fn build_maze_binary_tree ()
{
    unsafe
    {
        for cell_row in 0..MAZE_ROWS_GLOB
        {
            for cell_col in 0..MAZE_COLUMNS_GLOB
            {
                let current_cell_index = cell_row * MAZE_COLUMNS_GLOB + cell_col;
                let mut valid_walls = [0; 2];
                let mut valid_wall_count = 0;

                // Check if we can go north (UP) - only if not in top row
                if cell_row > 0
                {
                    let wall_index = CELL_TO_WALLS_GLOB[current_cell_index][UP];
                    if wall_index != NO_CELL
                    {
                        valid_walls[valid_wall_count] = wall_index as usize;
                        valid_wall_count = valid_wall_count + 1;
                    }
                }

                // Check if we can go east (RIGHT) - only if not in rightmost column
                if cell_col < MAZE_COLUMNS_GLOB - 1
                {
                    let wall_index = CELL_TO_WALLS_GLOB[current_cell_index][RIGHT];
                    if wall_index != NO_CELL
                    {
                        valid_walls[valid_wall_count] = wall_index as usize;
                        valid_wall_count = valid_wall_count + 1;
                    }
                }

                // If we have at least one valid wall, pick one randomly and remove it
                if valid_wall_count > 0
                {
                    let random_wall_index = random_range ( 0, valid_wall_count );
                    let wall_to_remove = valid_walls[random_wall_index];
                    WALLS_UP_GLOB[wall_to_remove] = false;
                }

                sleep_half_second ();
                print_maze ();
            }
        }
    }
}



// Recursive Division maze generation algorithm.
// Start with an empty area (all walls down), then recursively
// divide the area with walls, leaving random gaps.
fn build_maze_recursive_division ()
{
    unsafe
    {
        // Start with all interior walls down
        for wall_index in 0..INTERIOR_WALL_COUNT_GLOB
        {
            WALLS_UP_GLOB[wall_index] = false;
        }

        print_maze ();
        sleep_half_second ();

        // Recursively divide the entire maze area
        divide_area ( 0, MAZE_ROWS_GLOB - 1, 0, MAZE_COLUMNS_GLOB - 1 );
    }
}



// Note we could use command line arguments instead of user 
// prompts to get the maze size and algorithm choice.
fn main ()
{
    // Seed random number generator
    seed_rng ();

    // Prompt the user for maze size
    unsafe
    {
        MAZE_COLUMNS_GLOB = 0;
        while MAZE_COLUMNS_GLOB <= 0
        {
            print! ( "Please enter number of columns for maze, must be greater than 1: " );
            io::stdout ().flush ().unwrap ();
            
            let mut user_input = String::new ();
            if io::stdin ().read_line ( &mut user_input ).is_ok ()
            {
                let user_input = user_input.trim ();
                
                // Check if string is a valid number
                let mut is_valid = true;
                if user_input.len () == 0
                {
                    is_valid = false;
                }
                else
                {
                    for character_index in 0..user_input.len ()
                    {
                        let character = user_input.chars ().nth ( character_index ).unwrap ();
                        if character < '0' || character > '9'
                        {
                            is_valid = false;
                            break;
                        }
                    }
                }
                
                if is_valid == true
                {
                    if let Ok ( value ) = user_input.parse::<usize> ()
                    {
                        MAZE_COLUMNS_GLOB = value;
                    }
                }
            }
        }

        MAZE_ROWS_GLOB = 0;
        while MAZE_ROWS_GLOB <= 0
        {
            print! ( "Please enter number of rows for maze, must be greater than 1: " );
            io::stdout ().flush ().unwrap ();
            
            let mut user_input = String::new ();
            if io::stdin ().read_line ( &mut user_input ).is_ok ()
            {
                let user_input = user_input.trim ();
                
                // Check if string is a valid number
                let mut is_valid = true;
                if user_input.len () == 0
                {
                    is_valid = false;
                }
                else
                {
                    for character_index in 0..user_input.len ()
                    {
                        let character = user_input.chars ().nth ( character_index ).unwrap ();
                        if character < '0' || character > '9'
                        {
                            is_valid = false;
                            break;
                        }
                    }
                }
                
                if is_valid == true
                {
                    if let Ok ( value ) = user_input.parse::<usize> ()
                    {
                        MAZE_ROWS_GLOB = value;
                    }
                }
            }
        }

        // Prompt the user for algorithm choice
        let mut algorithm_choice = 0;
        while algorithm_choice < 1 || algorithm_choice > 5
        {
            println! ( "Please choose maze generation algorithm:" );
            println! ( "1 - Kruskal's Algorithm" );
            println! ( "2 - Prim's Algorithm" );
            println! ( "3 - Depth-First Search" );
            println! ( "4 - Binary Tree Algorithm" );
            println! ( "5 - Recursive Division Algorithm" );
            
            let mut user_input = String::new ();
            if io::stdin ().read_line ( &mut user_input ).is_ok ()
            {
                let user_input = user_input.trim ();
                
                // Check if string is a valid number
                let mut is_valid = true;
                if user_input.len () == 0
                {
                    is_valid = false;
                }
                else
                {
                    for character_index in 0..user_input.len ()
                    {
                        let character = user_input.chars ().nth ( character_index ).unwrap ();
                        if character < '0' || character > '9'
                        {
                            is_valid = false;
                            break;
                        }
                    }
                }
                
                if is_valid == true
                {
                    if let Ok ( value ) = user_input.parse::<i32> ()
                    {
                        algorithm_choice = value;
                    }
                }
            }
        }

        // Setup maze datastructures for the user entered size.
        ALL_HORIZONTAL_WALLS_UP_GLOB = vec![true; MAZE_COLUMNS_GLOB];
        
        INTERIOR_WALL_COUNT_GLOB = MAZE_ROWS_GLOB * ( MAZE_COLUMNS_GLOB - 1 ) + ( MAZE_ROWS_GLOB - 1 ) * MAZE_COLUMNS_GLOB;
        
        WALLS_UP_GLOB = vec![true; INTERIOR_WALL_COUNT_GLOB];

        // Initialize algorithm state globals
        CELL_VISITED_GLOB = vec![false; MAZE_ROWS_GLOB * MAZE_COLUMNS_GLOB];
        
        FRONTIER_WALLS_GLOB = vec![0; INTERIOR_WALL_COUNT_GLOB];
        
        CELL_IN_MAZE_GLOB = vec![false; MAZE_ROWS_GLOB * MAZE_COLUMNS_GLOB];

        // Initialize lookup tables based on chosen algorithm
        initialize_lookup_tables ( algorithm_choice );

        // Execute the chosen algorithm
        if algorithm_choice == KRUSKAL_ALGORITHM
        {
            build_maze_kruskal ();
        }
        else if algorithm_choice == PRIM_ALGORITHM
        {
            build_maze_prim ();
        }
        else if algorithm_choice == DEPTH_FIRST_ALGORITHM
        {
            build_maze_depth_first ();
        }
        else if algorithm_choice == BINARY_TREE_ALGORITHM
        {
            build_maze_binary_tree ();
        }
        else if algorithm_choice == RECURSIVE_DIVISION_ALGORITHM
        {
            build_maze_recursive_division ();
        }

        println! ( "Press Enter to exit..." );
        let mut _input = String::new ();
        io::stdin ().read_line ( &mut _input ).unwrap ();
    }
}
