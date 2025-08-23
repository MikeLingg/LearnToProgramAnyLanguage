use std::{thread, time::Duration};

fn main() {
    // Initialization code:
    // Cell indexes:
    // +-+-+-+
    // |0|1|2|
    // +-+-+-+
    // |3|4|5|
    // +-+-+-+
    // |6|7|8|
    // +-+-+-+
    
    // Interior wall indexes:
    // +-+-+-+
    // | 0 1 |
    // +2+3+4+
    // | 5 6 |
    // +7+8+9+
    // | 10 11 |
    // +-+-+-+
    
    // Define directions
    const LEFT: usize = 0;
    //const UP: usize = 1;
    //const RIGHT: usize = 2;
    //const DOWN: usize = 3;
    
    // For each cell 0-8, indicate if a wall is exterior and cannot be removed (-1) or its interior index for each of
    // the four directions, LEFT, UP, RIGHT, DOWN.
    let cell_to_wall_lut: [[i32; 4]; 9] = [
        [-1, -1, 0, 2],   [0, -1, 1, 3],   [1, -1, -1, 4],
        [-1, 2, 5, 7],    [5, 3, 6, 8],    [6, 4, -1, 9],
        [-1, 7, 10, -1],  [10, 8, 11, -1], [11, 9, -1, -1]
    ];
    
    // 12 interior walls in a 3x3 maze. Start with all the walls up.
    let mut wall_list = [true, true, true, true, true, true, true, true, true, true, true, true];
    
    // Print initial maze
    println!( "Initial Maze:" );
    
    // Print out the maze, this is a rather painful copy/paste job without loops and functions.
    // Note printChar is meant to print a single character with no newlines.
    // Horizontal walls above row 1 - All are exterior walls, no conditions.
    let mut current_interior_wall = 0;
    
    // +-+-+-+
    print!( "{}", '+' );
    print!( "{}", '-' );
    print!( "{}", '+' );
    print!( "{}", '-' );
    print!( "{}", '+' );
    print!( "{}", '-' );
    println!( "{}", '+' );
    
    // Vertical walls and cells row 1.
    // The left and right vertical walls are exterior, always up.
    // | | | |
    print!( "{}", '|' );
    print!( "{}", ' ' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '|' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", ' ' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '|' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", ' ' );
    println!( "{}", '|' );
    
    // Horizontal walls above row 2
    // +-+-+-+
    print!( "{}", '+' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '-' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", '+' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '-' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", '+' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '-' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    println!( "{}", '+' );
    
    // Vertical walls and cells row 2.
    // The left and right vertical walls are exterior, always up.
    // | | | |
    print!( "{}", '|' );
    print!( "{}", ' ' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '|' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", ' ' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '|' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", ' ' );
    println!( "{}", '|' );
    
    // Horizontal walls above row 3
    // +-+-+-+
    print!( "{}", '+' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '-' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", '+' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '-' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", '+' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '-' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    println!( "{}", '+' );
    
    // Vertical walls and cells row 3.
    // The left and right vertical walls are exterior, always up.
    // | | | |
    print!( "{}", '|' );
    print!( "{}", ' ' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '|' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", ' ' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '|' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", ' ' );
    println!( "{}", '|' );
    
    // Horizontal walls below row 3 - All are exterior walls, no conditions.
    // +-+-+-+
    print!( "{}", '+' );
    print!( "{}", '-' );
    print!( "{}", '+' );
    print!( "{}", '-' );
    print!( "{}", '+' );
    print!( "{}", '-' );
    println!( "{}", '+' );
    
    // Now process each cell 0-8
    // Cell 0
    println!( "\nRemoving wall from cell 0:" );
    
    // Remove wall code:
    // Remove a cell wall if possible for cell 0
    let cell_index = 0;
    let mut wall_to_remove: usize = 0;
    
    let mut wall_removed = false;
    let mut direction = LEFT;
    
    // If the wall in this direction is NOT an exterior wall
    if ( cell_to_wall_lut[cell_index][direction] >= 0 ) {
        wall_to_remove = cell_to_wall_lut[cell_index][direction] as usize;
        // If this wall has not been already removed
        if ( wall_list[wall_to_remove] == true ) {
            // Remove the wall and indicate a wall was successfully removed
            wall_list[wall_to_remove] = false;
            wall_removed = true;
        }
    }
    
    // If we haven't removed a wall yet, try to remove one in the next direction
    // Check the guard, have we removed one wall
    if ( wall_removed == false ) {
        direction = direction + 1;
        // If the wall in this direction is NOT an exterior wall
        if ( cell_to_wall_lut[cell_index][direction] >= 0 ) {
            wall_to_remove = cell_to_wall_lut[cell_index][direction] as usize;
            // If this wall has not been already removed
            if ( wall_list[wall_to_remove] == true ) {
                // Remove the wall and indicate a wall was successfully removed
                wall_list[wall_to_remove] = false;
                wall_removed = true;
            }
        }
    }
    
    // If we haven't removed a wall yet, try to remove one in the next direction
    // Check the guard, have we removed one wall
    if ( wall_removed == false ) {
        direction = direction + 1;
        // If the wall in this direction is NOT an exterior wall
        if ( cell_to_wall_lut[cell_index][direction] >= 0 ) {
            wall_to_remove = cell_to_wall_lut[cell_index][direction] as usize;
            // If this wall has not been already removed
            if ( wall_list[wall_to_remove] == true ) {
                // Remove the wall and indicate a wall was successfully removed
                wall_list[wall_to_remove] = false;
                wall_removed = true;
            }
        }
    }
    
    // If we haven't removed a wall yet, try to remove one in the next direction
    // Check the guard, have we removed one wall
    if ( wall_removed == false ) {
        direction = direction + 1;
        // If the wall in this direction is NOT an exterior wall
        if ( cell_to_wall_lut[cell_index][direction] >= 0 ) {
            wall_to_remove = cell_to_wall_lut[cell_index][direction] as usize;
            // If this wall has not been already removed
            if ( wall_list[wall_to_remove] == true ) {
                // Remove the wall and indicate a wall was successfully removed
                wall_list[wall_to_remove] = false;
                wall_removed = true;
            }
        }
    }
    
    // Print maze after cell 0
    thread::sleep( Duration::from_millis( 500 ) );
    
    // Print maze (duplicate of above print code)
    current_interior_wall = 0;
    
    // +-+-+-+
    print!( "{}", '+' );
    print!( "{}", '-' );
    print!( "{}", '+' );
    print!( "{}", '-' );
    print!( "{}", '+' );
    print!( "{}", '-' );
    println!( "{}", '+' );
    
    // Row 1 vertical walls and cells
    print!( "{}", '|' );
    print!( "{}", ' ' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '|' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", ' ' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '|' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", ' ' );
    println!( "{}", '|' );
    
    // Row 2 horizontal walls
    print!( "{}", '+' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '-' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", '+' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '-' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", '+' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '-' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    println!( "{}", '+' );
    
    // Row 2 vertical walls and cells
    print!( "{}", '|' );
    print!( "{}", ' ' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '|' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", ' ' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '|' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", ' ' );
    println!( "{}", '|' );
    
    // Row 3 horizontal walls
    print!( "{}", '+' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '-' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", '+' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '-' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", '+' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '-' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    println!( "{}", '+' );
    
    // Row 3 vertical walls and cells
    print!( "{}", '|' );
    print!( "{}", ' ' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '|' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", ' ' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '|' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", ' ' );
    println!( "{}", '|' );
    
    // Bottom border
    print!( "{}", '+' );
    print!( "{}", '-' );
    print!( "{}", '+' );
    print!( "{}", '-' );
    print!( "{}", '+' );
    print!( "{}", '-' );
    println!( "{}", '+' );

    // Cell 1
    println!( "\nRemoving wall from cell 1:" );
    
    // Remove wall code:
    // Remove a cell wall if possible for cell 1
    let cell_index = 1;
    let mut wall_to_remove: usize = 0;
    
    let mut wall_removed = false;
    let mut direction = LEFT;
    
    // If the wall in this direction is NOT an exterior wall
    if ( cell_to_wall_lut[cell_index][direction] >= 0 ) {
        wall_to_remove = cell_to_wall_lut[cell_index][direction] as usize;
        // If this wall has not been already removed
        if ( wall_list[wall_to_remove] == true ) {
            // Remove the wall and indicate a wall was successfully removed
            wall_list[wall_to_remove] = false;
            wall_removed = true;
        }
    }
    
    // If we haven't removed a wall yet, try to remove one in the next direction
    // Check the guard, have we removed one wall
    if ( wall_removed == false ) {
        direction = direction + 1;
        // If the wall in this direction is NOT an exterior wall
        if ( cell_to_wall_lut[cell_index][direction] >= 0 ) {
            wall_to_remove = cell_to_wall_lut[cell_index][direction] as usize;
            // If this wall has not been already removed
            if ( wall_list[wall_to_remove] == true ) {
                // Remove the wall and indicate a wall was successfully removed
                wall_list[wall_to_remove] = false;
                wall_removed = true;
            }
        }
    }
    
    // If we haven't removed a wall yet, try to remove one in the next direction
    // Check the guard, have we removed one wall
    if ( wall_removed == false ) {
        direction = direction + 1;
        // If the wall in this direction is NOT an exterior wall
        if ( cell_to_wall_lut[cell_index][direction] >= 0 ) {
            wall_to_remove = cell_to_wall_lut[cell_index][direction] as usize;
            // If this wall has not been already removed
            if ( wall_list[wall_to_remove] == true ) {
                // Remove the wall and indicate a wall was successfully removed
                wall_list[wall_to_remove] = false;
                wall_removed = true;
            }
        }
    }
    
    // If we haven't removed a wall yet, try to remove one in the next direction
    // Check the guard, have we removed one wall
    if ( wall_removed == false ) {
        direction = direction + 1;
        // If the wall in this direction is NOT an exterior wall
        if ( cell_to_wall_lut[cell_index][direction] >= 0 ) {
            wall_to_remove = cell_to_wall_lut[cell_index][direction] as usize;
            // If this wall has not been already removed
            if ( wall_list[wall_to_remove] == true ) {
                // Remove the wall and indicate a wall was successfully removed
                wall_list[wall_to_remove] = false;
                wall_removed = true;
            }
        }
    }
    
    // Print maze after cell 1
    thread::sleep( Duration::from_millis( 500 ) );
    
    // Print maze (duplicate of above print code)
    current_interior_wall = 0;
    
    // +-+-+-+
    print!( "{}", '+' );
    print!( "{}", '-' );
    print!( "{}", '+' );
    print!( "{}", '-' );
    print!( "{}", '+' );
    print!( "{}", '-' );
    println!( "{}", '+' );
    
    // Row 1 vertical walls and cells
    print!( "{}", '|' );
    print!( "{}", ' ' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '|' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", ' ' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '|' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", ' ' );
    println!( "{}", '|' );
    
    // Row 2 horizontal walls
    print!( "{}", '+' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '-' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", '+' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '-' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", '+' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '-' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    println!( "{}", '+' );
    
    // Row 2 vertical walls and cells
    print!( "{}", '|' );
    print!( "{}", ' ' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '|' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", ' ' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '|' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", ' ' );
    println!( "{}", '|' );
    
    // Row 3 horizontal walls
    print!( "{}", '+' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '-' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", '+' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '-' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", '+' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '-' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    println!( "{}", '+' );
    
    // Row 3 vertical walls and cells
    print!( "{}", '|' );
    print!( "{}", ' ' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '|' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", ' ' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '|' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", ' ' );
    println!( "{}", '|' );
    
    // Bottom border
    print!( "{}", '+' );
    print!( "{}", '-' );
    print!( "{}", '+' );
    print!( "{}", '-' );
    print!( "{}", '+' );
    print!( "{}", '-' );
    println!( "{}", '+' );

    // Cell 2
    println!( "\nRemoving wall from cell 2:" );
    
    // Remove wall code:
    // Remove a cell wall if possible for cell 2
    let cell_index = 2;
    let mut wall_to_remove: usize = 0;
    
    let mut wall_removed = false;
    let mut direction = LEFT;
    
    // If the wall in this direction is NOT an exterior wall
    if ( cell_to_wall_lut[cell_index][direction] >= 0 ) {
        wall_to_remove = cell_to_wall_lut[cell_index][direction] as usize;
        // If this wall has not been already removed
        if ( wall_list[wall_to_remove] == true ) {
            // Remove the wall and indicate a wall was successfully removed
            wall_list[wall_to_remove] = false;
            wall_removed = true;
        }
    }
    
    // If we haven't removed a wall yet, try to remove one in the next direction
    // Check the guard, have we removed one wall
    if ( wall_removed == false ) {
        direction = direction + 1;
        // If the wall in this direction is NOT an exterior wall
        if ( cell_to_wall_lut[cell_index][direction] >= 0 ) {
            wall_to_remove = cell_to_wall_lut[cell_index][direction] as usize;
            // If this wall has not been already removed
            if ( wall_list[wall_to_remove] == true ) {
                // Remove the wall and indicate a wall was successfully removed
                wall_list[wall_to_remove] = false;
                wall_removed = true;
            }
        }
    }
    
    // If we haven't removed a wall yet, try to remove one in the next direction
    // Check the guard, have we removed one wall
    if ( wall_removed == false ) {
        direction = direction + 1;
        // If the wall in this direction is NOT an exterior wall
        if ( cell_to_wall_lut[cell_index][direction] >= 0 ) {
            wall_to_remove = cell_to_wall_lut[cell_index][direction] as usize;
            // If this wall has not been already removed
            if ( wall_list[wall_to_remove] == true ) {
                // Remove the wall and indicate a wall was successfully removed
                wall_list[wall_to_remove] = false;
                wall_removed = true;
            }
        }
    }
    
    // If we haven't removed a wall yet, try to remove one in the next direction
    // Check the guard, have we removed one wall
    if ( wall_removed == false ) {
        direction = direction + 1;
        // If the wall in this direction is NOT an exterior wall
        if ( cell_to_wall_lut[cell_index][direction] >= 0 ) {
            wall_to_remove = cell_to_wall_lut[cell_index][direction] as usize;
            // If this wall has not been already removed
            if ( wall_list[wall_to_remove] == true ) {
                // Remove the wall and indicate a wall was successfully removed
                wall_list[wall_to_remove] = false;
                wall_removed = true;
            }
        }
    }
    
    // Print maze after cell 2
    thread::sleep( Duration::from_millis( 500 ) );
    
    // Print maze (duplicate of above print code)
    current_interior_wall = 0;
    
    // +-+-+-+
    print!( "{}", '+' );
    print!( "{}", '-' );
    print!( "{}", '+' );
    print!( "{}", '-' );
    print!( "{}", '+' );
    print!( "{}", '-' );
    println!( "{}", '+' );
    
    // Row 1 vertical walls and cells
    print!( "{}", '|' );
    print!( "{}", ' ' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '|' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", ' ' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '|' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", ' ' );
    println!( "{}", '|' );
    
    // Row 2 horizontal walls
    print!( "{}", '+' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '-' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", '+' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '-' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", '+' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '-' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    println!( "{}", '+' );
    
    // Row 2 vertical walls and cells
    print!( "{}", '|' );
    print!( "{}", ' ' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '|' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", ' ' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '|' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", ' ' );
    println!( "{}", '|' );
    
    // Row 3 horizontal walls
    print!( "{}", '+' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '-' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", '+' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '-' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", '+' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '-' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    println!( "{}", '+' );
    
    // Row 3 vertical walls and cells
    print!( "{}", '|' );
    print!( "{}", ' ' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '|' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", ' ' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '|' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", ' ' );
    println!( "{}", '|' );
    
    // Bottom border
    print!( "{}", '+' );
    print!( "{}", '-' );
    print!( "{}", '+' );
    print!( "{}", '-' );
    print!( "{}", '+' );
    print!( "{}", '-' );
    println!( "{}", '+' );

    // Cell 3
    println!( "\nRemoving wall from cell 3:" );
    
    // Remove wall code:
    // Remove a cell wall if possible for cell 3
    let cell_index = 3;
    let mut wall_to_remove: usize = 0;
    
    let mut wall_removed = false;
    let mut direction = LEFT;
    
    // If the wall in this direction is NOT an exterior wall
    if ( cell_to_wall_lut[cell_index][direction] >= 0 ) {
        wall_to_remove = cell_to_wall_lut[cell_index][direction] as usize;
        // If this wall has not been already removed
        if ( wall_list[wall_to_remove] == true ) {
            // Remove the wall and indicate a wall was successfully removed
            wall_list[wall_to_remove] = false;
            wall_removed = true;
        }
    }
    
    // If we haven't removed a wall yet, try to remove one in the next direction
    // Check the guard, have we removed one wall
    if ( wall_removed == false ) {
        direction = direction + 1;
        // If the wall in this direction is NOT an exterior wall
        if ( cell_to_wall_lut[cell_index][direction] >= 0 ) {
            wall_to_remove = cell_to_wall_lut[cell_index][direction] as usize;
            // If this wall has not been already removed
            if ( wall_list[wall_to_remove] == true ) {
                // Remove the wall and indicate a wall was successfully removed
                wall_list[wall_to_remove] = false;
                wall_removed = true;
            }
        }
    }
    
    // If we haven't removed a wall yet, try to remove one in the next direction
    // Check the guard, have we removed one wall
    if ( wall_removed == false ) {
        direction = direction + 1;
        // If the wall in this direction is NOT an exterior wall
        if ( cell_to_wall_lut[cell_index][direction] >= 0 ) {
            wall_to_remove = cell_to_wall_lut[cell_index][direction] as usize;
            // If this wall has not been already removed
            if ( wall_list[wall_to_remove] == true ) {
                // Remove the wall and indicate a wall was successfully removed
                wall_list[wall_to_remove] = false;
                wall_removed = true;
            }
        }
    }
    
    // If we haven't removed a wall yet, try to remove one in the next direction
    // Check the guard, have we removed one wall
    if ( wall_removed == false ) {
        direction = direction + 1;
        // If the wall in this direction is NOT an exterior wall
        if ( cell_to_wall_lut[cell_index][direction] >= 0 ) {
            wall_to_remove = cell_to_wall_lut[cell_index][direction] as usize;
            // If this wall has not been already removed
            if ( wall_list[wall_to_remove] == true ) {
                // Remove the wall and indicate a wall was successfully removed
                wall_list[wall_to_remove] = false;
                wall_removed = true;
            }
        }
    }
    
    // Print maze after cell 3
    thread::sleep( Duration::from_millis( 500 ) );
    
    // Print maze (duplicate of above print code)
    current_interior_wall = 0;
    
    // +-+-+-+
    print!( "{}", '+' );
    print!( "{}", '-' );
    print!( "{}", '+' );
    print!( "{}", '-' );
    print!( "{}", '+' );
    print!( "{}", '-' );
    println!( "{}", '+' );
    
    // Row 1 vertical walls and cells
    print!( "{}", '|' );
    print!( "{}", ' ' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '|' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", ' ' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '|' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", ' ' );
    println!( "{}", '|' );
    
    // Row 2 horizontal walls
    print!( "{}", '+' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '-' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", '+' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '-' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", '+' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '-' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    println!( "{}", '+' );
    
    // Row 2 vertical walls and cells
    print!( "{}", '|' );
    print!( "{}", ' ' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '|' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", ' ' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '|' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", ' ' );
    println!( "{}", '|' );
    
    // Row 3 horizontal walls
    print!( "{}", '+' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '-' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", '+' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '-' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", '+' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '-' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    println!( "{}", '+' );
    
    // Row 3 vertical walls and cells
    print!( "{}", '|' );
    print!( "{}", ' ' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '|' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", ' ' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '|' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", ' ' );
    println!( "{}", '|' );
    
    // Bottom border
    print!( "{}", '+' );
    print!( "{}", '-' );
    print!( "{}", '+' );
    print!( "{}", '-' );
    print!( "{}", '+' );
    print!( "{}", '-' );
    println!( "{}", '+' );

    // Cell 4
    println!( "\nRemoving wall from cell 4:" );
    
    // Remove wall code:
    // Remove a cell wall if possible for cell 4
    let cell_index = 4;
    let mut wall_to_remove: usize = 0;
    
    let mut wall_removed = false;
    let mut direction = LEFT;
    
    // If the wall in this direction is NOT an exterior wall
    if ( cell_to_wall_lut[cell_index][direction] >= 0 ) {
        wall_to_remove = cell_to_wall_lut[cell_index][direction] as usize;
        // If this wall has not been already removed
        if ( wall_list[wall_to_remove] == true ) {
            // Remove the wall and indicate a wall was successfully removed
            wall_list[wall_to_remove] = false;
            wall_removed = true;
        }
    }
    
    // If we haven't removed a wall yet, try to remove one in the next direction
    // Check the guard, have we removed one wall
    if ( wall_removed == false ) {
        direction = direction + 1;
        // If the wall in this direction is NOT an exterior wall
        if ( cell_to_wall_lut[cell_index][direction] >= 0 ) {
            wall_to_remove = cell_to_wall_lut[cell_index][direction] as usize;
            // If this wall has not been already removed
            if ( wall_list[wall_to_remove] == true ) {
                // Remove the wall and indicate a wall was successfully removed
                wall_list[wall_to_remove] = false;
                wall_removed = true;
            }
        }
    }
    
    // If we haven't removed a wall yet, try to remove one in the next direction
    // Check the guard, have we removed one wall
    if ( wall_removed == false ) {
        direction = direction + 1;
        // If the wall in this direction is NOT an exterior wall
        if ( cell_to_wall_lut[cell_index][direction] >= 0 ) {
            wall_to_remove = cell_to_wall_lut[cell_index][direction] as usize;
            // If this wall has not been already removed
            if ( wall_list[wall_to_remove] == true ) {
                // Remove the wall and indicate a wall was successfully removed
                wall_list[wall_to_remove] = false;
                wall_removed = true;
            }
        }
    }
    
    // If we haven't removed a wall yet, try to remove one in the next direction
    // Check the guard, have we removed one wall
    if ( wall_removed == false ) {
        direction = direction + 1;
        // If the wall in this direction is NOT an exterior wall
        if ( cell_to_wall_lut[cell_index][direction] >= 0 ) {
            wall_to_remove = cell_to_wall_lut[cell_index][direction] as usize;
            // If this wall has not been already removed
            if ( wall_list[wall_to_remove] == true ) {
                // Remove the wall and indicate a wall was successfully removed
                wall_list[wall_to_remove] = false;
                wall_removed = true;
            }
        }
    }
    
    // Print maze after cell 4
    thread::sleep( Duration::from_millis( 500 ) );
    
    // Print maze (duplicate of above print code)
    current_interior_wall = 0;
    
    // +-+-+-+
    print!( "{}", '+' );
    print!( "{}", '-' );
    print!( "{}", '+' );
    print!( "{}", '-' );
    print!( "{}", '+' );
    print!( "{}", '-' );
    println!( "{}", '+' );
    
    // Row 1 vertical walls and cells
    print!( "{}", '|' );
    print!( "{}", ' ' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '|' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", ' ' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '|' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", ' ' );
    println!( "{}", '|' );
    
    // Row 2 horizontal walls
    print!( "{}", '+' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '-' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", '+' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '-' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", '+' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '-' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    println!( "{}", '+' );
    
    // Row 2 vertical walls and cells
    print!( "{}", '|' );
    print!( "{}", ' ' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '|' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", ' ' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '|' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", ' ' );
    println!( "{}", '|' );
    
    // Row 3 horizontal walls
    print!( "{}", '+' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '-' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", '+' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '-' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", '+' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '-' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    println!( "{}", '+' );
    
    // Row 3 vertical walls and cells
    print!( "{}", '|' );
    print!( "{}", ' ' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '|' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", ' ' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '|' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", ' ' );
    println!( "{}", '|' );
    
    // Bottom border
    print!( "{}", '+' );
    print!( "{}", '-' );
    print!( "{}", '+' );
    print!( "{}", '-' );
    print!( "{}", '+' );
    print!( "{}", '-' );
    println!( "{}", '+' );

    // Cell 5
    println!( "\nRemoving wall from cell 5:" );
    
    // Remove wall code:
    // Remove a cell wall if possible for cell 5
    let cell_index = 5;
    let mut wall_to_remove: usize = 0;
    
    let mut wall_removed = false;
    let mut direction = LEFT;
    
    // If the wall in this direction is NOT an exterior wall
    if ( cell_to_wall_lut[cell_index][direction] >= 0 ) {
        wall_to_remove = cell_to_wall_lut[cell_index][direction] as usize;
        // If this wall has not been already removed
        if ( wall_list[wall_to_remove] == true ) {
            // Remove the wall and indicate a wall was successfully removed
            wall_list[wall_to_remove] = false;
            wall_removed = true;
        }
    }
    
    // If we haven't removed a wall yet, try to remove one in the next direction
    // Check the guard, have we removed one wall
    if ( wall_removed == false ) {
        direction = direction + 1;
        // If the wall in this direction is NOT an exterior wall
        if ( cell_to_wall_lut[cell_index][direction] >= 0 ) {
            wall_to_remove = cell_to_wall_lut[cell_index][direction] as usize;
            // If this wall has not been already removed
            if ( wall_list[wall_to_remove] == true ) {
                // Remove the wall and indicate a wall was successfully removed
                wall_list[wall_to_remove] = false;
                wall_removed = true;
            }
        }
    }
    
    // If we haven't removed a wall yet, try to remove one in the next direction
    // Check the guard, have we removed one wall
    if ( wall_removed == false ) {
        direction = direction + 1;
        // If the wall in this direction is NOT an exterior wall
        if ( cell_to_wall_lut[cell_index][direction] >= 0 ) {
            wall_to_remove = cell_to_wall_lut[cell_index][direction] as usize;
            // If this wall has not been already removed
            if ( wall_list[wall_to_remove] == true ) {
                // Remove the wall and indicate a wall was successfully removed
                wall_list[wall_to_remove] = false;
                wall_removed = true;
            }
        }
    }
    
    // If we haven't removed a wall yet, try to remove one in the next direction
    // Check the guard, have we removed one wall
    if ( wall_removed == false ) {
        direction = direction + 1;
        // If the wall in this direction is NOT an exterior wall
        if ( cell_to_wall_lut[cell_index][direction] >= 0 ) {
            wall_to_remove = cell_to_wall_lut[cell_index][direction] as usize;
            // If this wall has not been already removed
            if ( wall_list[wall_to_remove] == true ) {
                // Remove the wall and indicate a wall was successfully removed
                wall_list[wall_to_remove] = false;
                wall_removed = true;
            }
        }
    }
    
    // Print maze after cell 5
    thread::sleep( Duration::from_millis( 500 ) );
    
    // Print maze (duplicate of above print code)
    current_interior_wall = 0;
    
    // +-+-+-+
    print!( "{}", '+' );
    print!( "{}", '-' );
    print!( "{}", '+' );
    print!( "{}", '-' );
    print!( "{}", '+' );
    print!( "{}", '-' );
    println!( "{}", '+' );
    
    // Row 1 vertical walls and cells
    print!( "{}", '|' );
    print!( "{}", ' ' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '|' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", ' ' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '|' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", ' ' );
    println!( "{}", '|' );
    
    // Row 2 horizontal walls
    print!( "{}", '+' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '-' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", '+' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '-' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", '+' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '-' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    println!( "{}", '+' );
    
    // Row 2 vertical walls and cells
    print!( "{}", '|' );
    print!( "{}", ' ' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '|' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", ' ' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '|' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", ' ' );
    println!( "{}", '|' );
    
    // Row 3 horizontal walls
    print!( "{}", '+' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '-' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", '+' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '-' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", '+' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '-' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    println!( "{}", '+' );
    
    // Row 3 vertical walls and cells
    print!( "{}", '|' );
    print!( "{}", ' ' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '|' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", ' ' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '|' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", ' ' );
    println!( "{}", '|' );
    
    // Bottom border
    print!( "{}", '+' );
    print!( "{}", '-' );
    print!( "{}", '+' );
    print!( "{}", '-' );
    print!( "{}", '+' );
    print!( "{}", '-' );
    println!( "{}", '+' );

    // Cell 6
    println!( "\nRemoving wall from cell 6:" );
    
    // Remove wall code:
    // Remove a cell wall if possible for cell 6
    let cell_index = 6;
    let mut wall_to_remove: usize = 0;
    
    let mut wall_removed = false;
    let mut direction = LEFT;
    
    // If the wall in this direction is NOT an exterior wall
    if ( cell_to_wall_lut[cell_index][direction] >= 0 ) {
        wall_to_remove = cell_to_wall_lut[cell_index][direction] as usize;
        // If this wall has not been already removed
        if ( wall_list[wall_to_remove] == true ) {
            // Remove the wall and indicate a wall was successfully removed
            wall_list[wall_to_remove] = false;
            wall_removed = true;
        }
    }
    
    // If we haven't removed a wall yet, try to remove one in the next direction
    // Check the guard, have we removed one wall
    if ( wall_removed == false ) {
        direction = direction + 1;
        // If the wall in this direction is NOT an exterior wall
        if ( cell_to_wall_lut[cell_index][direction] >= 0 ) {
            wall_to_remove = cell_to_wall_lut[cell_index][direction] as usize;
            // If this wall has not been already removed
            if ( wall_list[wall_to_remove] == true ) {
                // Remove the wall and indicate a wall was successfully removed
                wall_list[wall_to_remove] = false;
                wall_removed = true;
            }
        }
    }
    
    // If we haven't removed a wall yet, try to remove one in the next direction
    // Check the guard, have we removed one wall
    if ( wall_removed == false ) {
        direction = direction + 1;
        // If the wall in this direction is NOT an exterior wall
        if ( cell_to_wall_lut[cell_index][direction] >= 0 ) {
            wall_to_remove = cell_to_wall_lut[cell_index][direction] as usize;
            // If this wall has not been already removed
            if ( wall_list[wall_to_remove] == true ) {
                // Remove the wall and indicate a wall was successfully removed
                wall_list[wall_to_remove] = false;
                wall_removed = true;
            }
        }
    }
    
    // If we haven't removed a wall yet, try to remove one in the next direction
    // Check the guard, have we removed one wall
    if ( wall_removed == false ) {
        direction = direction + 1;
        // If the wall in this direction is NOT an exterior wall
        if ( cell_to_wall_lut[cell_index][direction] >= 0 ) {
            wall_to_remove = cell_to_wall_lut[cell_index][direction] as usize;
            // If this wall has not been already removed
            if ( wall_list[wall_to_remove] == true ) {
                // Remove the wall and indicate a wall was successfully removed
                wall_list[wall_to_remove] = false;
                wall_removed = true;
            }
        }
    }
    
    // Print maze after cell 6
    thread::sleep( Duration::from_millis( 500 ) );
    
    // Print maze (duplicate of above print code)
    current_interior_wall = 0;
    
    // +-+-+-+
    print!( "{}", '+' );
    print!( "{}", '-' );
    print!( "{}", '+' );
    print!( "{}", '-' );
    print!( "{}", '+' );
    print!( "{}", '-' );
    println!( "{}", '+' );
    
    // Row 1 vertical walls and cells
    print!( "{}", '|' );
    print!( "{}", ' ' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '|' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", ' ' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '|' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", ' ' );
    println!( "{}", '|' );
    
    // Row 2 horizontal walls
    print!( "{}", '+' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '-' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", '+' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '-' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", '+' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '-' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    println!( "{}", '+' );
    
    // Row 2 vertical walls and cells
    print!( "{}", '|' );
    print!( "{}", ' ' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '|' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", ' ' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '|' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", ' ' );
    println!( "{}", '|' );
    
    // Row 3 horizontal walls
    print!( "{}", '+' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '-' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", '+' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '-' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", '+' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '-' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    println!( "{}", '+' );
    
    // Row 3 vertical walls and cells
    print!( "{}", '|' );
    print!( "{}", ' ' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '|' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", ' ' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '|' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", ' ' );
    println!( "{}", '|' );
    
    // Bottom border
    print!( "{}", '+' );
    print!( "{}", '-' );
    print!( "{}", '+' );
    print!( "{}", '-' );
    print!( "{}", '+' );
    print!( "{}", '-' );
    println!( "{}", '+' );

    // Cell 7
    println!( "\nRemoving wall from cell 7:" );
    
    // Remove wall code:
    // Remove a cell wall if possible for cell 7
    let cell_index = 7;
    let mut wall_to_remove: usize = 0;
    
    let mut wall_removed = false;
    let mut direction = LEFT;
    
    // If the wall in this direction is NOT an exterior wall
    if ( cell_to_wall_lut[cell_index][direction] >= 0 ) {
        wall_to_remove = cell_to_wall_lut[cell_index][direction] as usize;
        // If this wall has not been already removed
        if ( wall_list[wall_to_remove] == true ) {
            // Remove the wall and indicate a wall was successfully removed
            wall_list[wall_to_remove] = false;
            wall_removed = true;
        }
    }
    
    // If we haven't removed a wall yet, try to remove one in the next direction
    // Check the guard, have we removed one wall
    if ( wall_removed == false ) {
        direction = direction + 1;
        // If the wall in this direction is NOT an exterior wall
        if ( cell_to_wall_lut[cell_index][direction] >= 0 ) {
            wall_to_remove = cell_to_wall_lut[cell_index][direction] as usize;
            // If this wall has not been already removed
            if ( wall_list[wall_to_remove] == true ) {
                // Remove the wall and indicate a wall was successfully removed
                wall_list[wall_to_remove] = false;
                wall_removed = true;
            }
        }
    }
    
    // If we haven't removed a wall yet, try to remove one in the next direction
    // Check the guard, have we removed one wall
    if ( wall_removed == false ) {
        direction = direction + 1;
        // If the wall in this direction is NOT an exterior wall
        if ( cell_to_wall_lut[cell_index][direction] >= 0 ) {
            wall_to_remove = cell_to_wall_lut[cell_index][direction] as usize;
            // If this wall has not been already removed
            if ( wall_list[wall_to_remove] == true ) {
                // Remove the wall and indicate a wall was successfully removed
                wall_list[wall_to_remove] = false;
                wall_removed = true;
            }
        }
    }
    
    // If we haven't removed a wall yet, try to remove one in the next direction
    // Check the guard, have we removed one wall
    if ( wall_removed == false ) {
        direction = direction + 1;
        // If the wall in this direction is NOT an exterior wall
        if ( cell_to_wall_lut[cell_index][direction] >= 0 ) {
            wall_to_remove = cell_to_wall_lut[cell_index][direction] as usize;
            // If this wall has not been already removed
            if ( wall_list[wall_to_remove] == true ) {
                // Remove the wall and indicate a wall was successfully removed
                wall_list[wall_to_remove] = false;
                wall_removed = true;
            }
        }
    }
    
    // Print maze after cell 7
    thread::sleep( Duration::from_millis( 500 ) );
    
    // Print maze (duplicate of above print code)
    current_interior_wall = 0;
    
    // +-+-+-+
    print!( "{}", '+' );
    print!( "{}", '-' );
    print!( "{}", '+' );
    print!( "{}", '-' );
    print!( "{}", '+' );
    print!( "{}", '-' );
    println!( "{}", '+' );
    
    // Row 1 vertical walls and cells
    print!( "{}", '|' );
    print!( "{}", ' ' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '|' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", ' ' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '|' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", ' ' );
    println!( "{}", '|' );
    
    // Row 2 horizontal walls
    print!( "{}", '+' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '-' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", '+' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '-' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", '+' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '-' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    println!( "{}", '+' );
    
    // Row 2 vertical walls and cells
    print!( "{}", '|' );
    print!( "{}", ' ' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '|' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", ' ' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '|' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", ' ' );
    println!( "{}", '|' );
    
    // Row 3 horizontal walls
    print!( "{}", '+' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '-' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", '+' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '-' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", '+' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '-' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    println!( "{}", '+' );
    
    // Row 3 vertical walls and cells
    print!( "{}", '|' );
    print!( "{}", ' ' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '|' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", ' ' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '|' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", ' ' );
    println!( "{}", '|' );
    
    // Bottom border
    print!( "{}", '+' );
    print!( "{}", '-' );
    print!( "{}", '+' );
    print!( "{}", '-' );
    print!( "{}", '+' );
    print!( "{}", '-' );
    println!( "{}", '+' );

    // Cell 8
    println!( "\nRemoving wall from cell 8:" );
    
    // Remove wall code:
    // Remove a cell wall if possible for cell 8
    let cell_index = 8;
    let mut wall_to_remove: usize = 0;
    
    let mut wall_removed = false;
    let mut direction = LEFT;
    
    // If the wall in this direction is NOT an exterior wall
    if ( cell_to_wall_lut[cell_index][direction] >= 0 ) {
        wall_to_remove = cell_to_wall_lut[cell_index][direction] as usize;
        // If this wall has not been already removed
        if ( wall_list[wall_to_remove] == true ) {
            // Remove the wall and indicate a wall was successfully removed
            wall_list[wall_to_remove] = false;
            wall_removed = true;
        }
    }
    
    // If we haven't removed a wall yet, try to remove one in the next direction
    // Check the guard, have we removed one wall
    if ( wall_removed == false ) {
        direction = direction + 1;
        // If the wall in this direction is NOT an exterior wall
        if ( cell_to_wall_lut[cell_index][direction] >= 0 ) {
            wall_to_remove = cell_to_wall_lut[cell_index][direction] as usize;
            // If this wall has not been already removed
            if ( wall_list[wall_to_remove] == true ) {
                // Remove the wall and indicate a wall was successfully removed
                wall_list[wall_to_remove] = false;
                wall_removed = true;
            }
        }
    }
    
    // If we haven't removed a wall yet, try to remove one in the next direction
    // Check the guard, have we removed one wall
    if ( wall_removed == false ) {
        direction = direction + 1;
        // If the wall in this direction is NOT an exterior wall
        if ( cell_to_wall_lut[cell_index][direction] >= 0 ) {
            wall_to_remove = cell_to_wall_lut[cell_index][direction] as usize;
            // If this wall has not been already removed
            if ( wall_list[wall_to_remove] == true ) {
                // Remove the wall and indicate a wall was successfully removed
                wall_list[wall_to_remove] = false;
                wall_removed = true;
            }
        }
    }
    
    // If we haven't removed a wall yet, try to remove one in the next direction
    // Check the guard, have we removed one wall
    if ( wall_removed == false ) {
        direction = direction + 1;
        // If the wall in this direction is NOT an exterior wall
        if ( cell_to_wall_lut[cell_index][direction] >= 0 ) {
            wall_to_remove = cell_to_wall_lut[cell_index][direction] as usize;
            // If this wall has not been already removed
            if ( wall_list[wall_to_remove] == true ) {
                // Remove the wall and indicate a wall was successfully removed
                wall_list[wall_to_remove] = false;
                wall_removed = true;
            }
        }
    }
    
    // Print maze after cell 8
    thread::sleep( Duration::from_millis( 500 ) );
    
    // Print maze (duplicate of above print code)
    current_interior_wall = 0;
    
    // +-+-+-+
    print!( "{}", '+' );
    print!( "{}", '-' );
    print!( "{}", '+' );
    print!( "{}", '-' );
    print!( "{}", '+' );
    print!( "{}", '-' );
    println!( "{}", '+' );
    
    // Row 1 vertical walls and cells
    print!( "{}", '|' );
    print!( "{}", ' ' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '|' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", ' ' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '|' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", ' ' );
    println!( "{}", '|' );
    
    // Row 2 horizontal walls
    print!( "{}", '+' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '-' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", '+' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '-' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", '+' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '-' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    println!( "{}", '+' );
    
    // Row 2 vertical walls and cells
    print!( "{}", '|' );
    print!( "{}", ' ' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '|' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", ' ' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '|' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", ' ' );
    println!( "{}", '|' );
    
    // Row 3 horizontal walls
    print!( "{}", '+' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '-' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", '+' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '-' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", '+' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '-' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    println!( "{}", '+' );
    
    // Row 3 vertical walls and cells
    print!( "{}", '|' );
    print!( "{}", ' ' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '|' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", ' ' );
    if ( wall_list[current_interior_wall] == true ) {
        print!( "{}", '|' );
    } else {
        print!( "{}", ' ' );
    }
    current_interior_wall = current_interior_wall + 1;
    print!( "{}", ' ' );
    println!( "{}", '|' );
    
    // Bottom border
    print!( "{}", '+' );
    print!( "{}", '-' );
    print!( "{}", '+' );
    print!( "{}", '-' );
    print!( "{}", '+' );
    print!( "{}", '-' );
    println!( "{}", '+' );

}
