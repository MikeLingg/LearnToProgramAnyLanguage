package main

import (
	"fmt"
	"time"
)

func main() {
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
	left := 0
	//up := 1
	//right := 2
	//down := 3

	// For each cell 0-8, indicate if a wall is exterior and cannot be removed (-1) or its interior index for each of
	// the four directions, LEFT, UP, RIGHT, DOWN.
	cellToWallLUT := [9][4]int{
		{-1, -1, 0, 2}, {0, -1, 1, 3}, {1, -1, -1, 4},
		{-1, 2, 5, 7}, {5, 3, 6, 8}, {6, 4, -1, 9},
		{-1, 7, 10, -1}, {10, 8, 11, -1}, {11, 9, -1, -1},
	}

	// 12 interior walls in a 3x3 maze. Start with all the walls up.
	wallList := [12]bool{true, true, true, true, true, true, true, true, true, true, true, true}

	// Print initial maze
	fmt.Printf("Initial Maze:\n")

	// Print out the maze, this is a rather painful copy/paste job without loops and functions.
	// Note printChar is meant to print a single character with no newlines.
	// Horizontal walls above row 1 - All are exterior walls, no conditions.
	currentInteriorWall := 0

	// +-+-+-+
	fmt.Printf("%c", '+')
	fmt.Printf("%c", '-')
	fmt.Printf("%c", '+')
	fmt.Printf("%c", '-')
	fmt.Printf("%c", '+')
	fmt.Printf("%c", '-')
	fmt.Printf("%c", '+')
	fmt.Printf("\n")

	// Vertical walls and cells row 1.
	// The left and right vertical walls are exterior, always up.
	// | | | |
	fmt.Printf("%c", '|')
	fmt.Printf("%c", ' ')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '|')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", ' ')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '|')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", ' ')
	fmt.Printf("%c", '|')
	fmt.Printf("\n")

	// Horizontal walls above row 2
	// +-+-+-+
	fmt.Printf("%c", '+')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '-')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", '+')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '-')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", '+')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '-')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", '+')
	fmt.Printf("\n")

	// Vertical walls and cells row 2.
	// The left and right vertical walls are exterior, always up.
	// | | | |
	fmt.Printf("%c", '|')
	fmt.Printf("%c", ' ')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '|')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", ' ')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '|')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", ' ')
	fmt.Printf("%c", '|')
	fmt.Printf("\n")

	// Horizontal walls above row 3
	// +-+-+-+
	fmt.Printf("%c", '+')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '-')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", '+')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '-')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", '+')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '-')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", '+')
	fmt.Printf("\n")

	// Vertical walls and cells row 3.
	// The left and right vertical walls are exterior, always up.
	// | | | |
	fmt.Printf("%c", '|')
	fmt.Printf("%c", ' ')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '|')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", ' ')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '|')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", ' ')
	fmt.Printf("%c", '|')
	fmt.Printf("\n")

	// Horizontal walls below row 3 - All are exterior walls, no conditions.
	// +-+-+-+
	fmt.Printf("%c", '+')
	fmt.Printf("%c", '-')
	fmt.Printf("%c", '+')
	fmt.Printf("%c", '-')
	fmt.Printf("%c", '+')
	fmt.Printf("%c", '-')
	fmt.Printf("%c", '+')
	fmt.Printf("\n")

	// Now process each cell 0-8
	// Cell 0
	fmt.Printf("\nRemoving wall from cell 0:\n")

	// Remove wall code:
	// Remove a cell wall if possible for cell 0
	cellIndex := 0
	var wallToRemove int

	wallRemoved := false
	direction := left

	// If the wall in this direction is NOT an exterior wall
	if cellToWallLUT[cellIndex][direction] >= 0 {
		wallToRemove = cellToWallLUT[cellIndex][direction]
		// If this wall has not been already removed
		if wallList[wallToRemove] == true {
			// Remove the wall and indicate a wall was successfully removed
			wallList[wallToRemove] = false
			wallRemoved = true
		}
	}

	// If we haven't removed a wall yet, try to remove one in the next direction
	// Check the guard, have we removed one wall
	if wallRemoved == false {
		direction = direction + 1
		// If the wall in this direction is NOT an exterior wall
		if cellToWallLUT[cellIndex][direction] >= 0 {
			wallToRemove = cellToWallLUT[cellIndex][direction]
			// If this wall has not been already removed
			if wallList[wallToRemove] == true {
				// Remove the wall and indicate a wall was successfully removed
				wallList[wallToRemove] = false
				wallRemoved = true
			}
		}
	}

	// If we haven't removed a wall yet, try to remove one in the next direction
	// Check the guard, have we removed one wall
	if wallRemoved == false {
		direction = direction + 1
		// If the wall in this direction is NOT an exterior wall
		if cellToWallLUT[cellIndex][direction] >= 0 {
			wallToRemove = cellToWallLUT[cellIndex][direction]
			// If this wall has not been already removed
			if wallList[wallToRemove] == true {
				// Remove the wall and indicate a wall was successfully removed
				wallList[wallToRemove] = false
				wallRemoved = true
			}
		}
	}

	// If we haven't removed a wall yet, try to remove one in the next direction
	// Check the guard, have we removed one wall
	if wallRemoved == false {
		direction = direction + 1
		// If the wall in this direction is NOT an exterior wall
		if cellToWallLUT[cellIndex][direction] >= 0 {
			wallToRemove = cellToWallLUT[cellIndex][direction]
			// If this wall has not been already removed
			if wallList[wallToRemove] == true {
				// Remove the wall and indicate a wall was successfully removed
				wallList[wallToRemove] = false
				wallRemoved = true
			}
		}
	}

	// Print maze after cell 0
	time.Sleep(500 * time.Millisecond)

	// Print maze (duplicate of above print code)
	currentInteriorWall = 0

	// +-+-+-+
	fmt.Printf("%c", '+')
	fmt.Printf("%c", '-')
	fmt.Printf("%c", '+')
	fmt.Printf("%c", '-')
	fmt.Printf("%c", '+')
	fmt.Printf("%c", '-')
	fmt.Printf("%c", '+')
	fmt.Printf("\n")

	// Row 1 vertical walls and cells
	fmt.Printf("%c", '|')
	fmt.Printf("%c", ' ')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '|')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", ' ')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '|')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", ' ')
	fmt.Printf("%c", '|')
	fmt.Printf("\n")

	// Row 2 horizontal walls
	fmt.Printf("%c", '+')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '-')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", '+')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '-')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", '+')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '-')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", '+')
	fmt.Printf("\n")

	// Row 2 vertical walls and cells
	fmt.Printf("%c", '|')
	fmt.Printf("%c", ' ')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '|')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", ' ')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '|')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", ' ')
	fmt.Printf("%c", '|')
	fmt.Printf("\n")

	// Row 3 horizontal walls
	fmt.Printf("%c", '+')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '-')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", '+')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '-')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", '+')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '-')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", '+')
	fmt.Printf("\n")

	// Row 3 vertical walls and cells
	fmt.Printf("%c", '|')
	fmt.Printf("%c", ' ')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '|')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", ' ')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '|')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", ' ')
	fmt.Printf("%c", '|')
	fmt.Printf("\n")

	// Bottom border
	fmt.Printf("%c", '+')
	fmt.Printf("%c", '-')
	fmt.Printf("%c", '+')
	fmt.Printf("%c", '-')
	fmt.Printf("%c", '+')
	fmt.Printf("%c", '-')
	fmt.Printf("%c", '+')
	fmt.Printf("\n")

	// Cell 1
	fmt.Printf("\nRemoving wall from cell 1:\n")

	// Remove wall code:
	// Remove a cell wall if possible for cell 1
	cellIndex = 1

	wallRemoved = false
	direction = left

	// If the wall in this direction is NOT an exterior wall
	if cellToWallLUT[cellIndex][direction] >= 0 {
		wallToRemove = cellToWallLUT[cellIndex][direction]
		// If this wall has not been already removed
		if wallList[wallToRemove] == true {
			// Remove the wall and indicate a wall was successfully removed
			wallList[wallToRemove] = false
			wallRemoved = true
		}
	}

	// If we haven't removed a wall yet, try to remove one in the next direction
	// Check the guard, have we removed one wall
	if wallRemoved == false {
		direction = direction + 1
		// If the wall in this direction is NOT an exterior wall
		if cellToWallLUT[cellIndex][direction] >= 0 {
			wallToRemove = cellToWallLUT[cellIndex][direction]
			// If this wall has not been already removed
			if wallList[wallToRemove] == true {
				// Remove the wall and indicate a wall was successfully removed
				wallList[wallToRemove] = false
				wallRemoved = true
			}
		}
	}

	// If we haven't removed a wall yet, try to remove one in the next direction
	// Check the guard, have we removed one wall
	if wallRemoved == false {
		direction = direction + 1
		// If the wall in this direction is NOT an exterior wall
		if cellToWallLUT[cellIndex][direction] >= 0 {
			wallToRemove = cellToWallLUT[cellIndex][direction]
			// If this wall has not been already removed
			if wallList[wallToRemove] == true {
				// Remove the wall and indicate a wall was successfully removed
				wallList[wallToRemove] = false
				wallRemoved = true
			}
		}
	}

	// If we haven't removed a wall yet, try to remove one in the next direction
	// Check the guard, have we removed one wall
	if wallRemoved == false {
		direction = direction + 1
		// If the wall in this direction is NOT an exterior wall
		if cellToWallLUT[cellIndex][direction] >= 0 {
			wallToRemove = cellToWallLUT[cellIndex][direction]
			// If this wall has not been already removed
			if wallList[wallToRemove] == true {
				// Remove the wall and indicate a wall was successfully removed
				wallList[wallToRemove] = false
				wallRemoved = true
			}
		}
	}

	// Print maze after cell 1
	time.Sleep(500 * time.Millisecond)

	// Print maze (duplicate of above print code)
	currentInteriorWall = 0

	// +-+-+-+
	fmt.Printf("%c", '+')
	fmt.Printf("%c", '-')
	fmt.Printf("%c", '+')
	fmt.Printf("%c", '-')
	fmt.Printf("%c", '+')
	fmt.Printf("%c", '-')
	fmt.Printf("%c", '+')
	fmt.Printf("\n")

	// Row 1 vertical walls and cells
	fmt.Printf("%c", '|')
	fmt.Printf("%c", ' ')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '|')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", ' ')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '|')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", ' ')
	fmt.Printf("%c", '|')
	fmt.Printf("\n")

	// Row 2 horizontal walls
	fmt.Printf("%c", '+')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '-')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", '+')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '-')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", '+')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '-')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", '+')
	fmt.Printf("\n")

	// Row 2 vertical walls and cells
	fmt.Printf("%c", '|')
	fmt.Printf("%c", ' ')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '|')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", ' ')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '|')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", ' ')
	fmt.Printf("%c", '|')
	fmt.Printf("\n")

	// Row 3 horizontal walls
	fmt.Printf("%c", '+')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '-')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", '+')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '-')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", '+')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '-')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", '+')
	fmt.Printf("\n")

	// Row 3 vertical walls and cells
	fmt.Printf("%c", '|')
	fmt.Printf("%c", ' ')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '|')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", ' ')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '|')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", ' ')
	fmt.Printf("%c", '|')
	fmt.Printf("\n")

	// Bottom border
	fmt.Printf("%c", '+')
	fmt.Printf("%c", '-')
	fmt.Printf("%c", '+')
	fmt.Printf("%c", '-')
	fmt.Printf("%c", '+')
	fmt.Printf("%c", '-')
	fmt.Printf("%c", '+')
	fmt.Printf("\n")

	// Cell 2
	fmt.Printf("\nRemoving wall from cell 2:\n")

	// Remove wall code:
	// Remove a cell wall if possible for cell 2
	cellIndex = 2

	wallRemoved = false
	direction = left

	// If the wall in this direction is NOT an exterior wall
	if cellToWallLUT[cellIndex][direction] >= 0 {
		wallToRemove = cellToWallLUT[cellIndex][direction]
		// If this wall has not been already removed
		if wallList[wallToRemove] == true {
			// Remove the wall and indicate a wall was successfully removed
			wallList[wallToRemove] = false
			wallRemoved = true
		}
	}

	// If we haven't removed a wall yet, try to remove one in the next direction
	// Check the guard, have we removed one wall
	if wallRemoved == false {
		direction = direction + 1
		// If the wall in this direction is NOT an exterior wall
		if cellToWallLUT[cellIndex][direction] >= 0 {
			wallToRemove = cellToWallLUT[cellIndex][direction]
			// If this wall has not been already removed
			if wallList[wallToRemove] == true {
				// Remove the wall and indicate a wall was successfully removed
				wallList[wallToRemove] = false
				wallRemoved = true
			}
		}
	}

	// If we haven't removed a wall yet, try to remove one in the next direction
	// Check the guard, have we removed one wall
	if wallRemoved == false {
		direction = direction + 1
		// If the wall in this direction is NOT an exterior wall
		if cellToWallLUT[cellIndex][direction] >= 0 {
			wallToRemove = cellToWallLUT[cellIndex][direction]
			// If this wall has not been already removed
			if wallList[wallToRemove] == true {
				// Remove the wall and indicate a wall was successfully removed
				wallList[wallToRemove] = false
				wallRemoved = true
			}
		}
	}

	// If we haven't removed a wall yet, try to remove one in the next direction
	// Check the guard, have we removed one wall
	if wallRemoved == false {
		direction = direction + 1
		// If the wall in this direction is NOT an exterior wall
		if cellToWallLUT[cellIndex][direction] >= 0 {
			wallToRemove = cellToWallLUT[cellIndex][direction]
			// If this wall has not been already removed
			if wallList[wallToRemove] == true {
				// Remove the wall and indicate a wall was successfully removed
				wallList[wallToRemove] = false
				wallRemoved = true
			}
		}
	}

	// Print maze after cell 2
	time.Sleep(500 * time.Millisecond)

	// Print maze (duplicate of above print code)
	currentInteriorWall = 0

	// +-+-+-+
	fmt.Printf("%c", '+')
	fmt.Printf("%c", '-')
	fmt.Printf("%c", '+')
	fmt.Printf("%c", '-')
	fmt.Printf("%c", '+')
	fmt.Printf("%c", '-')
	fmt.Printf("%c", '+')
	fmt.Printf("\n")

	// Row 1 vertical walls and cells
	fmt.Printf("%c", '|')
	fmt.Printf("%c", ' ')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '|')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", ' ')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '|')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", ' ')
	fmt.Printf("%c", '|')
	fmt.Printf("\n")

	// Row 2 horizontal walls
	fmt.Printf("%c", '+')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '-')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", '+')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '-')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", '+')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '-')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", '+')
	fmt.Printf("\n")

	// Row 2 vertical walls and cells
	fmt.Printf("%c", '|')
	fmt.Printf("%c", ' ')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '|')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", ' ')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '|')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", ' ')
	fmt.Printf("%c", '|')
	fmt.Printf("\n")

	// Row 3 horizontal walls
	fmt.Printf("%c", '+')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '-')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", '+')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '-')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", '+')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '-')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", '+')
	fmt.Printf("\n")

	// Row 3 vertical walls and cells
	fmt.Printf("%c", '|')
	fmt.Printf("%c", ' ')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '|')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", ' ')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '|')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", ' ')
	fmt.Printf("%c", '|')
	fmt.Printf("\n")

	// Bottom border
	fmt.Printf("%c", '+')
	fmt.Printf("%c", '-')
	fmt.Printf("%c", '+')
	fmt.Printf("%c", '-')
	fmt.Printf("%c", '+')
	fmt.Printf("%c", '-')
	fmt.Printf("%c", '+')
	fmt.Printf("\n")

	// Cell 3
	fmt.Printf("\nRemoving wall from cell 3:\n")

	// Remove wall code:
	// Remove a cell wall if possible for cell 3
	cellIndex = 3

	wallRemoved = false
	direction = left

	// If the wall in this direction is NOT an exterior wall
	if cellToWallLUT[cellIndex][direction] >= 0 {
		wallToRemove = cellToWallLUT[cellIndex][direction]
		// If this wall has not been already removed
		if wallList[wallToRemove] == true {
			// Remove the wall and indicate a wall was successfully removed
			wallList[wallToRemove] = false
			wallRemoved = true
		}
	}

	// If we haven't removed a wall yet, try to remove one in the next direction
	// Check the guard, have we removed one wall
	if wallRemoved == false {
		direction = direction + 1
		// If the wall in this direction is NOT an exterior wall
		if cellToWallLUT[cellIndex][direction] >= 0 {
			wallToRemove = cellToWallLUT[cellIndex][direction]
			// If this wall has not been already removed
			if wallList[wallToRemove] == true {
				// Remove the wall and indicate a wall was successfully removed
				wallList[wallToRemove] = false
				wallRemoved = true
			}
		}
	}

	// If we haven't removed a wall yet, try to remove one in the next direction
	// Check the guard, have we removed one wall
	if wallRemoved == false {
		direction = direction + 1
		// If the wall in this direction is NOT an exterior wall
		if cellToWallLUT[cellIndex][direction] >= 0 {
			wallToRemove = cellToWallLUT[cellIndex][direction]
			// If this wall has not been already removed
			if wallList[wallToRemove] == true {
				// Remove the wall and indicate a wall was successfully removed
				wallList[wallToRemove] = false
				wallRemoved = true
			}
		}
	}

	// If we haven't removed a wall yet, try to remove one in the next direction
	// Check the guard, have we removed one wall
	if wallRemoved == false {
		direction = direction + 1
		// If the wall in this direction is NOT an exterior wall
		if cellToWallLUT[cellIndex][direction] >= 0 {
			wallToRemove = cellToWallLUT[cellIndex][direction]
			// If this wall has not been already removed
			if wallList[wallToRemove] == true {
				// Remove the wall and indicate a wall was successfully removed
				wallList[wallToRemove] = false
				wallRemoved = true
			}
		}
	}

	// Print maze after cell 3
	time.Sleep(500 * time.Millisecond)

	// Print maze (duplicate of above print code)
	currentInteriorWall = 0

	// +-+-+-+
	fmt.Printf("%c", '+')
	fmt.Printf("%c", '-')
	fmt.Printf("%c", '+')
	fmt.Printf("%c", '-')
	fmt.Printf("%c", '+')
	fmt.Printf("%c", '-')
	fmt.Printf("%c", '+')
	fmt.Printf("\n")

	// Row 1 vertical walls and cells
	fmt.Printf("%c", '|')
	fmt.Printf("%c", ' ')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '|')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", ' ')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '|')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", ' ')
	fmt.Printf("%c", '|')
	fmt.Printf("\n")

	// Row 2 horizontal walls
	fmt.Printf("%c", '+')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '-')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", '+')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '-')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", '+')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '-')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", '+')
	fmt.Printf("\n")

	// Row 2 vertical walls and cells
	fmt.Printf("%c", '|')
	fmt.Printf("%c", ' ')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '|')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", ' ')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '|')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", ' ')
	fmt.Printf("%c", '|')
	fmt.Printf("\n")

	// Row 3 horizontal walls
	fmt.Printf("%c", '+')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '-')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", '+')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '-')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", '+')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '-')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", '+')
	fmt.Printf("\n")

	// Row 3 vertical walls and cells
	fmt.Printf("%c", '|')
	fmt.Printf("%c", ' ')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '|')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", ' ')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '|')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", ' ')
	fmt.Printf("%c", '|')
	fmt.Printf("\n")

	// Bottom border
	fmt.Printf("%c", '+')
	fmt.Printf("%c", '-')
	fmt.Printf("%c", '+')
	fmt.Printf("%c", '-')
	fmt.Printf("%c", '+')
	fmt.Printf("%c", '-')
	fmt.Printf("%c", '+')
	fmt.Printf("\n")

	// Cell 4
	fmt.Printf("\nRemoving wall from cell 4:\n")

	// Remove wall code:
	// Remove a cell wall if possible for cell 4
	cellIndex = 4

	wallRemoved = false
	direction = left

	// If the wall in this direction is NOT an exterior wall
	if cellToWallLUT[cellIndex][direction] >= 0 {
		wallToRemove = cellToWallLUT[cellIndex][direction]
		// If this wall has not been already removed
		if wallList[wallToRemove] == true {
			// Remove the wall and indicate a wall was successfully removed
			wallList[wallToRemove] = false
			wallRemoved = true
		}
	}

	// If we haven't removed a wall yet, try to remove one in the next direction
	// Check the guard, have we removed one wall
	if wallRemoved == false {
		direction = direction + 1
		// If the wall in this direction is NOT an exterior wall
		if cellToWallLUT[cellIndex][direction] >= 0 {
			wallToRemove = cellToWallLUT[cellIndex][direction]
			// If this wall has not been already removed
			if wallList[wallToRemove] == true {
				// Remove the wall and indicate a wall was successfully removed
				wallList[wallToRemove] = false
				wallRemoved = true
			}
		}
	}

	// If we haven't removed a wall yet, try to remove one in the next direction
	// Check the guard, have we removed one wall
	if wallRemoved == false {
		direction = direction + 1
		// If the wall in this direction is NOT an exterior wall
		if cellToWallLUT[cellIndex][direction] >= 0 {
			wallToRemove = cellToWallLUT[cellIndex][direction]
			// If this wall has not been already removed
			if wallList[wallToRemove] == true {
				// Remove the wall and indicate a wall was successfully removed
				wallList[wallToRemove] = false
				wallRemoved = true
			}
		}
	}

	// If we haven't removed a wall yet, try to remove one in the next direction
	// Check the guard, have we removed one wall
	if wallRemoved == false {
		direction = direction + 1
		// If the wall in this direction is NOT an exterior wall
		if cellToWallLUT[cellIndex][direction] >= 0 {
			wallToRemove = cellToWallLUT[cellIndex][direction]
			// If this wall has not been already removed
			if wallList[wallToRemove] == true {
				// Remove the wall and indicate a wall was successfully removed
				wallList[wallToRemove] = false
				wallRemoved = true
			}
		}
	}

	// Print maze after cell 4
	time.Sleep(500 * time.Millisecond)

	// Print maze (duplicate of above print code)
	currentInteriorWall = 0

	// +-+-+-+
	fmt.Printf("%c", '+')
	fmt.Printf("%c", '-')
	fmt.Printf("%c", '+')
	fmt.Printf("%c", '-')
	fmt.Printf("%c", '+')
	fmt.Printf("%c", '-')
	fmt.Printf("%c", '+')
	fmt.Printf("\n")

	// Row 1 vertical walls and cells
	fmt.Printf("%c", '|')
	fmt.Printf("%c", ' ')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '|')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", ' ')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '|')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", ' ')
	fmt.Printf("%c", '|')
	fmt.Printf("\n")

	// Row 2 horizontal walls
	fmt.Printf("%c", '+')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '-')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", '+')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '-')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", '+')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '-')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", '+')
	fmt.Printf("\n")

	// Row 2 vertical walls and cells
	fmt.Printf("%c", '|')
	fmt.Printf("%c", ' ')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '|')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", ' ')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '|')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", ' ')
	fmt.Printf("%c", '|')
	fmt.Printf("\n")

	// Row 3 horizontal walls
	fmt.Printf("%c", '+')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '-')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", '+')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '-')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", '+')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '-')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", '+')
	fmt.Printf("\n")

	// Row 3 vertical walls and cells
	fmt.Printf("%c", '|')
	fmt.Printf("%c", ' ')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '|')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", ' ')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '|')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", ' ')
	fmt.Printf("%c", '|')
	fmt.Printf("\n")

	// Bottom border
	fmt.Printf("%c", '+')
	fmt.Printf("%c", '-')
	fmt.Printf("%c", '+')
	fmt.Printf("%c", '-')
	fmt.Printf("%c", '+')
	fmt.Printf("%c", '-')
	fmt.Printf("%c", '+')
	fmt.Printf("\n")

	// Cell 5
	fmt.Printf("\nRemoving wall from cell 5:\n")

	// Remove wall code:
	// Remove a cell wall if possible for cell 5
	cellIndex = 5

	wallRemoved = false
	direction = left

	// If the wall in this direction is NOT an exterior wall
	if cellToWallLUT[cellIndex][direction] >= 0 {
		wallToRemove = cellToWallLUT[cellIndex][direction]
		// If this wall has not been already removed
		if wallList[wallToRemove] == true {
			// Remove the wall and indicate a wall was successfully removed
			wallList[wallToRemove] = false
			wallRemoved = true
		}
	}

	// If we haven't removed a wall yet, try to remove one in the next direction
	// Check the guard, have we removed one wall
	if wallRemoved == false {
		direction = direction + 1
		// If the wall in this direction is NOT an exterior wall
		if cellToWallLUT[cellIndex][direction] >= 0 {
			wallToRemove = cellToWallLUT[cellIndex][direction]
			// If this wall has not been already removed
			if wallList[wallToRemove] == true {
				// Remove the wall and indicate a wall was successfully removed
				wallList[wallToRemove] = false
				wallRemoved = true
			}
		}
	}

	// If we haven't removed a wall yet, try to remove one in the next direction
	// Check the guard, have we removed one wall
	if wallRemoved == false {
		direction = direction + 1
		// If the wall in this direction is NOT an exterior wall
		if cellToWallLUT[cellIndex][direction] >= 0 {
			wallToRemove = cellToWallLUT[cellIndex][direction]
			// If this wall has not been already removed
			if wallList[wallToRemove] == true {
				// Remove the wall and indicate a wall was successfully removed
				wallList[wallToRemove] = false
				wallRemoved = true
			}
		}
	}

	// If we haven't removed a wall yet, try to remove one in the next direction
	// Check the guard, have we removed one wall
	if wallRemoved == false {
		direction = direction + 1
		// If the wall in this direction is NOT an exterior wall
		if cellToWallLUT[cellIndex][direction] >= 0 {
			wallToRemove = cellToWallLUT[cellIndex][direction]
			// If this wall has not been already removed
			if wallList[wallToRemove] == true {
				// Remove the wall and indicate a wall was successfully removed
				wallList[wallToRemove] = false
				wallRemoved = true
			}
		}
	}

	// Print maze after cell 5
	time.Sleep(500 * time.Millisecond)

	// Print maze (duplicate of above print code)
	currentInteriorWall = 0

	// +-+-+-+
	fmt.Printf("%c", '+')
	fmt.Printf("%c", '-')
	fmt.Printf("%c", '+')
	fmt.Printf("%c", '-')
	fmt.Printf("%c", '+')
	fmt.Printf("%c", '-')
	fmt.Printf("%c", '+')
	fmt.Printf("\n")

	// Row 1 vertical walls and cells
	fmt.Printf("%c", '|')
	fmt.Printf("%c", ' ')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '|')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", ' ')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '|')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", ' ')
	fmt.Printf("%c", '|')
	fmt.Printf("\n")

	// Row 2 horizontal walls
	fmt.Printf("%c", '+')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '-')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", '+')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '-')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", '+')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '-')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", '+')
	fmt.Printf("\n")

	// Row 2 vertical walls and cells
	fmt.Printf("%c", '|')
	fmt.Printf("%c", ' ')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '|')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", ' ')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '|')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", ' ')
	fmt.Printf("%c", '|')
	fmt.Printf("\n")

	// Row 3 horizontal walls
	fmt.Printf("%c", '+')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '-')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", '+')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '-')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", '+')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '-')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", '+')
	fmt.Printf("\n")

	// Row 3 vertical walls and cells
	fmt.Printf("%c", '|')
	fmt.Printf("%c", ' ')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '|')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", ' ')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '|')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", ' ')
	fmt.Printf("%c", '|')
	fmt.Printf("\n")

	// Bottom border
	fmt.Printf("%c", '+')
	fmt.Printf("%c", '-')
	fmt.Printf("%c", '+')
	fmt.Printf("%c", '-')
	fmt.Printf("%c", '+')
	fmt.Printf("%c", '-')
	fmt.Printf("%c", '+')
	fmt.Printf("\n")

	// Cell 6
	fmt.Printf("\nRemoving wall from cell 6:\n")

	// Remove wall code:
	// Remove a cell wall if possible for cell 6
	cellIndex = 6

	wallRemoved = false
	direction = left

	// If the wall in this direction is NOT an exterior wall
	if cellToWallLUT[cellIndex][direction] >= 0 {
		wallToRemove = cellToWallLUT[cellIndex][direction]
		// If this wall has not been already removed
		if wallList[wallToRemove] == true {
			// Remove the wall and indicate a wall was successfully removed
			wallList[wallToRemove] = false
			wallRemoved = true
		}
	}

	// If we haven't removed a wall yet, try to remove one in the next direction
	// Check the guard, have we removed one wall
	if wallRemoved == false {
		direction = direction + 1
		// If the wall in this direction is NOT an exterior wall
		if cellToWallLUT[cellIndex][direction] >= 0 {
			wallToRemove = cellToWallLUT[cellIndex][direction]
			// If this wall has not been already removed
			if wallList[wallToRemove] == true {
				// Remove the wall and indicate a wall was successfully removed
				wallList[wallToRemove] = false
				wallRemoved = true
			}
		}
	}

	// If we haven't removed a wall yet, try to remove one in the next direction
	// Check the guard, have we removed one wall
	if wallRemoved == false {
		direction = direction + 1
		// If the wall in this direction is NOT an exterior wall
		if cellToWallLUT[cellIndex][direction] >= 0 {
			wallToRemove = cellToWallLUT[cellIndex][direction]
			// If this wall has not been already removed
			if wallList[wallToRemove] == true {
				// Remove the wall and indicate a wall was successfully removed
				wallList[wallToRemove] = false
				wallRemoved = true
			}
		}
	}

	// If we haven't removed a wall yet, try to remove one in the next direction
	// Check the guard, have we removed one wall
	if wallRemoved == false {
		direction = direction + 1
		// If the wall in this direction is NOT an exterior wall
		if cellToWallLUT[cellIndex][direction] >= 0 {
			wallToRemove = cellToWallLUT[cellIndex][direction]
			// If this wall has not been already removed
			if wallList[wallToRemove] == true {
				// Remove the wall and indicate a wall was successfully removed
				wallList[wallToRemove] = false
				wallRemoved = true
			}
		}
	}

	// Print maze after cell 6
	time.Sleep(500 * time.Millisecond)

	// Print maze (duplicate of above print code)
	currentInteriorWall = 0

	// +-+-+-+
	fmt.Printf("%c", '+')
	fmt.Printf("%c", '-')
	fmt.Printf("%c", '+')
	fmt.Printf("%c", '-')
	fmt.Printf("%c", '+')
	fmt.Printf("%c", '-')
	fmt.Printf("%c", '+')
	fmt.Printf("\n")

	// Row 1 vertical walls and cells
	fmt.Printf("%c", '|')
	fmt.Printf("%c", ' ')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '|')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", ' ')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '|')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", ' ')
	fmt.Printf("%c", '|')
	fmt.Printf("\n")

	// Row 2 horizontal walls
	fmt.Printf("%c", '+')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '-')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", '+')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '-')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", '+')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '-')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", '+')
	fmt.Printf("\n")

	// Row 2 vertical walls and cells
	fmt.Printf("%c", '|')
	fmt.Printf("%c", ' ')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '|')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", ' ')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '|')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", ' ')
	fmt.Printf("%c", '|')
	fmt.Printf("\n")

	// Row 3 horizontal walls
	fmt.Printf("%c", '+')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '-')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", '+')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '-')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", '+')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '-')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", '+')
	fmt.Printf("\n")

	// Row 3 vertical walls and cells
	fmt.Printf("%c", '|')
	fmt.Printf("%c", ' ')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '|')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", ' ')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '|')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", ' ')
	fmt.Printf("%c", '|')
	fmt.Printf("\n")

	// Bottom border
	fmt.Printf("%c", '+')
	fmt.Printf("%c", '-')
	fmt.Printf("%c", '+')
	fmt.Printf("%c", '-')
	fmt.Printf("%c", '+')
	fmt.Printf("%c", '-')
	fmt.Printf("%c", '+')
	fmt.Printf("\n")

	// Cell 7
	fmt.Printf("\nRemoving wall from cell 7:\n")

	// Remove wall code:
	// Remove a cell wall if possible for cell 7
	cellIndex = 7

	wallRemoved = false
	direction = left

	// If the wall in this direction is NOT an exterior wall
	if cellToWallLUT[cellIndex][direction] >= 0 {
		wallToRemove = cellToWallLUT[cellIndex][direction]
		// If this wall has not been already removed
		if wallList[wallToRemove] == true {
			// Remove the wall and indicate a wall was successfully removed
			wallList[wallToRemove] = false
			wallRemoved = true
		}
	}

	// If we haven't removed a wall yet, try to remove one in the next direction
	// Check the guard, have we removed one wall
	if wallRemoved == false {
		direction = direction + 1
		// If the wall in this direction is NOT an exterior wall
		if cellToWallLUT[cellIndex][direction] >= 0 {
			wallToRemove = cellToWallLUT[cellIndex][direction]
			// If this wall has not been already removed
			if wallList[wallToRemove] == true {
				// Remove the wall and indicate a wall was successfully removed
				wallList[wallToRemove] = false
				wallRemoved = true
			}
		}
	}

	// If we haven't removed a wall yet, try to remove one in the next direction
	// Check the guard, have we removed one wall
	if wallRemoved == false {
		direction = direction + 1
		// If the wall in this direction is NOT an exterior wall
		if cellToWallLUT[cellIndex][direction] >= 0 {
			wallToRemove = cellToWallLUT[cellIndex][direction]
			// If this wall has not been already removed
			if wallList[wallToRemove] == true {
				// Remove the wall and indicate a wall was successfully removed
				wallList[wallToRemove] = false
				wallRemoved = true
			}
		}
	}

	// If we haven't removed a wall yet, try to remove one in the next direction
	// Check the guard, have we removed one wall
	if wallRemoved == false {
		direction = direction + 1
		// If the wall in this direction is NOT an exterior wall
		if cellToWallLUT[cellIndex][direction] >= 0 {
			wallToRemove = cellToWallLUT[cellIndex][direction]
			// If this wall has not been already removed
			if wallList[wallToRemove] == true {
				// Remove the wall and indicate a wall was successfully removed
				wallList[wallToRemove] = false
				wallRemoved = true
			}
		}
	}

	// Print maze after cell 7
	time.Sleep(500 * time.Millisecond)

	// Print maze (duplicate of above print code)
	currentInteriorWall = 0

	// +-+-+-+
	fmt.Printf("%c", '+')
	fmt.Printf("%c", '-')
	fmt.Printf("%c", '+')
	fmt.Printf("%c", '-')
	fmt.Printf("%c", '+')
	fmt.Printf("%c", '-')
	fmt.Printf("%c", '+')
	fmt.Printf("\n")

	// Row 1 vertical walls and cells
	fmt.Printf("%c", '|')
	fmt.Printf("%c", ' ')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '|')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", ' ')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '|')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", ' ')
	fmt.Printf("%c", '|')
	fmt.Printf("\n")

	// Row 2 horizontal walls
	fmt.Printf("%c", '+')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '-')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", '+')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '-')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", '+')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '-')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", '+')
	fmt.Printf("\n")

	// Row 2 vertical walls and cells
	fmt.Printf("%c", '|')
	fmt.Printf("%c", ' ')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '|')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", ' ')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '|')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", ' ')
	fmt.Printf("%c", '|')
	fmt.Printf("\n")

	// Row 3 horizontal walls
	fmt.Printf("%c", '+')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '-')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", '+')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '-')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", '+')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '-')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", '+')
	fmt.Printf("\n")

	// Row 3 vertical walls and cells
	fmt.Printf("%c", '|')
	fmt.Printf("%c", ' ')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '|')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", ' ')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '|')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", ' ')
	fmt.Printf("%c", '|')
	fmt.Printf("\n")

	// Bottom border
	fmt.Printf("%c", '+')
	fmt.Printf("%c", '-')
	fmt.Printf("%c", '+')
	fmt.Printf("%c", '-')
	fmt.Printf("%c", '+')
	fmt.Printf("%c", '-')
	fmt.Printf("%c", '+')
	fmt.Printf("\n")

	// Cell 8
	fmt.Printf("\nRemoving wall from cell 8:\n")

	// Remove wall code:
	// Remove a cell wall if possible for cell 8
	cellIndex = 8

	wallRemoved = false
	direction = left

	// If the wall in this direction is NOT an exterior wall
	if cellToWallLUT[cellIndex][direction] >= 0 {
		wallToRemove = cellToWallLUT[cellIndex][direction]
		// If this wall has not been already removed
		if wallList[wallToRemove] == true {
			// Remove the wall and indicate a wall was successfully removed
			wallList[wallToRemove] = false
			wallRemoved = true
		}
	}

	// If we haven't removed a wall yet, try to remove one in the next direction
	// Check the guard, have we removed one wall
	if wallRemoved == false {
		direction = direction + 1
		// If the wall in this direction is NOT an exterior wall
		if cellToWallLUT[cellIndex][direction] >= 0 {
			wallToRemove = cellToWallLUT[cellIndex][direction]
			// If this wall has not been already removed
			if wallList[wallToRemove] == true {
				// Remove the wall and indicate a wall was successfully removed
				wallList[wallToRemove] = false
				wallRemoved = true
			}
		}
	}

	// If we haven't removed a wall yet, try to remove one in the next direction
	// Check the guard, have we removed one wall
	if wallRemoved == false {
		direction = direction + 1
		// If the wall in this direction is NOT an exterior wall
		if cellToWallLUT[cellIndex][direction] >= 0 {
			wallToRemove = cellToWallLUT[cellIndex][direction]
			// If this wall has not been already removed
			if wallList[wallToRemove] == true {
				// Remove the wall and indicate a wall was successfully removed
				wallList[wallToRemove] = false
				wallRemoved = true
			}
		}
	}

	// If we haven't removed a wall yet, try to remove one in the next direction
	// Check the guard, have we removed one wall
	if wallRemoved == false {
		direction = direction + 1
		// If the wall in this direction is NOT an exterior wall
		if cellToWallLUT[cellIndex][direction] >= 0 {
			wallToRemove = cellToWallLUT[cellIndex][direction]
			// If this wall has not been already removed
			if wallList[wallToRemove] == true {
				// Remove the wall and indicate a wall was successfully removed
				wallList[wallToRemove] = false
				wallRemoved = true
			}
		}
	}

	// Print maze after cell 8
	time.Sleep(500 * time.Millisecond)

	// Print maze (duplicate of above print code)
	currentInteriorWall = 0

	// +-+-+-+
	fmt.Printf("%c", '+')
	fmt.Printf("%c", '-')
	fmt.Printf("%c", '+')
	fmt.Printf("%c", '-')
	fmt.Printf("%c", '+')
	fmt.Printf("%c", '-')
	fmt.Printf("%c", '+')
	fmt.Printf("\n")

	// Row 1 vertical walls and cells
	fmt.Printf("%c", '|')
	fmt.Printf("%c", ' ')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '|')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", ' ')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '|')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", ' ')
	fmt.Printf("%c", '|')
	fmt.Printf("\n")

	// Row 2 horizontal walls
	fmt.Printf("%c", '+')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '-')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", '+')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '-')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", '+')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '-')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", '+')
	fmt.Printf("\n")

	// Row 2 vertical walls and cells
	fmt.Printf("%c", '|')
	fmt.Printf("%c", ' ')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '|')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", ' ')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '|')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", ' ')
	fmt.Printf("%c", '|')
	fmt.Printf("\n")

	// Row 3 horizontal walls
	fmt.Printf("%c", '+')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '-')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", '+')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '-')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", '+')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '-')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", '+')
	fmt.Printf("\n")

	// Row 3 vertical walls and cells
	fmt.Printf("%c", '|')
	fmt.Printf("%c", ' ')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '|')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", ' ')
	if wallList[currentInteriorWall] == true {
		fmt.Printf("%c", '|')
	} else {
		fmt.Printf("%c", ' ')
	}
	currentInteriorWall = currentInteriorWall + 1
	fmt.Printf("%c", ' ')
	fmt.Printf("%c", '|')
	fmt.Printf("\n")

	// Bottom border
	fmt.Printf("%c", '+')
	fmt.Printf("%c", '-')
	fmt.Printf("%c", '+')
	fmt.Printf("%c", '-')
	fmt.Printf("%c", '+')
	fmt.Printf("%c", '-')
	fmt.Printf("%c", '+')
	fmt.Printf("\n")
}
