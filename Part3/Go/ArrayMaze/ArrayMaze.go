package main

import "fmt"

func main() {
	// This maze program does not necessarily progress the program, 
	// but provides a construct we will use eventually in the program.
	mazeWalls := [5][3]rune{
		{'|', ' ', 0},
		{' ', ' ', ' '},
		{' ', '|', 0},
		{' ', '-', ' '},
		{'|', ' ', 0},
	}

	const LEFT = 0
	const UP = 1
	const RIGHT = 2
	const DOWN = 3

	neighborLookup := [9][4]int{
		{-1, -1, -1, 3}, // Cell 0
		{-1, -1, 2, 4},  // Cell 1
		{1, -1, -1, 5},  // Cell 2
		{-1, 0, 4, 6},   // Cell 3
		{3, 1, -1, -1},  // Cell 4
		{-1, 2, -1, 8},  // Cell 5
		{-1, 3, -1, -1}, // Cell 6
		{-1, -1, 8, -1}, // Cell 7
		{7, 5, -1, -1},  // Cell 8
	}

	topBottomRow := [7]rune{'+', '-', '+', '-', '+', '-', '+'}

	// This sort of code will look better when we have loops and functions
	// We see how a 3x3 maze is created from the array of data, and using the 
	// neighbor lookup provides -1 where a wall is in that direction, otherwise 
	// provides the cell number in that direction.
	fmt.Printf("%c", topBottomRow[0])
	fmt.Printf("%c", topBottomRow[1])
	fmt.Printf("%c", topBottomRow[2])
	fmt.Printf("%c", topBottomRow[3])
	fmt.Printf("%c", topBottomRow[4])
	fmt.Printf("%c", topBottomRow[5])
	fmt.Printf("%c", topBottomRow[6])
	fmt.Printf("\n")

	fmt.Printf("|0")
	fmt.Printf("%c", mazeWalls[0][0])
	fmt.Printf("1")
	fmt.Printf("%c", mazeWalls[0][1])
	fmt.Printf("2|")
	fmt.Printf("\n")

	fmt.Printf("+")
	fmt.Printf("%c", mazeWalls[1][0])
	fmt.Printf("+")
	fmt.Printf("%c", mazeWalls[1][1])
	fmt.Printf("+")
	fmt.Printf("%c", mazeWalls[1][2])
	fmt.Printf("+")
	fmt.Printf("\n")

	fmt.Printf("|3")
	fmt.Printf("%c", mazeWalls[2][0])
	fmt.Printf("4")
	fmt.Printf("%c", mazeWalls[2][1])
	fmt.Printf("5|")
	fmt.Printf("\n")

	fmt.Printf("+")
	fmt.Printf("%c", mazeWalls[3][0])
	fmt.Printf("+")
	fmt.Printf("%c", mazeWalls[3][1])
	fmt.Printf("+")
	fmt.Printf("%c", mazeWalls[3][2])
	fmt.Printf("+")
	fmt.Printf("\n")

	fmt.Printf("|6")
	fmt.Printf("%c", mazeWalls[4][0])
	fmt.Printf("7")
	fmt.Printf("%c", mazeWalls[4][1])
	fmt.Printf("8|")
	fmt.Printf("\n")

	fmt.Printf("%c", topBottomRow[0])
	fmt.Printf("%c", topBottomRow[1])
	fmt.Printf("%c", topBottomRow[2])
	fmt.Printf("%c", topBottomRow[3])
	fmt.Printf("%c", topBottomRow[4])
	fmt.Printf("%c", topBottomRow[5])
	fmt.Printf("%c", topBottomRow[6])
	fmt.Printf("\n")

	// So take cell number 4 and see what rooms are around it, 
	// cell 3 is to the left and cell 1 is up, but walls are right and down.
	fmt.Printf("Cell to left of cell 4 is: %d\n", neighborLookup[4][LEFT])
	fmt.Printf("Cell to up of cell 4 is: %d\n", neighborLookup[4][UP])
	fmt.Printf("Cell to right of cell 4 is: %d\n", neighborLookup[4][RIGHT])
	fmt.Printf("Cell to down of cell 4 is: %d\n", neighborLookup[4][DOWN])
}
