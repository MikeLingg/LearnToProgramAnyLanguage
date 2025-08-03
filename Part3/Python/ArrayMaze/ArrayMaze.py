def main():
    # This maze program does not necessarily progress the program, 
    # but provides a construct we will use eventually in the program.
    mazeWalls = [
        [ '|', ' ' ],
        [ ' ', ' ', ' ' ],
        [ ' ', '|' ],
        [ ' ', '-', ' ' ],
        [ '|', ' ' ]
    ]
    
    LEFT = 0
    UP = 1
    RIGHT = 2
    DOWN = 3
    
    neighborLookup = [
        [ -1, -1, -1, 3 ],  # Cell 0
        [ -1, -1, 2, 4 ],   # Cell 1
        [ 1, -1, -1, 5 ],   # Cell 2
        [ -1, 0, 4, 6 ],    # Cell 3
        [ 3, 1, -1, -1 ],   # Cell 4
        [ -1, 2, -1, 8 ],   # Cell 5
        [ -1, 3, -1, -1 ],  # Cell 6
        [ -1, -1, 8, -1 ],  # Cell 7
        [ 7, 5, -1, -1 ]    # Cell 8
    ]
    
    topBottomRow = [ '+', '-', '+', '-', '+', '-', '+' ]
    
    # This sort of code will look better when we have loops and functions
    # We see how a 3x3 maze is created from the array of data, and using the 
    # neighbor lookup provides -1 where a wall is in that direction, otherwise 
    # provides the cell number in that direction.
    print( topBottomRow[ 0 ], end='' )
    print( topBottomRow[ 1 ], end='' )
    print( topBottomRow[ 2 ], end='' )
    print( topBottomRow[ 3 ], end='' )
    print( topBottomRow[ 4 ], end='' )
    print( topBottomRow[ 5 ], end='' )
    print( topBottomRow[ 6 ], end='' )
    print()
    
    print( "|0", end='' )
    print( mazeWalls[ 0 ][ 0 ], end='' )
    print( "1", end='' )
    print( mazeWalls[ 0 ][ 1 ], end='' )
    print( "2|", end='' )
    print()
    
    print( "+", end='' )
    print( mazeWalls[ 1 ][ 0 ], end='' )
    print( "+", end='' )
    print( mazeWalls[ 1 ][ 1 ], end='' )
    print( "+", end='' )
    print( mazeWalls[ 1 ][ 2 ], end='' )
    print( "+", end='' )
    print()
    
    print( "|3", end='' )
    print( mazeWalls[ 2 ][ 0 ], end='' )
    print( "4", end='' )
    print( mazeWalls[ 2 ][ 1 ], end='' )
    print( "5|", end='' )
    print()
    
    print( "+", end='' )
    print( mazeWalls[ 3 ][ 0 ], end='' )
    print( "+", end='' )
    print( mazeWalls[ 3 ][ 1 ], end='' )
    print( "+", end='' )
    print( mazeWalls[ 3 ][ 2 ], end='' )
    print( "+", end='' )
    print()
    
    print( "|6", end='' )
    print( mazeWalls[ 4 ][ 0 ], end='' )
    print( "7", end='' )
    print( mazeWalls[ 4 ][ 1 ], end='' )
    print( "8|", end='' )
    print()
    
    print( topBottomRow[ 0 ], end='' )
    print( topBottomRow[ 1 ], end='' )
    print( topBottomRow[ 2 ], end='' )
    print( topBottomRow[ 3 ], end='' )
    print( topBottomRow[ 4 ], end='' )
    print( topBottomRow[ 5 ], end='' )
    print( topBottomRow[ 6 ], end='' )
    print()
    
    # So take cell number 4 and see what rooms are around it, 
    # cell 3 is to the left and cell 1 is up, but walls are right and down.
    print( f"Cell to left of cell 4 is: { neighborLookup[ 4 ][ LEFT ] }" )
    print( f"Cell to up of cell 4 is: { neighborLookup[ 4 ][ UP ] }" )
    print( f"Cell to right of cell 4 is: { neighborLookup[ 4 ][ RIGHT ] }" )
    print( f"Cell to down of cell 4 is: { neighborLookup[ 4 ][ DOWN ] }" )

if __name__ == "__main__":
    main()
