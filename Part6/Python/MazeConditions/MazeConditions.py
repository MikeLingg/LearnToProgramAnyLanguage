import time

def main():
    # Initialization code:
    # Cell indexes:
    # +-+-+-+
    # |0|1|2|
    # +-+-+-+
    # |3|4|5|
    # +-+-+-+
    # |6|7|8|
    # +-+-+-+
    
    # Interior wall indexes:
    # +-+-+-+
    # | 0 1 |
    # +2+3+4+
    # | 5 6 |
    # +7+8+9+
    # | 10 11 |
    # +-+-+-+
    
    # Define directions
    LEFT = 0
    UP = 1
    RIGHT = 2
    DOWN = 3
    
    # For each cell 0-8, indicate if a wall is exterior and cannot be removed (-1) or its interior index for each of
    # the four directions, LEFT, UP, RIGHT, DOWN.
    cellToWallLUT = [
        [-1, -1, 0, 2],   [0, -1, 1, 3],   [1, -1, -1, 4],
        [-1, 2, 5, 7],    [5, 3, 6, 8],    [6, 4, -1, 9],
        [-1, 7, 10, -1],  [10, 8, 11, -1], [11, 9, -1, -1]
    ]
    
    # 12 interior walls in a 3x3 maze. Start with all the walls up.
    wallList = [True, True, True, True, True, True, True, True, True, True, True, True]
    
    # Print initial maze
    print( "Initial Maze:" )
    
    # Print out the maze, this is a rather painful copy/paste job without loops and functions.
    # Note printChar is meant to print a single character with no newlines.
    # Horizontal walls above row 1 - All are exterior walls, no conditions.
    currentInteriorWall = 0
    
    # +-+-+-+
    print( '+', end='' )
    print( '-', end='' )
    print( '+', end='' )
    print( '-', end='' )
    print( '+', end='' )
    print( '-', end='' )
    print( '+' )
    
    # Vertical walls and cells row 1.
    # The left and right vertical walls are exterior, always up.
    # | | | |
    print( '|', end='' )
    print( ' ', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '|', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( ' ', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '|', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( ' ', end='' )
    print( '|' )
    
    # Horizontal walls above row 2
    # +-+-+-+
    print( '+', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '-', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( '+', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '-', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( '+', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '-', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( '+' )
    
    # Vertical walls and cells row 2.
    # The left and right vertical walls are exterior, always up.
    # | | | |
    print( '|', end='' )
    print( ' ', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '|', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( ' ', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '|', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( ' ', end='' )
    print( '|' )
    
    # Horizontal walls above row 3
    # +-+-+-+
    print( '+', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '-', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( '+', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '-', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( '+', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '-', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( '+' )
    
    # Vertical walls and cells row 3.
    # The left and right vertical walls are exterior, always up.
    # | | | |
    print( '|', end='' )
    print( ' ', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '|', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( ' ', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '|', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( ' ', end='' )
    print( '|' )
    
    # Horizontal walls below row 3 - All are exterior walls, no conditions.
    # +-+-+-+
    print( '+', end='' )
    print( '-', end='' )
    print( '+', end='' )
    print( '-', end='' )
    print( '+', end='' )
    print( '-', end='' )
    print( '+' )
    
    # Now process each cell 0-8
    # Cell 0
    print( "\nRemoving wall from cell 0:" )
    
    # Remove wall code:
    # Remove a cell wall if possible for cell 0
    cellIndex = 0
    wallToRemove = 0
    
    wallRemoved = False
    direction = LEFT
    
    # If the wall in this direction is NOT an exterior wall
    if ( cellToWallLUT[cellIndex][direction] >= 0 ):
        wallToRemove = cellToWallLUT[cellIndex][direction]
        # If this wall has not been already removed
        if ( wallList[wallToRemove] == True ):
            # Remove the wall and indicate a wall was successfully removed
            wallList[wallToRemove] = False
            wallRemoved = True
    
    # If we haven't removed a wall yet, try to remove one in the next direction
    # Check the guard, have we removed one wall
    if ( wallRemoved == False ):
        direction = direction + 1
        # If the wall in this direction is NOT an exterior wall
        if ( cellToWallLUT[cellIndex][direction] >= 0 ):
            wallToRemove = cellToWallLUT[cellIndex][direction]
            # If this wall has not been already removed
            if ( wallList[wallToRemove] == True ):
                # Remove the wall and indicate a wall was successfully removed
                wallList[wallToRemove] = False
                wallRemoved = True
    
    # If we haven't removed a wall yet, try to remove one in the next direction
    # Check the guard, have we removed one wall
    if ( wallRemoved == False ):
        direction = direction + 1
        # If the wall in this direction is NOT an exterior wall
        if ( cellToWallLUT[cellIndex][direction] >= 0 ):
            wallToRemove = cellToWallLUT[cellIndex][direction]
            # If this wall has not been already removed
            if ( wallList[wallToRemove] == True ):
                # Remove the wall and indicate a wall was successfully removed
                wallList[wallToRemove] = False
                wallRemoved = True
    
    # If we haven't removed a wall yet, try to remove one in the next direction
    # Check the guard, have we removed one wall
    if ( wallRemoved == False ):
        direction = direction + 1
        # If the wall in this direction is NOT an exterior wall
        if ( cellToWallLUT[cellIndex][direction] >= 0 ):
            wallToRemove = cellToWallLUT[cellIndex][direction]
            # If this wall has not been already removed
            if ( wallList[wallToRemove] == True ):
                # Remove the wall and indicate a wall was successfully removed
                wallList[wallToRemove] = False
                wallRemoved = True
    
    # Print maze after cell 0
    time.sleep( 0.5 )
    
    # Print maze (duplicate of above print code)
    currentInteriorWall = 0
    
    # +-+-+-+
    print( '+', end='' )
    print( '-', end='' )
    print( '+', end='' )
    print( '-', end='' )
    print( '+', end='' )
    print( '-', end='' )
    print( '+' )
    
    # Row 1 vertical walls and cells
    print( '|', end='' )
    print( ' ', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '|', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( ' ', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '|', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( ' ', end='' )
    print( '|' )
    
    # Row 2 horizontal walls
    print( '+', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '-', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( '+', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '-', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( '+', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '-', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( '+' )
    
    # Row 2 vertical walls and cells
    print( '|', end='' )
    print( ' ', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '|', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( ' ', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '|', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( ' ', end='' )
    print( '|' )
    
    # Row 3 horizontal walls
    print( '+', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '-', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( '+', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '-', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( '+', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '-', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( '+' )
    
    # Row 3 vertical walls and cells
    print( '|', end='' )
    print( ' ', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '|', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( ' ', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '|', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( ' ', end='' )
    print( '|' )
    
    # Bottom border
    print( '+', end='' )
    print( '-', end='' )
    print( '+', end='' )
    print( '-', end='' )
    print( '+', end='' )
    print( '-', end='' )
    print( '+' )

    # Cell 1
    print( "\nRemoving wall from cell 1:" )
    
    # Remove wall code:
    # Remove a cell wall if possible for cell 1
    cellIndex = 1
    wallToRemove = 0
    
    wallRemoved = False
    direction = LEFT
    
    # If the wall in this direction is NOT an exterior wall
    if ( cellToWallLUT[cellIndex][direction] >= 0 ):
        wallToRemove = cellToWallLUT[cellIndex][direction]
        # If this wall has not been already removed
        if ( wallList[wallToRemove] == True ):
            # Remove the wall and indicate a wall was successfully removed
            wallList[wallToRemove] = False
            wallRemoved = True
    
    # If we haven't removed a wall yet, try to remove one in the next direction
    # Check the guard, have we removed one wall
    if ( wallRemoved == False ):
        direction = direction + 1
        # If the wall in this direction is NOT an exterior wall
        if ( cellToWallLUT[cellIndex][direction] >= 0 ):
            wallToRemove = cellToWallLUT[cellIndex][direction]
            # If this wall has not been already removed
            if ( wallList[wallToRemove] == True ):
                # Remove the wall and indicate a wall was successfully removed
                wallList[wallToRemove] = False
                wallRemoved = True
    
    # If we haven't removed a wall yet, try to remove one in the next direction
    # Check the guard, have we removed one wall
    if ( wallRemoved == False ):
        direction = direction + 1
        # If the wall in this direction is NOT an exterior wall
        if ( cellToWallLUT[cellIndex][direction] >= 0 ):
            wallToRemove = cellToWallLUT[cellIndex][direction]
            # If this wall has not been already removed
            if ( wallList[wallToRemove] == True ):
                # Remove the wall and indicate a wall was successfully removed
                wallList[wallToRemove] = False
                wallRemoved = True
    
    # If we haven't removed a wall yet, try to remove one in the next direction
    # Check the guard, have we removed one wall
    if ( wallRemoved == False ):
        direction = direction + 1
        # If the wall in this direction is NOT an exterior wall
        if ( cellToWallLUT[cellIndex][direction] >= 0 ):
            wallToRemove = cellToWallLUT[cellIndex][direction]
            # If this wall has not been already removed
            if ( wallList[wallToRemove] == True ):
                # Remove the wall and indicate a wall was successfully removed
                wallList[wallToRemove] = False
                wallRemoved = True
    
    # Print maze after cell 1
    time.sleep( 0.5 )
    
    # Print maze (duplicate of above print code)
    currentInteriorWall = 0
    
    # +-+-+-+
    print( '+', end='' )
    print( '-', end='' )
    print( '+', end='' )
    print( '-', end='' )
    print( '+', end='' )
    print( '-', end='' )
    print( '+' )
    
    # Row 1 vertical walls and cells
    print( '|', end='' )
    print( ' ', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '|', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( ' ', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '|', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( ' ', end='' )
    print( '|' )
    
    # Row 2 horizontal walls
    print( '+', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '-', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( '+', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '-', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( '+', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '-', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( '+' )
    
    # Row 2 vertical walls and cells
    print( '|', end='' )
    print( ' ', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '|', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( ' ', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '|', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( ' ', end='' )
    print( '|' )
    
    # Row 3 horizontal walls
    print( '+', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '-', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( '+', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '-', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( '+', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '-', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( '+' )
    
    # Row 3 vertical walls and cells
    print( '|', end='' )
    print( ' ', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '|', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( ' ', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '|', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( ' ', end='' )
    print( '|' )
    
    # Bottom border
    print( '+', end='' )
    print( '-', end='' )
    print( '+', end='' )
    print( '-', end='' )
    print( '+', end='' )
    print( '-', end='' )
    print( '+' )

    # Cell 2
    print( "\nRemoving wall from cell 2:" )
    
    # Remove wall code:
    # Remove a cell wall if possible for cell 2
    cellIndex = 2
    wallToRemove = 0
    
    wallRemoved = False
    direction = LEFT
    
    # If the wall in this direction is NOT an exterior wall
    if ( cellToWallLUT[cellIndex][direction] >= 0 ):
        wallToRemove = cellToWallLUT[cellIndex][direction]
        # If this wall has not been already removed
        if ( wallList[wallToRemove] == True ):
            # Remove the wall and indicate a wall was successfully removed
            wallList[wallToRemove] = False
            wallRemoved = True
    
    # If we haven't removed a wall yet, try to remove one in the next direction
    # Check the guard, have we removed one wall
    if ( wallRemoved == False ):
        direction = direction + 1
        # If the wall in this direction is NOT an exterior wall
        if ( cellToWallLUT[cellIndex][direction] >= 0 ):
            wallToRemove = cellToWallLUT[cellIndex][direction]
            # If this wall has not been already removed
            if ( wallList[wallToRemove] == True ):
                # Remove the wall and indicate a wall was successfully removed
                wallList[wallToRemove] = False
                wallRemoved = True
    
    # If we haven't removed a wall yet, try to remove one in the next direction
    # Check the guard, have we removed one wall
    if ( wallRemoved == False ):
        direction = direction + 1
        # If the wall in this direction is NOT an exterior wall
        if ( cellToWallLUT[cellIndex][direction] >= 0 ):
            wallToRemove = cellToWallLUT[cellIndex][direction]
            # If this wall has not been already removed
            if ( wallList[wallToRemove] == True ):
                # Remove the wall and indicate a wall was successfully removed
                wallList[wallToRemove] = False
                wallRemoved = True
    
    # If we haven't removed a wall yet, try to remove one in the next direction
    # Check the guard, have we removed one wall
    if ( wallRemoved == False ):
        direction = direction + 1
        # If the wall in this direction is NOT an exterior wall
        if ( cellToWallLUT[cellIndex][direction] >= 0 ):
            wallToRemove = cellToWallLUT[cellIndex][direction]
            # If this wall has not been already removed
            if ( wallList[wallToRemove] == True ):
                # Remove the wall and indicate a wall was successfully removed
                wallList[wallToRemove] = False
                wallRemoved = True
    
    # Print maze after cell 2
    time.sleep( 0.5 )
    
    # Print maze (duplicate of above print code)
    currentInteriorWall = 0
    
    # +-+-+-+
    print( '+', end='' )
    print( '-', end='' )
    print( '+', end='' )
    print( '-', end='' )
    print( '+', end='' )
    print( '-', end='' )
    print( '+' )
    
    # Row 1 vertical walls and cells
    print( '|', end='' )
    print( ' ', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '|', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( ' ', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '|', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( ' ', end='' )
    print( '|' )
    
    # Row 2 horizontal walls
    print( '+', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '-', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( '+', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '-', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( '+', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '-', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( '+' )
    
    # Row 2 vertical walls and cells
    print( '|', end='' )
    print( ' ', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '|', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( ' ', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '|', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( ' ', end='' )
    print( '|' )
    
    # Row 3 horizontal walls
    print( '+', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '-', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( '+', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '-', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( '+', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '-', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( '+' )
    
    # Row 3 vertical walls and cells
    print( '|', end='' )
    print( ' ', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '|', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( ' ', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '|', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( ' ', end='' )
    print( '|' )
    
    # Bottom border
    print( '+', end='' )
    print( '-', end='' )
    print( '+', end='' )
    print( '-', end='' )
    print( '+', end='' )
    print( '-', end='' )
    print( '+' )

    # Cell 3
    print( "\nRemoving wall from cell 3:" )
    
    # Remove wall code:
    # Remove a cell wall if possible for cell 3
    cellIndex = 3
    wallToRemove = 0
    
    wallRemoved = False
    direction = LEFT
    
    # If the wall in this direction is NOT an exterior wall
    if ( cellToWallLUT[cellIndex][direction] >= 0 ):
        wallToRemove = cellToWallLUT[cellIndex][direction]
        # If this wall has not been already removed
        if ( wallList[wallToRemove] == True ):
            # Remove the wall and indicate a wall was successfully removed
            wallList[wallToRemove] = False
            wallRemoved = True
    
    # If we haven't removed a wall yet, try to remove one in the next direction
    # Check the guard, have we removed one wall
    if ( wallRemoved == False ):
        direction = direction + 1
        # If the wall in this direction is NOT an exterior wall
        if ( cellToWallLUT[cellIndex][direction] >= 0 ):
            wallToRemove = cellToWallLUT[cellIndex][direction]
            # If this wall has not been already removed
            if ( wallList[wallToRemove] == True ):
                # Remove the wall and indicate a wall was successfully removed
                wallList[wallToRemove] = False
                wallRemoved = True
    
    # If we haven't removed a wall yet, try to remove one in the next direction
    # Check the guard, have we removed one wall
    if ( wallRemoved == False ):
        direction = direction + 1
        # If the wall in this direction is NOT an exterior wall
        if ( cellToWallLUT[cellIndex][direction] >= 0 ):
            wallToRemove = cellToWallLUT[cellIndex][direction]
            # If this wall has not been already removed
            if ( wallList[wallToRemove] == True ):
                # Remove the wall and indicate a wall was successfully removed
                wallList[wallToRemove] = False
                wallRemoved = True
    
    # If we haven't removed a wall yet, try to remove one in the next direction
    # Check the guard, have we removed one wall
    if ( wallRemoved == False ):
        direction = direction + 1
        # If the wall in this direction is NOT an exterior wall
        if ( cellToWallLUT[cellIndex][direction] >= 0 ):
            wallToRemove = cellToWallLUT[cellIndex][direction]
            # If this wall has not been already removed
            if ( wallList[wallToRemove] == True ):
                # Remove the wall and indicate a wall was successfully removed
                wallList[wallToRemove] = False
                wallRemoved = True
    
    # Print maze after cell 3
    time.sleep( 0.5 )
    
    # Print maze (duplicate of above print code)
    currentInteriorWall = 0
    
    # +-+-+-+
    print( '+', end='' )
    print( '-', end='' )
    print( '+', end='' )
    print( '-', end='' )
    print( '+', end='' )
    print( '-', end='' )
    print( '+' )
    
    # Row 1 vertical walls and cells
    print( '|', end='' )
    print( ' ', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '|', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( ' ', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '|', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( ' ', end='' )
    print( '|' )
    
    # Row 2 horizontal walls
    print( '+', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '-', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( '+', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '-', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( '+', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '-', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( '+' )
    
    # Row 2 vertical walls and cells
    print( '|', end='' )
    print( ' ', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '|', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( ' ', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '|', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( ' ', end='' )
    print( '|' )
    
    # Row 3 horizontal walls
    print( '+', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '-', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( '+', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '-', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( '+', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '-', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( '+' )
    
    # Row 3 vertical walls and cells
    print( '|', end='' )
    print( ' ', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '|', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( ' ', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '|', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( ' ', end='' )
    print( '|' )
    
    # Bottom border
    print( '+', end='' )
    print( '-', end='' )
    print( '+', end='' )
    print( '-', end='' )
    print( '+', end='' )
    print( '-', end='' )
    print( '+' )

    # Cell 4
    print( "\nRemoving wall from cell 4:" )
    
    # Remove wall code:
    # Remove a cell wall if possible for cell 4
    cellIndex = 4
    wallToRemove = 0
    
    wallRemoved = False
    direction = LEFT
    
    # If the wall in this direction is NOT an exterior wall
    if ( cellToWallLUT[cellIndex][direction] >= 0 ):
        wallToRemove = cellToWallLUT[cellIndex][direction]
        # If this wall has not been already removed
        if ( wallList[wallToRemove] == True ):
            # Remove the wall and indicate a wall was successfully removed
            wallList[wallToRemove] = False
            wallRemoved = True
    
    # If we haven't removed a wall yet, try to remove one in the next direction
    # Check the guard, have we removed one wall
    if ( wallRemoved == False ):
        direction = direction + 1
        # If the wall in this direction is NOT an exterior wall
        if ( cellToWallLUT[cellIndex][direction] >= 0 ):
            wallToRemove = cellToWallLUT[cellIndex][direction]
            # If this wall has not been already removed
            if ( wallList[wallToRemove] == True ):
                # Remove the wall and indicate a wall was successfully removed
                wallList[wallToRemove] = False
                wallRemoved = True
    
    # If we haven't removed a wall yet, try to remove one in the next direction
    # Check the guard, have we removed one wall
    if ( wallRemoved == False ):
        direction = direction + 1
        # If the wall in this direction is NOT an exterior wall
        if ( cellToWallLUT[cellIndex][direction] >= 0 ):
            wallToRemove = cellToWallLUT[cellIndex][direction]
            # If this wall has not been already removed
            if ( wallList[wallToRemove] == True ):
                # Remove the wall and indicate a wall was successfully removed
                wallList[wallToRemove] = False
                wallRemoved = True
    
    # If we haven't removed a wall yet, try to remove one in the next direction
    # Check the guard, have we removed one wall
    if ( wallRemoved == False ):
        direction = direction + 1
        # If the wall in this direction is NOT an exterior wall
        if ( cellToWallLUT[cellIndex][direction] >= 0 ):
            wallToRemove = cellToWallLUT[cellIndex][direction]
            # If this wall has not been already removed
            if ( wallList[wallToRemove] == True ):
                # Remove the wall and indicate a wall was successfully removed
                wallList[wallToRemove] = False
                wallRemoved = True
    
    # Print maze after cell 4
    time.sleep( 0.5 )
    
    # Print maze (duplicate of above print code)
    currentInteriorWall = 0
    
    # +-+-+-+
    print( '+', end='' )
    print( '-', end='' )
    print( '+', end='' )
    print( '-', end='' )
    print( '+', end='' )
    print( '-', end='' )
    print( '+' )
    
    # Row 1 vertical walls and cells
    print( '|', end='' )
    print( ' ', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '|', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( ' ', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '|', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( ' ', end='' )
    print( '|' )
    
    # Row 2 horizontal walls
    print( '+', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '-', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( '+', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '-', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( '+', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '-', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( '+' )
    
    # Row 2 vertical walls and cells
    print( '|', end='' )
    print( ' ', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '|', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( ' ', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '|', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( ' ', end='' )
    print( '|' )
    
    # Row 3 horizontal walls
    print( '+', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '-', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( '+', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '-', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( '+', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '-', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( '+' )
    
    # Row 3 vertical walls and cells
    print( '|', end='' )
    print( ' ', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '|', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( ' ', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '|', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( ' ', end='' )
    print( '|' )
    
    # Bottom border
    print( '+', end='' )
    print( '-', end='' )
    print( '+', end='' )
    print( '-', end='' )
    print( '+', end='' )
    print( '-', end='' )
    print( '+' )

    # Cell 5
    print( "\nRemoving wall from cell 5:" )
    
    # Remove wall code:
    # Remove a cell wall if possible for cell 5
    cellIndex = 5
    wallToRemove = 0
    
    wallRemoved = False
    direction = LEFT
    
    # If the wall in this direction is NOT an exterior wall
    if ( cellToWallLUT[cellIndex][direction] >= 0 ):
        wallToRemove = cellToWallLUT[cellIndex][direction]
        # If this wall has not been already removed
        if ( wallList[wallToRemove] == True ):
            # Remove the wall and indicate a wall was successfully removed
            wallList[wallToRemove] = False
            wallRemoved = True
    
    # If we haven't removed a wall yet, try to remove one in the next direction
    # Check the guard, have we removed one wall
    if ( wallRemoved == False ):
        direction = direction + 1
        # If the wall in this direction is NOT an exterior wall
        if ( cellToWallLUT[cellIndex][direction] >= 0 ):
            wallToRemove = cellToWallLUT[cellIndex][direction]
            # If this wall has not been already removed
            if ( wallList[wallToRemove] == True ):
                # Remove the wall and indicate a wall was successfully removed
                wallList[wallToRemove] = False
                wallRemoved = True
    
    # If we haven't removed a wall yet, try to remove one in the next direction
    # Check the guard, have we removed one wall
    if ( wallRemoved == False ):
        direction = direction + 1
        # If the wall in this direction is NOT an exterior wall
        if ( cellToWallLUT[cellIndex][direction] >= 0 ):
            wallToRemove = cellToWallLUT[cellIndex][direction]
            # If this wall has not been already removed
            if ( wallList[wallToRemove] == True ):
                # Remove the wall and indicate a wall was successfully removed
                wallList[wallToRemove] = False
                wallRemoved = True
    
    # If we haven't removed a wall yet, try to remove one in the next direction
    # Check the guard, have we removed one wall
    if ( wallRemoved == False ):
        direction = direction + 1
        # If the wall in this direction is NOT an exterior wall
        if ( cellToWallLUT[cellIndex][direction] >= 0 ):
            wallToRemove = cellToWallLUT[cellIndex][direction]
            # If this wall has not been already removed
            if ( wallList[wallToRemove] == True ):
                # Remove the wall and indicate a wall was successfully removed
                wallList[wallToRemove] = False
                wallRemoved = True
    
    # Print maze after cell 5
    time.sleep( 0.5 )
    
    # Print maze (duplicate of above print code)
    currentInteriorWall = 0
    
    # +-+-+-+
    print( '+', end='' )
    print( '-', end='' )
    print( '+', end='' )
    print( '-', end='' )
    print( '+', end='' )
    print( '-', end='' )
    print( '+' )
    
    # Row 1 vertical walls and cells
    print( '|', end='' )
    print( ' ', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '|', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( ' ', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '|', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( ' ', end='' )
    print( '|' )
    
    # Row 2 horizontal walls
    print( '+', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '-', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( '+', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '-', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( '+', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '-', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( '+' )
    
    # Row 2 vertical walls and cells
    print( '|', end='' )
    print( ' ', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '|', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( ' ', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '|', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( ' ', end='' )
    print( '|' )
    
    # Row 3 horizontal walls
    print( '+', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '-', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( '+', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '-', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( '+', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '-', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( '+' )
    
    # Row 3 vertical walls and cells
    print( '|', end='' )
    print( ' ', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '|', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( ' ', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '|', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( ' ', end='' )
    print( '|' )
    
    # Bottom border
    print( '+', end='' )
    print( '-', end='' )
    print( '+', end='' )
    print( '-', end='' )
    print( '+', end='' )
    print( '-', end='' )
    print( '+' )

    # Cell 6
    print( "\nRemoving wall from cell 6:" )
    
    # Remove wall code:
    # Remove a cell wall if possible for cell 6
    cellIndex = 6
    wallToRemove = 0
    
    wallRemoved = False
    direction = LEFT
    
    # If the wall in this direction is NOT an exterior wall
    if ( cellToWallLUT[cellIndex][direction] >= 0 ):
        wallToRemove = cellToWallLUT[cellIndex][direction]
        # If this wall has not been already removed
        if ( wallList[wallToRemove] == True ):
            # Remove the wall and indicate a wall was successfully removed
            wallList[wallToRemove] = False
            wallRemoved = True
    
    # If we haven't removed a wall yet, try to remove one in the next direction
    # Check the guard, have we removed one wall
    if ( wallRemoved == False ):
        direction = direction + 1
        # If the wall in this direction is NOT an exterior wall
        if ( cellToWallLUT[cellIndex][direction] >= 0 ):
            wallToRemove = cellToWallLUT[cellIndex][direction]
            # If this wall has not been already removed
            if ( wallList[wallToRemove] == True ):
                # Remove the wall and indicate a wall was successfully removed
                wallList[wallToRemove] = False
                wallRemoved = True
    
    # If we haven't removed a wall yet, try to remove one in the next direction
    # Check the guard, have we removed one wall
    if ( wallRemoved == False ):
        direction = direction + 1
        # If the wall in this direction is NOT an exterior wall
        if ( cellToWallLUT[cellIndex][direction] >= 0 ):
            wallToRemove = cellToWallLUT[cellIndex][direction]
            # If this wall has not been already removed
            if ( wallList[wallToRemove] == True ):
                # Remove the wall and indicate a wall was successfully removed
                wallList[wallToRemove] = False
                wallRemoved = True
    
    # If we haven't removed a wall yet, try to remove one in the next direction
    # Check the guard, have we removed one wall
    if ( wallRemoved == False ):
        direction = direction + 1
        # If the wall in this direction is NOT an exterior wall
        if ( cellToWallLUT[cellIndex][direction] >= 0 ):
            wallToRemove = cellToWallLUT[cellIndex][direction]
            # If this wall has not been already removed
            if ( wallList[wallToRemove] == True ):
                # Remove the wall and indicate a wall was successfully removed
                wallList[wallToRemove] = False
                wallRemoved = True
    
    # Print maze after cell 6
    time.sleep( 0.5 )
    
    # Print maze (duplicate of above print code)
    currentInteriorWall = 0
    
    # +-+-+-+
    print( '+', end='' )
    print( '-', end='' )
    print( '+', end='' )
    print( '-', end='' )
    print( '+', end='' )
    print( '-', end='' )
    print( '+' )
    
    # Row 1 vertical walls and cells
    print( '|', end='' )
    print( ' ', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '|', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( ' ', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '|', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( ' ', end='' )
    print( '|' )
    
    # Row 2 horizontal walls
    print( '+', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '-', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( '+', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '-', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( '+', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '-', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( '+' )
    
    # Row 2 vertical walls and cells
    print( '|', end='' )
    print( ' ', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '|', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( ' ', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '|', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( ' ', end='' )
    print( '|' )
    
    # Row 3 horizontal walls
    print( '+', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '-', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( '+', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '-', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( '+', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '-', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( '+' )
    
    # Row 3 vertical walls and cells
    print( '|', end='' )
    print( ' ', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '|', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( ' ', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '|', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( ' ', end='' )
    print( '|' )
    
    # Bottom border
    print( '+', end='' )
    print( '-', end='' )
    print( '+', end='' )
    print( '-', end='' )
    print( '+', end='' )
    print( '-', end='' )
    print( '+' )

    # Cell 7
    print( "\nRemoving wall from cell 7:" )
    
    # Remove wall code:
    # Remove a cell wall if possible for cell 7
    cellIndex = 7
    wallToRemove = 0
    
    wallRemoved = False
    direction = LEFT
    
    # If the wall in this direction is NOT an exterior wall
    if ( cellToWallLUT[cellIndex][direction] >= 0 ):
        wallToRemove = cellToWallLUT[cellIndex][direction]
        # If this wall has not been already removed
        if ( wallList[wallToRemove] == True ):
            # Remove the wall and indicate a wall was successfully removed
            wallList[wallToRemove] = False
            wallRemoved = True
    
    # If we haven't removed a wall yet, try to remove one in the next direction
    # Check the guard, have we removed one wall
    if ( wallRemoved == False ):
        direction = direction + 1
        # If the wall in this direction is NOT an exterior wall
        if ( cellToWallLUT[cellIndex][direction] >= 0 ):
            wallToRemove = cellToWallLUT[cellIndex][direction]
            # If this wall has not been already removed
            if ( wallList[wallToRemove] == True ):
                # Remove the wall and indicate a wall was successfully removed
                wallList[wallToRemove] = False
                wallRemoved = True
    
    # If we haven't removed a wall yet, try to remove one in the next direction
    # Check the guard, have we removed one wall
    if ( wallRemoved == False ):
        direction = direction + 1
        # If the wall in this direction is NOT an exterior wall
        if ( cellToWallLUT[cellIndex][direction] >= 0 ):
            wallToRemove = cellToWallLUT[cellIndex][direction]
            # If this wall has not been already removed
            if ( wallList[wallToRemove] == True ):
                # Remove the wall and indicate a wall was successfully removed
                wallList[wallToRemove] = False
                wallRemoved = True
    
    # If we haven't removed a wall yet, try to remove one in the next direction
    # Check the guard, have we removed one wall
    if ( wallRemoved == False ):
        direction = direction + 1
        # If the wall in this direction is NOT an exterior wall
        if ( cellToWallLUT[cellIndex][direction] >= 0 ):
            wallToRemove = cellToWallLUT[cellIndex][direction]
            # If this wall has not been already removed
            if ( wallList[wallToRemove] == True ):
                # Remove the wall and indicate a wall was successfully removed
                wallList[wallToRemove] = False
                wallRemoved = True
    
    # Print maze after cell 7
    time.sleep( 0.5 )
    
    # Print maze (duplicate of above print code)
    currentInteriorWall = 0
    
    # +-+-+-+
    print( '+', end='' )
    print( '-', end='' )
    print( '+', end='' )
    print( '-', end='' )
    print( '+', end='' )
    print( '-', end='' )
    print( '+' )
    
    # Row 1 vertical walls and cells
    print( '|', end='' )
    print( ' ', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '|', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( ' ', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '|', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( ' ', end='' )
    print( '|' )
    
    # Row 2 horizontal walls
    print( '+', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '-', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( '+', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '-', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( '+', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '-', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( '+' )
    
    # Row 2 vertical walls and cells
    print( '|', end='' )
    print( ' ', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '|', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( ' ', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '|', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( ' ', end='' )
    print( '|' )
    
    # Row 3 horizontal walls
    print( '+', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '-', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( '+', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '-', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( '+', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '-', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( '+' )
    
    # Row 3 vertical walls and cells
    print( '|', end='' )
    print( ' ', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '|', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( ' ', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '|', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( ' ', end='' )
    print( '|' )
    
    # Bottom border
    print( '+', end='' )
    print( '-', end='' )
    print( '+', end='' )
    print( '-', end='' )
    print( '+', end='' )
    print( '-', end='' )
    print( '+' )

    # Cell 8
    print( "\nRemoving wall from cell 8:" )
    
    # Remove wall code:
    # Remove a cell wall if possible for cell 8
    cellIndex = 8
    wallToRemove = 0
    
    wallRemoved = False
    direction = LEFT
    
    # If the wall in this direction is NOT an exterior wall
    if ( cellToWallLUT[cellIndex][direction] >= 0 ):
        wallToRemove = cellToWallLUT[cellIndex][direction]
        # If this wall has not been already removed
        if ( wallList[wallToRemove] == True ):
            # Remove the wall and indicate a wall was successfully removed
            wallList[wallToRemove] = False
            wallRemoved = True
    
    # If we haven't removed a wall yet, try to remove one in the next direction
    # Check the guard, have we removed one wall
    if ( wallRemoved == False ):
        direction = direction + 1
        # If the wall in this direction is NOT an exterior wall
        if ( cellToWallLUT[cellIndex][direction] >= 0 ):
            wallToRemove = cellToWallLUT[cellIndex][direction]
            # If this wall has not been already removed
            if ( wallList[wallToRemove] == True ):
                # Remove the wall and indicate a wall was successfully removed
                wallList[wallToRemove] = False
                wallRemoved = True
    
    # If we haven't removed a wall yet, try to remove one in the next direction
    # Check the guard, have we removed one wall
    if ( wallRemoved == False ):
        direction = direction + 1
        # If the wall in this direction is NOT an exterior wall
        if ( cellToWallLUT[cellIndex][direction] >= 0 ):
            wallToRemove = cellToWallLUT[cellIndex][direction]
            # If this wall has not been already removed
            if ( wallList[wallToRemove] == True ):
                # Remove the wall and indicate a wall was successfully removed
                wallList[wallToRemove] = False
                wallRemoved = True
    
    # If we haven't removed a wall yet, try to remove one in the next direction
    # Check the guard, have we removed one wall
    if ( wallRemoved == False ):
        direction = direction + 1
        # If the wall in this direction is NOT an exterior wall
        if ( cellToWallLUT[cellIndex][direction] >= 0 ):
            wallToRemove = cellToWallLUT[cellIndex][direction]
            # If this wall has not been already removed
            if ( wallList[wallToRemove] == True ):
                # Remove the wall and indicate a wall was successfully removed
                wallList[wallToRemove] = False
                wallRemoved = True
    
    # Print maze after cell 8
    time.sleep( 0.5 )
    
    # Print maze (duplicate of above print code)
    currentInteriorWall = 0
    
    # +-+-+-+
    print( '+', end='' )
    print( '-', end='' )
    print( '+', end='' )
    print( '-', end='' )
    print( '+', end='' )
    print( '-', end='' )
    print( '+' )
    
    # Row 1 vertical walls and cells
    print( '|', end='' )
    print( ' ', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '|', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( ' ', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '|', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( ' ', end='' )
    print( '|' )
    
    # Row 2 horizontal walls
    print( '+', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '-', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( '+', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '-', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( '+', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '-', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( '+' )
    
    # Row 2 vertical walls and cells
    print( '|', end='' )
    print( ' ', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '|', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( ' ', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '|', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( ' ', end='' )
    print( '|' )
    
    # Row 3 horizontal walls
    print( '+', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '-', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( '+', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '-', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( '+', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '-', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( '+' )
    
    # Row 3 vertical walls and cells
    print( '|', end='' )
    print( ' ', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '|', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( ' ', end='' )
    if ( wallList[currentInteriorWall] == True ):
        print( '|', end='' )
    else:
        print( ' ', end='' )
    currentInteriorWall = currentInteriorWall + 1
    print( ' ', end='' )
    print( '|' )
    
    # Bottom border
    print( '+', end='' )
    print( '-', end='' )
    print( '+', end='' )
    print( '-', end='' )
    print( '+', end='' )
    print( '-', end='' )
    print( '+' )

if __name__ == "__main__":
    main()
