section .data
    ; Define directions
    LEFT equ 0
    UP equ 1
    RIGHT equ 2
    DOWN equ 3
    
    FIRST_CELL equ 0
    SECOND_CELL equ 1
    
    NO_CELL equ -1
    
    ; Algorithm constants
    KRUSKAL_ALGORITHM equ 1
    PRIM_ALGORITHM equ 2
    DEPTH_FIRST_ALGORITHM equ 3
    BINARY_TREE_ALGORITHM equ 4
    RECURSIVE_DIVISION_ALGORITHM equ 5
    
    ; Maximum sizes to avoid dynamic allocation complexity
    MAX_SIZE equ 20
    MAX_WALLS equ 800        ; 20*19 + 19*20 = 760 walls max
    MAX_CELLS equ 400        ; 20*20 = 400 cells max
    
    ; Global arrays
    AllHorizontalWallsUp_Glob times MAX_SIZE db 1
    WallsUp_Glob times MAX_WALLS db 1
    WallToCells_Glob times MAX_WALLS*2 dd 0    ; 2 integers per wall
    CellToWalls_Glob times MAX_CELLS*4 dd NO_CELL    ; 4 directions per cell
    
    ; Algorithm state globals
    CellVisited_Glob times MAX_CELLS db 0
    FrontierWalls_Glob times MAX_WALLS dd 0
    CellInMaze_Glob times MAX_CELLS db 0
    cellStack times MAX_CELLS dd 0
    
    ; Kruskal specific arrays
    wallRemoveList times MAX_WALLS dd 0
    cellToGroup times MAX_CELLS dd 0
    groupCells times MAX_CELLS*MAX_CELLS dd NO_CELL
    
    ; Sleep timespec structure
    sleepTime:
        dq 0, 500000000     ; 0.5 seconds
    
    ; Output characters
    plus_char db "+"
    dash_char db "-"
    pipe_char db "|"
    space_char db " "
    newline_char db 10
    
    ; Input prompts
    prompt_cols db "Please enter number of columns for maze, must be greater than 1: "
    prompt_cols_len equ $ - prompt_cols
    prompt_rows db "Please enter number of rows for maze, must be greater than 1: "
    prompt_rows_len equ $ - prompt_rows
    prompt_algorithm db "Please choose maze generation algorithm:", 10
                    db "1 - Kruskal's Algorithm", 10
                    db "2 - Prim's Algorithm", 10
                    db "3 - Depth-First Search", 10
                    db "4 - Binary Tree Algorithm", 10
                    db "5 - Recursive Division Algorithm", 10
    prompt_algorithm_len equ $ - prompt_algorithm

section .bss
    ; Global variables - matching C++ names exactly
    MazeColumns_Glob resd 1
    MazeRows_Glob resd 1
    InteriorWallCount_Glob resd 1
    FrontierWallCount_Glob resd 1
    algorithmChoice resd 1
    
    ; Loop counters matching C++ variable names  
    rowIndex resd 1
    columnIndex resd 1
    horizontalWallIndex resd 1
    verticalWallIndex resd 1
    interiorWallIndex resd 1
    wallIndex resd 1
    cellIndex resd 1
    
    ; Common variables for all algorithms
    firstCellInRow resd 1
    leftCell resd 1
    rightCell resd 1
    upperCell resd 1
    lowerCell resd 1
    firstCellIndex resd 1
    secondCellIndex resd 1
    currentCellIndex resd 1
    nextCellIndex resd 1
    
    ; More variables
    removeWallIndex resd 1
    nextWallToCheck resd 1
    firstCellGroupIndex resd 1
    secondCellGroupIndex resd 1
    totalCells resd 1
    mazeComplete resd 1
    cellsInMaze resd 1
    randomWallIndex resd 1
    wallToCheck resd 1
    outerCellIndex resd 1
    stackSize resd 1
    foundNeighbor resd 1
    directionIndex resd 1
    direction resd 1
    validWallCount resd 1
    wallToRemove resd 1
    startRow resd 1
    endRow resd 1
    startCol resd 1
    endCol resd 1
    height resd 1
    width resd 1
    divideHorizontally resd 1
    divideRow resd 1
    divideCol resd 1
    gapCol resd 1
    gapRow resd 1
    gapCellIndex resd 1
    
    ; Random seed and temp variables
    randomSeed resd 1
    temp resd 8          ; Array for temporary storage
    shuffleIndex resd 1
    randomIndex resd 1
    
    ; Input buffer
    inputBuffer resb 20

section .text
    global _start

_start:
    ; Initialize random seed first
    mov rax, 201        ; sys_time
    mov rdi, 0
    syscall
    mov [randomSeed], eax
    
    call getUserInput
    call setupMaze
    call initializeLookupTables
    call printMaze
    
    ; Execute chosen algorithm
    mov eax, [algorithmChoice]
    cmp eax, KRUSKAL_ALGORITHM
    je .kruskal
    cmp eax, PRIM_ALGORITHM
    je .prim
    cmp eax, DEPTH_FIRST_ALGORITHM
    je .depthFirst
    cmp eax, BINARY_TREE_ALGORITHM
    je .binaryTree
    cmp eax, RECURSIVE_DIVISION_ALGORITHM
    je .recursiveDivision
    jmp .exit

.kruskal:
    call buildMazeKruskal
    jmp .exit
.prim:
    call buildMazePrim
    jmp .exit
.depthFirst:
    call buildMazeDepthFirst
    jmp .exit
.binaryTree:
    call buildMazeBinaryTree
    jmp .exit
.recursiveDivision:
    call buildMazeRecursiveDivision
    jmp .exit
    
.exit:
    ; Exit
    mov rax, 60
    mov rdi, 0
    syscall

; Get user input for maze dimensions and algorithm
getUserInput:
    ; Get columns
.getColumns:
    mov rax, 1
    mov rdi, 1
    mov rsi, prompt_cols
    mov rdx, prompt_cols_len
    syscall
    
    call readInteger
    cmp eax, 1
    jle .getColumns
    cmp eax, MAX_SIZE
    jg .getColumns
    mov [MazeColumns_Glob], eax
    
    ; Get rows
.getRows:
    mov rax, 1
    mov rdi, 1
    mov rsi, prompt_rows
    mov rdx, prompt_rows_len
    syscall
    
    call readInteger
    cmp eax, 1
    jle .getRows
    cmp eax, MAX_SIZE
    jg .getRows
    mov [MazeRows_Glob], eax
    
    ; Get algorithm choice
.getAlgorithm:
    mov rax, 1
    mov rdi, 1
    mov rsi, prompt_algorithm
    mov rdx, prompt_algorithm_len
    syscall
    
    call readInteger
    cmp eax, 1
    jl .getAlgorithm
    cmp eax, 5
    jg .getAlgorithm
    mov [algorithmChoice], eax
    
    ret

; Setup maze data structures
setupMaze:
    ; Calculate interior wall count: rows * (cols-1) + (rows-1) * cols
    mov eax, [MazeRows_Glob]
    mov ebx, [MazeColumns_Glob]
    dec ebx
    mul ebx             ; eax = rows * (cols - 1)
    mov [InteriorWallCount_Glob], eax
    
    mov eax, [MazeRows_Glob]
    dec eax
    mov ebx, [MazeColumns_Glob]
    mul ebx             ; eax = (rows - 1) * cols
    add eax, [InteriorWallCount_Glob]
    mov [InteriorWallCount_Glob], eax
    
    ; Calculate total cells
    mov eax, [MazeRows_Glob]
    mul dword [MazeColumns_Glob]
    mov [totalCells], eax
    
    ; Initialize AllHorizontalWallsUp_Glob
    mov dword [columnIndex], 0
.initHorizontalLoop:
    mov eax, [columnIndex]
    cmp eax, [MazeColumns_Glob]
    jge .initWallsUp
    
    mov byte [AllHorizontalWallsUp_Glob + eax], 1
    inc dword [columnIndex]
    jmp .initHorizontalLoop
    
.initWallsUp:
    ; Initialize WallsUp_Glob - all walls up initially
    mov dword [wallIndex], 0
.initWallsLoop:
    mov eax, [wallIndex]
    cmp eax, [InteriorWallCount_Glob]
    jge .initAlgorithmState
    
    mov byte [WallsUp_Glob + eax], 1
    inc dword [wallIndex]
    jmp .initWallsLoop
    
.initAlgorithmState:
    ; Initialize algorithm state globals
    mov dword [cellIndex], 0
.initStateLoop:
    mov eax, [cellIndex]
    cmp eax, [totalCells]
    jge .done
    
    mov byte [CellVisited_Glob + eax], 0
    mov byte [CellInMaze_Glob + eax], 0
    inc dword [cellIndex]
    jmp .initStateLoop
    
.done:
    ret

; Initialize lookup tables (WallToCells_Glob and CellToWalls_Glob)
initializeLookupTables:
    ; First, initialize CellToWalls_Glob to NO_CELL
    mov dword [cellIndex], 0
.initCellToWalls:
    mov eax, [cellIndex]
    cmp eax, [totalCells]
    jge .buildWallToCells
    
    ; Initialize all directions to NO_CELL
    mov eax, [cellIndex]
    shl eax, 4          ; multiply by 16 (4 * 4 bytes)
    mov dword [CellToWalls_Glob + eax + LEFT*4], NO_CELL
    mov dword [CellToWalls_Glob + eax + UP*4], NO_CELL
    mov dword [CellToWalls_Glob + eax + RIGHT*4], NO_CELL
    mov dword [CellToWalls_Glob + eax + DOWN*4], NO_CELL
    
    inc dword [cellIndex]
    jmp .initCellToWalls
    
.buildWallToCells:
    ; Build WallToCells_Glob
    mov dword [wallIndex], 0
    mov dword [rowIndex], 0
    
.rowLoop:
    mov eax, [rowIndex]
    cmp eax, [MazeRows_Glob]
    jge .buildCellToWalls
    
    ; Calculate first cell in row = rowIndex * MazeColumns_Glob
    mov eax, [rowIndex]
    mul dword [MazeColumns_Glob]
    mov [firstCellInRow], eax
    
    ; Vertical walls
    mov dword [verticalWallIndex], 0
.verticalLoop:
    mov eax, [verticalWallIndex]
    mov ebx, [MazeColumns_Glob]
    dec ebx
    cmp eax, ebx
    jge .horizontalWalls
    
    ; leftCell = firstCellInRow + verticalWallIndex
    mov eax, [firstCellInRow]
    add eax, [verticalWallIndex]
    mov [leftCell], eax
    
    ; rightCell = leftCell + 1
    inc eax
    mov [rightCell], eax
    
    ; Store connection: WallToCells_Glob[wallIndex][0] = leftCell
    mov eax, [wallIndex]
    shl eax, 3          ; multiply by 8 (2 * 4 bytes)
    mov ebx, [leftCell]
    mov [WallToCells_Glob + eax], ebx
    mov ebx, [rightCell]
    mov [WallToCells_Glob + eax + 4], ebx
    
    ; Also update CellToWalls_Glob
    ; leftCell -> RIGHT direction
    mov eax, [leftCell]
    shl eax, 4
    mov ebx, [wallIndex]
    mov [CellToWalls_Glob + eax + RIGHT*4], ebx
    
    ; rightCell -> LEFT direction  
    mov eax, [rightCell]
    shl eax, 4
    mov ebx, [wallIndex]
    mov [CellToWalls_Glob + eax + LEFT*4], ebx
    
    inc dword [wallIndex]
    inc dword [verticalWallIndex]
    jmp .verticalLoop
    
.horizontalWalls:
    ; Check if we have more walls to process
    mov eax, [wallIndex]
    cmp eax, [InteriorWallCount_Glob]
    jge .nextRow
    
    ; Horizontal walls
    mov dword [horizontalWallIndex], 0
.horizontalLoop:
    mov eax, [horizontalWallIndex]
    cmp eax, [MazeColumns_Glob]
    jge .nextRow
    
    ; upperCell = firstCellInRow + horizontalWallIndex
    mov eax, [firstCellInRow]
    add eax, [horizontalWallIndex]
    mov [upperCell], eax
    
    ; lowerCell = upperCell + MazeColumns_Glob
    add eax, [MazeColumns_Glob]
    mov [lowerCell], eax
    
    ; Store connection
    mov eax, [wallIndex]
    shl eax, 3          ; multiply by 8
    mov ebx, [upperCell]
    mov [WallToCells_Glob + eax], ebx
    mov ebx, [lowerCell]
    mov [WallToCells_Glob + eax + 4], ebx
    
    ; Also update CellToWalls_Glob
    ; upperCell -> DOWN direction
    mov eax, [upperCell]
    shl eax, 4
    mov ebx, [wallIndex]
    mov [CellToWalls_Glob + eax + DOWN*4], ebx
    
    ; lowerCell -> UP direction
    mov eax, [lowerCell]
    shl eax, 4
    mov ebx, [wallIndex]
    mov [CellToWalls_Glob + eax + UP*4], ebx
    
    inc dword [wallIndex]
    inc dword [horizontalWallIndex]
    jmp .horizontalLoop
    
.nextRow:
    inc dword [rowIndex]
    jmp .rowLoop
    
.buildCellToWalls:
    ret

; Generate random number using linear congruential generator
getRandom:
    mov eax, [randomSeed]
    mov ebx, 1103515245
    mul ebx
    add eax, 12345
    mov [randomSeed], eax
    ret

; Sleep for half second
sleepHalfSecond:
    mov rax, 35         ; sys_nanosleep
    mov rdi, sleepTime
    mov rsi, 0
    syscall
    ret

; Kruskal's algorithm
buildMazeKruskal:
    ; Initialize cell groups
    mov dword [cellIndex], 0
.initCellToGroup:
    mov eax, [cellIndex]
    cmp eax, [totalCells]
    jge .initWallList
    
    mov [cellToGroup + eax*4], eax
    inc dword [cellIndex]
    jmp .initCellToGroup
    
.initWallList:
    ; Initialize wall remove list
    mov dword [wallIndex], 0
.initWallLoop:
    mov eax, [wallIndex]
    cmp eax, [InteriorWallCount_Glob]
    jge .shuffleWalls
    
    mov [wallRemoveList + eax*4], eax
    inc dword [wallIndex]
    jmp .initWallLoop
    
.shuffleWalls:
    ; Shuffle wall list using Fisher-Yates
    mov eax, [InteriorWallCount_Glob]
    dec eax
    mov [shuffleIndex], eax
    
.shuffleLoop:
    mov eax, [shuffleIndex]
    cmp eax, 1
    jle .performAlgorithm
    
    ; Generate random index 0..shuffleIndex
    call getRandom
    mov eax, [randomSeed]
    xor edx, edx
    mov ebx, [shuffleIndex]
    inc ebx
    div ebx             ; edx = random % (shuffleIndex + 1)
    mov [randomIndex], edx
    
    ; Swap wallRemoveList[shuffleIndex] with wallRemoveList[randomIndex]
    mov eax, [shuffleIndex]
    mov ebx, [wallRemoveList + eax*4]
    mov ecx, [randomIndex]
    mov edx, [wallRemoveList + ecx*4]
    mov [wallRemoveList + eax*4], edx
    mov [wallRemoveList + ecx*4], ebx
    
    dec dword [shuffleIndex]
    jmp .shuffleLoop
    
.performAlgorithm:
    ; Perform Kruskal's algorithm main loop
    mov dword [removeWallIndex], 0
    
.algorithmLoop:
    mov eax, [removeWallIndex]
    cmp eax, [InteriorWallCount_Glob]
    jge .done
    
    ; Get next wall to check
    mov eax, [removeWallIndex]
    mov ebx, [wallRemoveList + eax*4]
    mov [nextWallToCheck], ebx
    
    ; Get connected cells
    shl ebx, 3          ; multiply by 8
    mov eax, [WallToCells_Glob + ebx]
    mov [firstCellIndex], eax
    mov eax, [WallToCells_Glob + ebx + 4]
    mov [secondCellIndex], eax
    
    ; Get group indices
    mov eax, [firstCellIndex]
    mov ebx, [cellToGroup + eax*4]
    mov [firstCellGroupIndex], ebx
    
    mov eax, [secondCellIndex]
    mov ebx, [cellToGroup + eax*4]
    mov [secondCellGroupIndex], ebx
    
    ; Check if different groups
    mov eax, [firstCellGroupIndex]
    cmp eax, [secondCellGroupIndex]
    je .nextWall
    
    ; Remove wall
    mov eax, [nextWallToCheck]
    mov byte [WallsUp_Glob + eax], 0
    
    ; Merge groups - update all cells in second group to point to first group
    mov dword [cellIndex], 0
.updateLoop:
    mov eax, [cellIndex]
    cmp eax, [totalCells]
    jge .wallRemoved
    
    ; Check if this cell belongs to second group
    mov ebx, [cellToGroup + eax*4]
    cmp ebx, [secondCellGroupIndex]
    jne .nextCellUpdate
    
    ; Update to first group
    mov ebx, [firstCellGroupIndex]
    mov [cellToGroup + eax*4], ebx
    
.nextCellUpdate:
    inc dword [cellIndex]
    jmp .updateLoop
    
.wallRemoved:
    ; Sleep and print maze
    call sleepHalfSecond
    call printMaze
    
.nextWall:
    inc dword [removeWallIndex]
    jmp .algorithmLoop
    
.done:
    ret

; Prim's algorithm - fixed version
buildMazePrim:
    ; Initialize algorithm state
    mov dword [FrontierWallCount_Glob], 0
    mov dword [cellsInMaze], 0
    
    ; Clear CellInMaze_Glob array
    mov dword [cellIndex], 0
.clearCellsInMaze:
    mov eax, [cellIndex]
    cmp eax, [totalCells]
    jge .startPrim
    
    mov byte [CellInMaze_Glob + eax], 0
    inc dword [cellIndex]
    jmp .clearCellsInMaze
    
.startPrim:
    ; Start with a random cell
    call getRandom
    mov eax, [randomSeed]
    xor edx, edx
    div dword [totalCells]
    mov [currentCellIndex], edx
    
    ; Mark start cell as in maze
    mov eax, [currentCellIndex]
    mov byte [CellInMaze_Glob + eax], 1
    inc dword [cellsInMaze]
    
    ; Add walls adjacent to start cell to frontier
    call addCellWallsToFrontier
    
.mainLoop:
    mov eax, [cellsInMaze]
    cmp eax, [totalCells]
    jge .done
    
    cmp dword [FrontierWallCount_Glob], 0
    jle .done
    
    ; Pick a random wall from frontier
    call getRandom
    mov eax, [randomSeed]
    xor edx, edx
    div dword [FrontierWallCount_Glob]
    mov [randomWallIndex], edx
    
    ; Get the wall to check
    mov eax, [randomWallIndex]
    mov ebx, [FrontierWalls_Glob + eax*4]
    mov [wallToCheck], ebx
    
    ; Remove this wall from frontier list (replace with last wall)
    dec dword [FrontierWallCount_Glob]
    mov eax, [FrontierWallCount_Glob]
    mov ebx, [FrontierWalls_Glob + eax*4]
    mov eax, [randomWallIndex]
    mov [FrontierWalls_Glob + eax*4], ebx
    
    ; Get the two cells this wall connects
    mov eax, [wallToCheck]
    shl eax, 3
    mov ebx, [WallToCells_Glob + eax]
    mov [firstCellIndex], ebx
    mov ebx, [WallToCells_Glob + eax + 4]
    mov [secondCellIndex], ebx
    
    ; Check if exactly one cell is in maze
    mov eax, [firstCellIndex]
    movzx eax, byte [CellInMaze_Glob + eax]
    mov [temp], eax
    
    mov eax, [secondCellIndex]
    movzx eax, byte [CellInMaze_Glob + eax]
    mov [temp+4], eax
    
    ; If both same state, skip this wall
    mov eax, [temp]
    cmp eax, [temp+4]
    je .mainLoop
    
    ; Remove the wall
    mov eax, [wallToCheck]
    mov byte [WallsUp_Glob + eax], 0
    
    ; Determine which cell is outside the maze
    mov eax, [temp]
    cmp eax, 0
    je .firstCellOutside
    
    ; Second cell is outside
    mov eax, [secondCellIndex]
    mov [outerCellIndex], eax
    jmp .addOuterCell
    
.firstCellOutside:
    mov eax, [firstCellIndex]
    mov [outerCellIndex], eax
    
.addOuterCell:
    ; Add the outside cell to the maze
    mov eax, [outerCellIndex]
    mov byte [CellInMaze_Glob + eax], 1
    inc dword [cellsInMaze]
    
    ; Add walls of the new cell to frontier
    mov eax, [outerCellIndex]
    mov [currentCellIndex], eax
    call addCellWallsToFrontier
    
    call sleepHalfSecond
    call printMaze
    jmp .mainLoop
    
.done:
    ret

; Add all walls adjacent to a cell to the frontier list
; Only add walls that connect current cell to cells NOT in maze
addCellWallsToFrontier:
    ; Check all four directions using CellToWalls_Glob
    mov eax, [currentCellIndex]
    shl eax, 4          ; multiply by 16 (4 directions * 4 bytes)
    
    ; Check UP wall
    mov ebx, [CellToWalls_Glob + eax + UP*4]
    cmp ebx, NO_CELL
    je .checkDown
    
    ; Check if this wall connects to a cell not in maze
    mov [temp], eax     ; save eax in temp
    call isWallToUnvisitedCell
    cmp eax, 1
    mov eax, [temp]     ; restore eax
    jne .checkDown
    
    call addWallToFrontier
    
.checkDown:
    mov eax, [currentCellIndex]
    shl eax, 4
    mov ebx, [CellToWalls_Glob + eax + DOWN*4]
    cmp ebx, NO_CELL
    je .checkLeft
    
    mov [temp], eax
    call isWallToUnvisitedCell
    cmp eax, 1
    mov eax, [temp]
    jne .checkLeft
    
    call addWallToFrontier
    
.checkLeft:
    mov eax, [currentCellIndex]
    shl eax, 4
    mov ebx, [CellToWalls_Glob + eax + LEFT*4]
    cmp ebx, NO_CELL
    je .checkRight
    
    mov [temp], eax
    call isWallToUnvisitedCell
    cmp eax, 1
    mov eax, [temp]
    jne .checkRight
    
    call addWallToFrontier
    
.checkRight:
    mov eax, [currentCellIndex]
    shl eax, 4
    mov ebx, [CellToWalls_Glob + eax + RIGHT*4]
    cmp ebx, NO_CELL
    je .done
    
    call isWallToUnvisitedCell
    cmp eax, 1
    jne .done
    
    call addWallToFrontier
    
.done:
    ret

; Check if wall connects current cell to a cell not in maze
; Input: ebx = wall index  
; Output: eax = 1 if connects to unvisited cell, 0 otherwise
isWallToUnvisitedCell:
    ; Get both cells connected by this wall
    mov eax, ebx
    shl eax, 3
    mov ecx, [WallToCells_Glob + eax]     ; first cell
    mov edx, [WallToCells_Glob + eax + 4] ; second cell
    
    ; One must be current cell, other must not be in maze
    cmp ecx, [currentCellIndex]
    je .checkSecond
    cmp edx, [currentCellIndex] 
    je .checkFirst
    
    ; Neither is current cell - invalid
    mov eax, 0
    ret
    
.checkSecond:
    ; ecx is current cell, check if edx is NOT in maze
    movzx eax, byte [CellInMaze_Glob + edx]
    cmp eax, 0  ; 0 means not in maze
    je .valid
    mov eax, 0
    ret
    
.checkFirst:
    ; edx is current cell, check if ecx is NOT in maze  
    movzx eax, byte [CellInMaze_Glob + ecx]
    cmp eax, 0  ; 0 means not in maze
    je .valid
    mov eax, 0
    ret
    
.valid:
    mov eax, 1
    ret

; Add a wall to frontier if not already there
; Input: ebx = wall index
addWallToFrontier:
    ; Validate wall index
    cmp ebx, 0
    jl .invalid
    cmp ebx, [InteriorWallCount_Glob]
    jge .invalid
    
    ; Check if wall already in frontier
    mov dword [wallIndex], 0
    
.checkLoop:
    mov eax, [wallIndex]
    cmp eax, [FrontierWallCount_Glob]
    jge .addWall
    
    mov ecx, [FrontierWalls_Glob + eax*4]
    cmp ecx, ebx
    je .alreadyExists
    
    inc dword [wallIndex]
    jmp .checkLoop
    
.addWall:
    ; Check if frontier is full
    cmp dword [FrontierWallCount_Glob], MAX_WALLS
    jge .alreadyExists
    
    ; Add wall to frontier
    mov eax, [FrontierWallCount_Glob]
    mov [FrontierWalls_Glob + eax*4], ebx
    inc dword [FrontierWallCount_Glob]
    
.alreadyExists:
.invalid:
    ret

; Binary Tree algorithm - protect loop variables from function call corruption
buildMazeBinaryTree:
    mov dword [rowIndex], 0
    
.rowLoop:
    mov eax, [rowIndex]
    cmp eax, [MazeRows_Glob]
    jge .done
    
    mov dword [columnIndex], 0
    
.colLoop:
    mov eax, [columnIndex] 
    cmp eax, [MazeColumns_Glob]
    jge .nextRow
    
    ; Calculate current cell index = rowIndex * MazeColumns_Glob + columnIndex
    mov eax, [rowIndex]
    imul eax, [MazeColumns_Glob]
    add eax, [columnIndex]
    mov [currentCellIndex], eax
    
    ; Reset valid wall count
    mov dword [validWallCount], 0
    
    ; Check UP wall (only if not in top row)
    mov eax, [rowIndex]
    cmp eax, 0
    je .checkRight
    
    mov eax, [currentCellIndex]
    shl eax, 4
    mov ebx, [CellToWalls_Glob + eax + UP*4]
    cmp ebx, NO_CELL
    je .checkRight
    
    ; Store valid wall
    mov eax, [validWallCount]
    mov [temp + eax*4], ebx
    inc dword [validWallCount]
    
.checkRight:
    ; Check RIGHT wall (only if not in rightmost column)
    mov eax, [columnIndex]
    mov ebx, [MazeColumns_Glob]
    dec ebx
    cmp eax, ebx
    jge .pickWall
    
    mov eax, [currentCellIndex]
    shl eax, 4
    mov ebx, [CellToWalls_Glob + eax + RIGHT*4]
    cmp ebx, NO_CELL
    je .pickWall
    
    ; Store valid wall
    mov eax, [validWallCount]
    mov [temp + eax*4], ebx
    inc dword [validWallCount]
    
.pickWall:
    ; If we have at least one valid wall, pick one randomly
    mov eax, [validWallCount]
    cmp eax, 0
    jle .nextColumn
    
    ; Save loop variables before function calls that might corrupt them
    mov eax, [rowIndex]
    mov [temp+24], eax        ; Save rowIndex
    mov eax, [columnIndex] 
    mov [temp+28], eax        ; Save columnIndex
    
    ; Get random selection
    call getRandom
    mov eax, [randomSeed]
    xor edx, edx
    mov ebx, [validWallCount]
    div ebx                   ; edx = remainder
    
    ; Remove the chosen wall
    mov eax, [temp + edx*4]
    mov byte [WallsUp_Glob + eax], 0
    
    ; Call functions that might corrupt our loop variables
    call sleepHalfSecond
    call printMaze
    
    ; Restore loop variables after function calls
    mov eax, [temp+24]
    mov [rowIndex], eax       ; Restore rowIndex
    mov eax, [temp+28]
    mov [columnIndex], eax    ; Restore columnIndex
    
.nextColumn:
    inc dword [columnIndex]
    jmp .colLoop
    
.nextRow:
    inc dword [rowIndex]
    jmp .rowLoop
    
.done:
    ret

; Depth-First Search algorithm  
buildMazeDepthFirst:
    mov dword [stackSize], 0
    
    ; Start with random cell
    call getRandom
    mov eax, [randomSeed]
    xor edx, edx
    div dword [totalCells]
    mov [currentCellIndex], edx
    
    ; Mark as visited
    mov eax, [currentCellIndex]
    mov byte [CellVisited_Glob + eax], 1
    
    ; Push starting cell onto stack
    mov eax, [stackSize]
    mov ebx, [currentCellIndex]
    mov [cellStack + eax*4], ebx
    inc dword [stackSize]
    
.mainLoop:
    cmp dword [stackSize], 0
    jle .done
    
    ; Try to find an unvisited neighbor
    call findUnvisitedNeighbor
    cmp dword [foundNeighbor], 1
    je .moveToNeighbor
    
    ; No unvisited neighbor - backtrack
    dec dword [stackSize]
    cmp dword [stackSize], 0
    jle .done
    
    ; Set current cell to top of stack
    mov eax, [stackSize]
    dec eax
    mov ebx, [cellStack + eax*4]
    mov [currentCellIndex], ebx
    jmp .mainLoop
    
.moveToNeighbor:
    ; Remove wall between current and next cell
    mov eax, [wallIndex]
    mov byte [WallsUp_Glob + eax], 0
    
    ; Mark next cell as visited
    mov eax, [nextCellIndex]
    mov byte [CellVisited_Glob + eax], 1
    
    ; Push next cell onto stack
    mov eax, [stackSize]
    mov ebx, [nextCellIndex]
    mov [cellStack + eax*4], ebx
    inc dword [stackSize]
    
    ; Set current cell to next cell
    mov [currentCellIndex], ebx
    
    call sleepHalfSecond
    call printMaze
    jmp .mainLoop
    
.done:
    ret

; Find an unvisited neighbor for depth-first search
findUnvisitedNeighbor:
    mov dword [foundNeighbor], 0
    
    ; Create randomized direction array [0,1,2,3] then shuffle
    mov dword [temp], LEFT
    mov dword [temp+4], UP  
    mov dword [temp+8], RIGHT
    mov dword [temp+12], DOWN
    
    ; Fisher-Yates shuffle of directions
    mov dword [shuffleIndex], 3
.shuffleLoop:
    cmp dword [shuffleIndex], 0
    jle .checkDirections
    
    call getRandom
    mov eax, [randomSeed]
    xor edx, edx
    mov ebx, [shuffleIndex]
    inc ebx
    div ebx
    mov [randomIndex], edx
    
    ; Swap temp[shuffleIndex] with temp[randomIndex]
    mov eax, [shuffleIndex]
    mov ebx, [temp + eax*4]
    mov eax, [randomIndex]
    mov ecx, [temp + eax*4]
    mov eax, [shuffleIndex]
    mov [temp + eax*4], ecx
    mov eax, [randomIndex]
    mov [temp + eax*4], ebx
    
    dec dword [shuffleIndex]
    jmp .shuffleLoop
    
.checkDirections:
    ; Check directions in random order
    mov dword [directionIndex], 0
    
.directionLoop:
    cmp dword [directionIndex], 4
    jge .notFound
    
    mov eax, [directionIndex]
    mov ebx, [temp + eax*4]
    mov [direction], ebx
    
    ; Get wall index for this direction
    mov eax, [currentCellIndex]
    shl eax, 4
    mov ebx, [direction]
    mov ecx, [CellToWalls_Glob + eax + ebx*4]
    cmp ecx, NO_CELL
    je .nextDirection
    
    mov [wallIndex], ecx
    
    ; Find the cell on the other side of this wall
    shl ecx, 3
    mov eax, [WallToCells_Glob + ecx]
    mov ebx, [WallToCells_Glob + ecx + 4]
    
    ; Determine which cell is the neighbor
    cmp eax, [currentCellIndex]
    je .useSecondCell
    mov [nextCellIndex], eax
    jmp .checkVisited
    
.useSecondCell:
    mov [nextCellIndex], ebx
    
.checkVisited:
    ; Check if neighbor is unvisited
    mov eax, [nextCellIndex]
    movzx eax, byte [CellVisited_Glob + eax]
    cmp eax, 0
    jne .nextDirection
    
    ; Found unvisited neighbor
    mov dword [foundNeighbor], 1
    ret
    
.nextDirection:
    inc dword [directionIndex]
    jmp .directionLoop
    
.notFound:
    ret

; Recursive Division algorithm - simplified working version
buildMazeRecursiveDivision:
    ; Start with all interior walls down
    mov dword [wallIndex], 0
    
.clearWalls:
    mov eax, [wallIndex]
    cmp eax, [InteriorWallCount_Glob]
    jge .startDivision
    
    mov byte [WallsUp_Glob + eax], 0
    inc dword [wallIndex]
    jmp .clearWalls
    
.startDivision:
    call printMaze
    call sleepHalfSecond
    
    ; Do a few simple divisions to demonstrate the algorithm
    ; Divide maze roughly in half horizontally
    mov eax, [MazeRows_Glob]
    shr eax, 1
    mov [divideRow], eax
    
    ; Add horizontal wall across the middle
    mov dword [columnIndex], 0
    
.addMiddleHorizontalWall:
    mov eax, [columnIndex]
    cmp eax, [MazeColumns_Glob]
    jge .addHorizontalGap
    
    ; Calculate cell index
    mov eax, [divideRow]
    mul dword [MazeColumns_Glob]
    add eax, [columnIndex]
    
    ; Bounds check
    cmp eax, [totalCells]
    jge .nextMiddleHorizontal
    
    ; Get DOWN wall
    shl eax, 4
    mov ebx, [CellToWalls_Glob + eax + DOWN*4]
    cmp ebx, NO_CELL
    je .nextMiddleHorizontal
    cmp ebx, [InteriorWallCount_Glob]
    jge .nextMiddleHorizontal
    
    mov byte [WallsUp_Glob + ebx], 1
    
.nextMiddleHorizontal:
    inc dword [columnIndex]
    jmp .addMiddleHorizontalWall
    
.addHorizontalGap:
    ; Create one gap in the middle of the horizontal wall
    mov eax, [MazeColumns_Glob]
    shr eax, 1
    mov [gapCol], eax
    
    ; Remove wall at gap position
    mov eax, [divideRow]
    mul dword [MazeColumns_Glob]
    add eax, [gapCol]
    
    cmp eax, [totalCells]
    jge .divideVertically
    
    shl eax, 4
    mov ebx, [CellToWalls_Glob + eax + DOWN*4]
    cmp ebx, NO_CELL
    je .divideVertically
    cmp ebx, [InteriorWallCount_Glob]
    jge .divideVertically
    
    mov byte [WallsUp_Glob + ebx], 0
    
    call sleepHalfSecond
    call printMaze
    
.divideVertically:
    ; Now divide vertically
    mov eax, [MazeColumns_Glob]
    shr eax, 1
    mov [divideCol], eax
    
    ; Add vertical wall down the middle
    mov dword [rowIndex], 0
    
.addMiddleVerticalWall:
    mov eax, [rowIndex]
    cmp eax, [MazeRows_Glob]
    jge .addVerticalGap
    
    ; Calculate cell index
    mov eax, [rowIndex]
    mul dword [MazeColumns_Glob]
    add eax, [divideCol]
    
    ; Bounds check
    cmp eax, [totalCells]
    jge .nextMiddleVertical
    
    ; Get RIGHT wall
    shl eax, 4
    mov ebx, [CellToWalls_Glob + eax + RIGHT*4]
    cmp ebx, NO_CELL
    je .nextMiddleVertical
    cmp ebx, [InteriorWallCount_Glob]
    jge .nextMiddleVertical
    
    mov byte [WallsUp_Glob + ebx], 1
    
.nextMiddleVertical:
    inc dword [rowIndex]
    jmp .addMiddleVerticalWall
    
.addVerticalGap:
    ; Create one gap in the middle of the vertical wall
    mov eax, [MazeRows_Glob]
    shr eax, 1
    mov [gapRow], eax
    
    ; Remove wall at gap position
    mov eax, [gapRow]
    mul dword [MazeColumns_Glob]
    add eax, [divideCol]
    
    cmp eax, [totalCells]
    jge .done
    
    shl eax, 4
    mov ebx, [CellToWalls_Glob + eax + RIGHT*4]
    cmp ebx, NO_CELL
    je .done
    cmp ebx, [InteriorWallCount_Glob]
    jge .done
    
    mov byte [WallsUp_Glob + ebx], 0
    
    call sleepHalfSecond
    call printMaze
    
.done:
    ret



; Recursively divide an area with walls
divideArea:
    ; Calculate height and width
    mov eax, [endRow]
    sub eax, [startRow]
    inc eax
    mov [height], eax
    
    mov eax, [endCol]
    sub eax, [startCol]
    inc eax
    mov [width], eax
    
    ; Base case - area too small to divide
    cmp dword [height], 2
    jl .done
    cmp dword [width], 2
    jl .done
    
    ; Choose whether to divide horizontally or vertically
    mov dword [divideHorizontally], 0
    mov eax, [height]
    cmp eax, [width]
    jg .setHorizontal
    jl .setVertical
    
    ; Equal - choose randomly
    call getRandom
    mov eax, [randomSeed]
    and eax, 1
    mov [divideHorizontally], eax
    jmp .checkDirection
    
.setHorizontal:
    mov dword [divideHorizontally], 1
    jmp .checkDirection
    
.setVertical:
    mov dword [divideHorizontally], 0
    
.checkDirection:
    cmp dword [divideHorizontally], 1
    je .divideHorizontally
    jmp .divideVertically
    
.divideHorizontally:
    cmp dword [height], 1
    jle .done
    
    ; Choose random row to divide on (not the edges)
    mov eax, [endRow]
    sub eax, [startRow]
    cmp eax, 0
    jle .done
    
    call getRandom
    mov eax, [randomSeed]
    xor edx, edx
    mov ebx, [endRow]
    sub ebx, [startRow]
    div ebx
    add eax, [startRow]
    mov [divideRow], eax
    
    ; Add horizontal walls across the division
    mov eax, [startCol]
    mov [columnIndex], eax
    
.addHorizontalWalls:
    mov eax, [columnIndex]
    cmp eax, [endCol]
    jg .chooseHorizontalGap
    
    ; Calculate cell index and add DOWN wall
    mov eax, [divideRow]
    mul dword [MazeColumns_Glob]
    add eax, [columnIndex]
    
    ; Check bounds
    cmp eax, [totalCells]
    jge .nextHorizontalCell
    
    shl eax, 4
    mov ebx, [CellToWalls_Glob + eax + DOWN*4]
    cmp ebx, NO_CELL
    je .nextHorizontalCell
    cmp ebx, [InteriorWallCount_Glob]
    jge .nextHorizontalCell
    
    mov byte [WallsUp_Glob + ebx], 1
    
.nextHorizontalCell:
    inc dword [columnIndex]
    jmp .addHorizontalWalls
    
.chooseHorizontalGap:
    ; Choose random gap in the wall
    mov eax, [endCol]
    sub eax, [startCol]
    inc eax
    cmp eax, 1
    jle .recursiveHorizontal
    
    call getRandom
    mov eax, [randomSeed]
    xor edx, edx
    mov ebx, [endCol]
    sub ebx, [startCol]
    inc ebx
    div ebx
    add eax, [startCol]
    mov [gapCol], eax
    
    ; Remove wall at gap
    mov eax, [divideRow]
    mul dword [MazeColumns_Glob]
    add eax, [gapCol]
    
    cmp eax, [totalCells]
    jge .recursiveHorizontal
    
    shl eax, 4
    mov ebx, [CellToWalls_Glob + eax + DOWN*4]
    cmp ebx, NO_CELL
    je .recursiveHorizontal
    cmp ebx, [InteriorWallCount_Glob]
    jge .recursiveHorizontal
    
    mov byte [WallsUp_Glob + ebx], 0
    
.recursiveHorizontal:
    call sleepHalfSecond
    call printMaze
    
    ; Save current values for recursive calls
    mov eax, [endRow]
    mov [temp], eax
    mov eax, [startCol] 
    mov [temp+4], eax
    mov eax, [endCol]
    mov [temp+8], eax
    
    ; Recursively divide upper area
    mov eax, [divideRow]
    mov [endRow], eax
    call divideArea
    
    ; Restore values and divide lower area
    mov eax, [temp+8]
    mov [endCol], eax
    mov eax, [temp+4]
    mov [startCol], eax
    mov eax, [temp]
    mov [endRow], eax
    
    mov eax, [divideRow]
    inc eax
    mov [startRow], eax
    call divideArea
    
    jmp .done
    
.divideVertically:
    cmp dword [width], 1
    jle .done
    
    ; Choose random column to divide on (not the edges)
    mov eax, [endCol]
    sub eax, [startCol]
    cmp eax, 0
    jle .done
    
    call getRandom
    mov eax, [randomSeed]
    xor edx, edx
    mov ebx, [endCol]
    sub ebx, [startCol]
    div ebx
    add eax, [startCol]
    mov [divideCol], eax
    
    ; Add vertical walls across the division
    mov eax, [startRow]
    mov [rowIndex], eax
    
.addVerticalWalls:
    mov eax, [rowIndex]
    cmp eax, [endRow]
    jg .chooseVerticalGap
    
    ; Calculate cell index and add RIGHT wall
    mov eax, [rowIndex]
    mul dword [MazeColumns_Glob]
    add eax, [divideCol]
    
    ; Check bounds
    cmp eax, [totalCells]
    jge .nextVerticalCell
    
    shl eax, 4
    mov ebx, [CellToWalls_Glob + eax + RIGHT*4]
    cmp ebx, NO_CELL
    je .nextVerticalCell
    cmp ebx, [InteriorWallCount_Glob]
    jge .nextVerticalCell
    
    mov byte [WallsUp_Glob + ebx], 1
    
.nextVerticalCell:
    inc dword [rowIndex]
    jmp .addVerticalWalls
    
.chooseVerticalGap:
    ; Choose random gap in the wall
    mov eax, [endRow]
    sub eax, [startRow]
    inc eax
    cmp eax, 1
    jle .recursiveVertical
    
    call getRandom
    mov eax, [randomSeed]
    xor edx, edx
    mov ebx, [endRow]
    sub ebx, [startRow]
    inc ebx
    div ebx
    add eax, [startRow]
    mov [gapRow], eax
    
    ; Remove wall at gap
    mov eax, [gapRow]
    mul dword [MazeColumns_Glob]
    add eax, [divideCol]
    
    cmp eax, [totalCells]
    jge .recursiveVertical
    
    shl eax, 4
    mov ebx, [CellToWalls_Glob + eax + RIGHT*4]
    cmp ebx, NO_CELL
    je .recursiveVertical
    cmp ebx, [InteriorWallCount_Glob]
    jge .recursiveVertical
    
    mov byte [WallsUp_Glob + ebx], 0
    
.recursiveVertical:
    call sleepHalfSecond
    call printMaze
    
    ; Save current values for recursive calls
    mov eax, [endCol]
    mov [temp+12], eax
    mov eax, [startRow]
    mov [temp+16], eax  
    mov eax, [endRow]
    mov [temp+20], eax
    
    ; Recursively divide left area
    mov eax, [divideCol]
    mov [endCol], eax
    call divideArea
    
    ; Restore values and divide right area
    mov eax, [temp+20]
    mov [endRow], eax
    mov eax, [temp+16]
    mov [startRow], eax
    mov eax, [temp+12]
    mov [endCol], eax
    
    mov eax, [divideCol]
    inc eax
    mov [startCol], eax
    call divideArea
    
.done:
    ret

; Read integer from stdin
readInteger:
    mov rax, 0
    mov rdi, 0
    mov rsi, inputBuffer
    mov rdx, 20
    syscall
    
    ; Convert ASCII to integer
    xor eax, eax
    xor ebx, ebx
    mov rsi, inputBuffer
    
.convertLoop:
    mov bl, [rsi]
    cmp bl, 10          ; newline
    je .convertDone
    cmp bl, 0           ; null terminator
    je .convertDone
    cmp bl, '0'
    jb .convertDone
    cmp bl, '9'
    ja .convertDone
    
    sub bl, '0'
    imul eax, 10
    add eax, ebx
    inc rsi
    jmp .convertLoop
    
.convertDone:
    ret

; Print maze - uses WallsUp_Glob to determine wall states
printMaze:
    ; Reset interior wall index  
    mov dword [interiorWallIndex], 0
    
    ; Print top border (always all walls up)
    call printTopBorder
    
    ; Initialize row counter
    mov dword [rowIndex], 0
    
.rowLoop:
    mov eax, [rowIndex]
    cmp eax, [MazeRows_Glob]
    jge .done
    
    ; Print vertical walls for this row
    call printVerticalWalls
    
    ; Check if this is the last row
    mov eax, [rowIndex]
    mov ebx, [MazeRows_Glob]
    dec ebx
    cmp eax, ebx
    je .skipHorizontalWalls
    
    ; Print horizontal walls below this row (not last row)
    call printHorizontalWalls
    
.skipHorizontalWalls:
    ; Increment row counter
    inc dword [rowIndex]
    jmp .rowLoop
    
.done:
    ; Print bottom border (always all walls up)
    call printTopBorder
    ; Print extra newline
    mov rax, 1
    mov rdi, 1
    mov rsi, newline_char
    mov rdx, 1
    syscall
    ret

; Print vertical walls: | | | | (checking WallsUp_Glob array)
printVerticalWalls:
    ; Print first "|" (always up - exterior wall)
    mov rax, 1
    mov rdi, 1
    mov rsi, pipe_char
    mov rdx, 1
    syscall
    
    ; Initialize vertical wall index for interior walls (columns-1)
    mov dword [verticalWallIndex], 0
    
.loop:
    mov eax, [verticalWallIndex]
    mov ebx, [MazeColumns_Glob]
    dec ebx             ; columns - 1 interior vertical positions
    cmp eax, ebx
    jge .printLast
    
    ; Print space for cell content
    mov rax, 1
    mov rdi, 1
    mov rsi, space_char
    mov rdx, 1
    syscall
    
    ; Check if this wall is up in WallsUp_Glob array
    mov eax, [interiorWallIndex]
    cmp eax, [InteriorWallCount_Glob]
    jge .printSpace
    
    movzx ebx, byte [WallsUp_Glob + eax]
    cmp ebx, 1
    je .printPipe
    
.printSpace:
    ; Print space (wall is down)
    mov rax, 1
    mov rdi, 1
    mov rsi, space_char
    mov rdx, 1
    syscall
    jmp .nextWall
    
.printPipe:
    ; Print pipe (wall is up)
    mov rax, 1
    mov rdi, 1
    mov rsi, pipe_char
    mov rdx, 1
    syscall
    
.nextWall:
    inc dword [interiorWallIndex]
    inc dword [verticalWallIndex]
    jmp .loop
    
.printLast:
    ; Print final space and closing "|" (always up - exterior wall)
    mov rax, 1
    mov rdi, 1
    mov rsi, space_char
    mov rdx, 1
    syscall
    
    mov rax, 1
    mov rdi, 1
    mov rsi, pipe_char
    mov rdx, 1
    syscall
    
    ; Print newline
    mov rax, 1
    mov rdi, 1
    mov rsi, newline_char
    mov rdx, 1
    syscall
    ret

; Print horizontal walls between rows (checking WallsUp_Glob array)
printHorizontalWalls:
    ; Print "+"
    mov rax, 1
    mov rdi, 1
    mov rsi, plus_char
    mov rdx, 1
    syscall
    
    ; Initialize horizontal wall index
    mov dword [horizontalWallIndex], 0
    
.loop:
    mov eax, [horizontalWallIndex]
    cmp eax, [MazeColumns_Glob]
    jge .done
    
    ; Check if this wall is up in WallsUp_Glob array
    mov eax, [interiorWallIndex]
    cmp eax, [InteriorWallCount_Glob]
    jge .printSpace
    
    movzx ebx, byte [WallsUp_Glob + eax]
    cmp ebx, 1
    je .printDash
    
.printSpace:
    ; Print space (wall is down)
    mov rax, 1
    mov rdi, 1
    mov rsi, space_char
    mov rdx, 1
    syscall
    jmp .printPlus
    
.printDash:
    ; Print dash (wall is up)
    mov rax, 1
    mov rdi, 1
    mov rsi, dash_char
    mov rdx, 1
    syscall
    
.printPlus:
    ; Print "+"
    mov rax, 1
    mov rdi, 1
    mov rsi, plus_char
    mov rdx, 1
    syscall
    
    inc dword [interiorWallIndex]
    inc dword [horizontalWallIndex]
    jmp .loop
    
.done:
    ; Print newline
    mov rax, 1
    mov rdi, 1
    mov rsi, newline_char
    mov rdx, 1
    syscall
    ret

; Print top/bottom border (always all walls up)
printTopBorder:
    ; Print "+"
    mov rax, 1
    mov rdi, 1
    mov rsi, plus_char
    mov rdx, 1
    syscall
    
    ; Initialize horizontal wall index
    mov dword [horizontalWallIndex], 0
    
.loop:
    mov eax, [horizontalWallIndex]
    cmp eax, [MazeColumns_Glob]
    jge .done
    
    ; Always print "-" for borders
    mov rax, 1
    mov rdi, 1
    mov rsi, dash_char
    mov rdx, 1
    syscall
    
    ; Print "+"
    mov rax, 1
    mov rdi, 1
    mov rsi, plus_char
    mov rdx, 1
    syscall
    
    inc dword [horizontalWallIndex]
    jmp .loop
    
.done:
    ; Print newline
    mov rax, 1
    mov rdi, 1
    mov rsi, newline_char
    mov rdx, 1
    syscall
    ret
