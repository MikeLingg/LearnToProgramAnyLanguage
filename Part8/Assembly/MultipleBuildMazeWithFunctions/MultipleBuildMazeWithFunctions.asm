; Maze Generation Program - Converted from C++ to NASM
; Original C++ file: MultipleBuildMazeWithFunctions.cpp
; Implements multiple maze generation algorithms: Kruskal, Prim, Depth-First, Binary Tree, Recursive Division

section .rodata
    ; Define directions - const int LEFT = 0, UP = 1, RIGHT = 2, DOWN = 3
    LEFT equ 0
    UP equ 1  
    RIGHT equ 2
    DOWN equ 3
    
    ; Define cell constants - const int FIRST_CELL = 0, SECOND_CELL = 1
    FIRST_CELL equ 0
    SECOND_CELL equ 1
    
    ; Define invalid cell - const int NO_CELL = -1
    NO_CELL equ -1
    
    ; Define algorithm types
    KRUSKAL_ALGORITHM equ 1
    PRIM_ALGORITHM equ 2
    DEPTH_FIRST_ALGORITHM equ 3
    BINARY_TREE_ALGORITHM equ 4
    RECURSIVE_DIVISION_ALGORITHM equ 5
    
    ; String constants for user prompts
    prompt_columns db "Please enter number of columns for maze, must be greater than 1: ", 0
    prompt_rows db "Please enter number of rows for maze, must be greater than 1: ", 0
    prompt_algorithm db "Please choose maze generation algorithm:", 10, 0
    alg_kruskal db "1 - Kruskal's Algorithm", 10, 0
    alg_prim db "2 - Prim's Algorithm", 10, 0
    alg_depth db "3 - Depth-First Search", 10, 0
    alg_binary db "4 - Binary Tree Algorithm", 10, 0
    alg_recursive db "5 - Recursive Division Algorithm", 10, 0

section .bss
    ; Global variables from C++
    AllHorizontalWallsUp_Glob resq 1    ; bool* AllHorizontalWallsUp_Glob
    WallsUp_Glob resq 1                 ; bool* WallsUp_Glob
    InteriorWallCount_Glob resd 1       ; int InteriorWallCount_Glob
    MazeRows_Glob resd 1                ; int MazeRows_Glob
    MazeColumns_Glob resd 1             ; int MazeColumns_Glob
    
    ; Additional globals for algorithm state sharing
    CellVisited_Glob resq 1             ; bool* CellVisited_Glob
    FrontierWalls_Glob resq 1           ; int* FrontierWalls_Glob
    FrontierWallCount_Glob resd 1       ; int FrontierWallCount_Glob
    CellInMaze_Glob resq 1              ; bool* CellInMaze_Glob
    
    ; Lookup tables for wall/cell relationships
    WallToCells_Glob resq 1             ; int** WallToCells_Glob
    CellToWalls_Glob resq 1             ; int** CellToWalls_Glob
    
    ; Local variables for input processing
    user_input resb 100

section .text
    global _start
    extern printf, putchar, puts, fgets, stdin, strlen, atoi, time, srand, rand
    extern clock, malloc, free

; Function: printHorizontalWalls(const bool* HorizontalWallsUp_Par)
; Prints a horizontal wall, similar to +-+-+-+, with walls down based on the provided parameters
printHorizontalWalls:
    push rbp
    mov rbp, rsp
    sub rsp, 32
    mov [rbp-24], rdi           ; HorizontalWallsUp_Par parameter
    
    mov edi, 43                 ; printf("+")
    call putchar
    
    mov dword [rbp-4], 0        ; horizontalWallIndex = 0
    jmp .L2
    
.L5:
    mov eax, [rbp-4]            ; horizontalWallIndex
    movsxd rdx, eax
    mov rax, [rbp-24]           ; HorizontalWallsUp_Par
    add rax, rdx
    movzx eax, byte [rax]
    test eax, eax
    je .L3
    
    mov edi, 45                 ; printf("-") - wall is up
    call putchar
    jmp .L4
    
.L3:
    mov edi, 32                 ; printf(" ") - wall is down
    call putchar
    
.L4:
    mov edi, 43                 ; printf("+")
    call putchar
    inc dword [rbp-4]           ; horizontalWallIndex++
    
.L2:
    mov eax, [MazeColumns_Glob]
    cmp [rbp-4], eax
    jl .L5
    
    mov edi, 10                 ; printf("\n")
    call putchar
    
    leave
    ret

; Function: printHorizontalWalls() - overloaded version
; Prints horizontal walls with all walls up
printHorizontalWalls_all_up:
    push rbp
    mov rbp, rsp
    
    mov rax, [AllHorizontalWallsUp_Glob]
    mov rdi, rax
    call printHorizontalWalls
    
    pop rbp
    ret

; Function: printVerticalWalls(const bool* VerticalWallsUp_Par)
; Prints vertical walls like | | | |, with walls down based on provided parameters
printVerticalWalls:
    push rbp
    mov rbp, rsp
    sub rsp, 32
    mov [rbp-24], rdi           ; VerticalWallsUp_Par parameter
    
    mov edi, 124                ; printf("|") - first wall is exterior, always up
    call putchar
    
    mov dword [rbp-4], 0        ; verticalWallIndex = 0
    jmp .L8
    
.L11:
    mov edi, 32                 ; printf(" ")
    call putchar
    
    mov eax, [rbp-4]            ; verticalWallIndex
    movsxd rdx, eax
    mov rax, [rbp-24]           ; VerticalWallsUp_Par
    add rax, rdx
    movzx eax, byte [rax]
    test eax, eax
    je .L9
    
    mov edi, 124                ; printf("|") - wall is up
    call putchar
    jmp .L10
    
.L9:
    mov edi, 32                 ; printf(" ") - wall is down
    call putchar
    
.L10:
    inc dword [rbp-4]           ; verticalWallIndex++
    
.L8:
    mov eax, [MazeColumns_Glob]
    dec eax
    cmp [rbp-4], eax
    jl .L11
    
    mov edi, 32                 ; printf(" ")
    call putchar
    mov edi, 124                ; printf("|") - last wall exterior, always up
    call putchar
    mov edi, 10                 ; printf("\n")
    call putchar
    
    leave
    ret

; Function: printMaze()
; Loop through the rows of the maze and print the maze based on WallsUp_Glob
printMaze:
    push rbp
    mov rbp, rsp
    push r12
    push rbx
    sub rsp, 64
    
    mov dword [rbp-72], 0       ; interiorWallIndex = 0
    
    ; First row is exterior walls
    call printHorizontalWalls_all_up
    
    mov dword [rbp-68], 0       ; rowIndex = 0
    jmp .L13
    
.L26:
    ; Create verticalWallsUp array on stack
    mov eax, [MazeColumns_Glob]
    dec eax
    ; Dynamic stack allocation for verticalWallsUp[MazeColumns_Glob - 1]
    movsxd rax, eax
    mov rsi, rax
    shl rsi, 0                  ; sizeof(bool) = 1
    sub rsp, rsi
    mov [rbp-48], rsp           ; verticalWallsUp pointer
    
    mov dword [rbp-64], 0       ; columnIndex = 0
    jmp .L17
    
.L18:
    mov rax, [WallsUp_Glob]
    mov edx, [rbp-72]           ; interiorWallIndex
    movsxd rdx, edx
    add rax, rdx
    movzx ecx, byte [rax]
    mov rax, [rbp-48]           ; verticalWallsUp
    mov edx, [rbp-64]           ; columnIndex
    movsxd rdx, edx
    mov [rax+rdx], cl
    inc dword [rbp-72]          ; interiorWallIndex++
    inc dword [rbp-64]          ; columnIndex++
    
.L17:
    mov eax, [MazeColumns_Glob]
    dec eax
    cmp [rbp-64], eax
    jl .L18
    
    mov rax, [rbp-48]           ; verticalWallsUp
    mov rdi, rax
    call printVerticalWalls
    
    ; Check if this is the last row
    mov eax, [MazeRows_Glob]
    dec eax
    cmp [rbp-68], eax           ; rowIndex
    jne .L19
    
    call printHorizontalWalls_all_up
    jmp .L20
    
.L19:
    ; Create horizontalWallsUp array for this row
    mov eax, [MazeColumns_Glob]
    movsxd rax, eax
    sub rsp, rax                ; horizontalWallsUp[MazeColumns_Glob]
    mov [rbp-32], rsp
    
    mov dword [rbp-60], 0       ; columnIndex = 0
    jmp .L24
    
.L25:
    mov rax, [WallsUp_Glob]
    mov edx, [rbp-72]           ; interiorWallIndex
    movsxd rdx, edx
    add rax, rdx
    movzx ecx, byte [rax]
    mov rax, [rbp-32]           ; horizontalWallsUp
    mov edx, [rbp-60]           ; columnIndex
    movsxd rdx, edx
    mov [rax+rdx], cl
    inc dword [rbp-72]          ; interiorWallIndex++
    inc dword [rbp-60]          ; columnIndex++
    
.L24:
    mov eax, [MazeColumns_Glob]
    cmp [rbp-60], eax
    jl .L25
    
    mov rax, [rbp-32]           ; horizontalWallsUp
    mov rdi, rax
    call printHorizontalWalls
    
.L20:
    inc dword [rbp-68]          ; rowIndex++
    
.L13:
    mov eax, [MazeRows_Glob]
    cmp [rbp-68], eax
    jl .L26
    
    mov edi, 10                 ; printf("\n")
    call putchar
    
    add rsp, 64
    pop rbx
    pop r12
    pop rbp
    ret

; Function: sleepHalfSecond()
; Simple sleep function using busy wait
sleepHalfSecond:
    push rbp
    mov rbp, rsp
    sub rsp, 16
    
    call clock                  ; start = clock()
    mov [rbp-8], rax
    
.L29:
    call clock
    sub rax, [rbp-8]            ; clock() - start
    cmp rax, 499999             ; CLOCKS_PER_SEC / 2
    jle .L29
    
    leave
    ret

; Function: initializeLookupTables(int AlgorithmType_Par)
; Initialize lookup tables for wall/cell relationships
; Must be called after maze dimensions are set
initializeLookupTables:
    push rbp
    mov rbp, rsp
    mov [rbp-84], edi           ; AlgorithmType_Par
    
    ; Build WallToCells_Glob for algorithms that need wall-to-cell lookups
    cmp dword [rbp-84], KRUSKAL_ALGORITHM
    je .L31
    cmp dword [rbp-84], PRIM_ALGORITHM
    je .L31
    cmp dword [rbp-84], DEPTH_FIRST_ALGORITHM
    je .L31
    cmp dword [rbp-84], BINARY_TREE_ALGORITHM
    je .L31
    cmp dword [rbp-84], RECURSIVE_DIVISION_ALGORITHM
    jne .L32
    
.L31:
    mov dword [rbp-76], 0       ; wallIndex = 0
    mov dword [rbp-72], 0       ; rowIndex = 0
    jmp .L33
    
.L39:
    ; Track the first cell in the current row
    mov eax, [MazeColumns_Glob]
    imul eax, [rbp-72]          ; rowIndex
    mov [rbp-52], eax           ; firstCellInRow
    
    ; Process vertical walls (0..MazeColumns_Glob-2)
    mov dword [rbp-68], 0       ; verticalWallIndex = 0
    jmp .L34
    
.L35:
    mov eax, [rbp-52]           ; firstCellInRow
    add eax, [rbp-68]           ; + verticalWallIndex
    mov [rbp-40], eax           ; leftCell
    mov eax, [rbp-40]
    inc eax
    mov [rbp-36], eax           ; rightCell = leftCell + 1
    
    ; WallToCells_Glob[wallIndex][FIRST_CELL] = leftCell
    mov rax, [WallToCells_Glob]
    mov edx, [rbp-76]           ; wallIndex
    movsxd rdx, edx
    shl rdx, 3
    add rax, rdx
    mov rax, [rax]
    mov edx, [rbp-40]           ; leftCell
    mov [rax], edx
    
    ; WallToCells_Glob[wallIndex][SECOND_CELL] = rightCell
    mov rax, [WallToCells_Glob]
    mov edx, [rbp-76]           ; wallIndex
    movsxd rdx, edx
    shl rdx, 3
    add rax, rdx
    mov rax, [rax]
    add rax, 8                  ; [RIGHT]
    mov eax, [rax]
    cmp eax, NO_CELL
    je .L65
    
    mov edi, eax
    call addWallToFrontier
    
.L65:
    leave
    ret

; Function: divideArea(int StartRow_Par, int EndRow_Par, int StartCol_Par, int EndCol_Par)
; Recursively divide an area with walls
divideArea:
    push rbp
    mov rbp, rsp
    sub rsp, 96
    mov [rbp-84], edi           ; StartRow_Par
    mov [rbp-88], esi           ; EndRow_Par
    mov [rbp-92], edx           ; StartCol_Par
    mov [rbp-96], ecx           ; EndCol_Par
    
    ; Calculate height and width
    mov eax, [rbp-88]           ; EndRow_Par
    sub eax, [rbp-84]           ; - StartRow_Par
    inc eax
    mov [rbp-56], eax           ; height
    
    mov eax, [rbp-96]           ; EndCol_Par
    sub eax, [rbp-92]           ; - StartCol_Par
    inc eax
    mov [rbp-52], eax           ; width
    
    ; Base case - area too small to divide (must be at least 3x3 to be worth visualizing)
    cmp dword [rbp-56], 3       ; height >= 3
    jl .L81
    cmp dword [rbp-52], 3       ; width >= 3
    jl .L81
    
.L67:
    ; Choose whether to divide horizontally or vertically
    mov byte [rbp-65], 0        ; divideHorizontally = false
    mov eax, [rbp-56]           ; height
    cmp eax, [rbp-52]           ; width
    jle .L69
    
    mov byte [rbp-65], 1        ; divideHorizontally = true
    jmp .L70
    
.L69:
    mov eax, [rbp-52]           ; width
    cmp eax, [rbp-56]           ; height
    jle .L71
    
    mov byte [rbp-65], 0        ; divideHorizontally = false
    jmp .L70
    
.L71:
    ; Square area - choose randomly
    call rand
    and eax, 1
    test eax, eax
    sete al
    mov [rbp-65], al
    
.L70:
    ; Check if dividing horizontally and height > 1
    cmp byte [rbp-65], 1
    jne .L72
    cmp dword [rbp-56], 1       ; height > 1
    jle .L72
    
    ; Choose random row to divide on
    call rand
    mov edi, [rbp-88]           ; EndRow_Par
    sub edi, [rbp-84]           ; - StartRow_Par
    cdq
    idiv edi
    add edx, [rbp-84]           ; + StartRow_Par
    mov [rbp-48], edx           ; divideRow
    
    ; Add horizontal wall
    mov eax, [rbp-92]           ; StartCol_Par
    mov [rbp-64], eax           ; wallCol
    jmp .L73
    
.L75:
    ; Calculate cellIndex
    mov eax, [MazeColumns_Glob]
    imul eax, [rbp-48]          ; divideRow
    add eax, [rbp-64]           ; + wallCol
    mov [rbp-32], eax           ; cellIndex
    
    ; Check bounds
    mov eax, [MazeRows_Glob]
    imul eax, [MazeColumns_Glob]
    cmp [rbp-32], eax
    jge .L74
    
    ; Get wall index for DOWN direction
    mov rax, [CellToWalls_Glob]
    mov edx, [rbp-32]           ; cellIndex
    movsxd rdx, edx
    shl rdx, 3
    add rax, rdx
    mov rax, [rax]
    mov eax, [rax+12]           ; [DOWN]
    mov [rbp-28], eax           ; wallIndex
    
    cmp dword [rbp-28], NO_CELL
    je .L74
    
    ; Set wall up
    mov rax, [WallsUp_Glob]
    mov edx, [rbp-28]           ; wallIndex
    movsxd rdx, edx
    add rax, rdx
    mov byte [rax], 1
    
.L74:
    inc dword [rbp-64]          ; wallCol++
    
.L73:
    mov eax, [rbp-64]           ; wallCol
    cmp eax, [rbp-96]           ; EndCol_Par
    jle .L75
    
    ; Choose random gap in the wall
    call rand
    mov edi, [rbp-96]           ; EndCol_Par
    sub edi, [rbp-92]           ; - StartCol_Par
    inc edi
    cdq
    idiv edi
    add edx, [rbp-92]           ; + StartCol_Par
    mov [rbp-44], edx           ; gapCol
    
    ; Calculate gap cell index
    mov eax, [MazeColumns_Glob]
    imul eax, [rbp-48]          ; divideRow
    add eax, [rbp-44]           ; + gapCol
    mov [rbp-40], eax           ; gapCellIndex
    
    ; Check bounds and remove gap wall
    mov eax, [MazeRows_Glob]
    imul eax, [MazeColumns_Glob]
    cmp [rbp-40], eax
    jge .L76
    
    mov rax, [CellToWalls_Glob]
    mov edx, [rbp-40]           ; gapCellIndex
    movsxd rdx, edx
    shl rdx, 3
    add rax, rdx
    mov rax, [rax]
    mov eax, [rax+12]           ; [DOWN]
    mov [rbp-36], eax           ; gapWallIndex
    
    cmp dword [rbp-36], NO_CELL
    je .L76
    
    ; Remove gap wall
    mov rax, [WallsUp_Glob]
    mov edx, [rbp-36]           ; gapWallIndex
    movsxd rdx, edx
    add rax, rdx
    mov byte [rax], 0
    
.L76:
    call sleepHalfSecond
    call printMaze
    
    ; Recursively divide the two areas
    mov ecx, [rbp-96]           ; EndCol_Par
    mov edx, [rbp-92]           ; StartCol_Par
    mov esi, [rbp-48]           ; divideRow
    mov edi, [rbp-84]           ; StartRow_Par
    call divideArea
    
    mov eax, [rbp-48]           ; divideRow
    inc eax
    mov edi, eax
    mov esi, [rbp-88]           ; EndRow_Par
    mov edx, [rbp-92]           ; StartCol_Par
    mov ecx, [rbp-96]           ; EndCol_Par
    call divideArea
    jmp .L81
    
.L72:
    ; Check if dividing vertically and width > 1
    cmp byte [rbp-65], 0
    jne .L81
    cmp dword [rbp-52], 1       ; width > 1
    jle .L81
    
    ; Choose random column to divide on
    call rand
    mov edi, [rbp-96]           ; EndCol_Par
    sub edi, [rbp-92]           ; - StartCol_Par
    cdq
    idiv edi
    add edx, [rbp-92]           ; + StartCol_Par
    mov [rbp-24], edx           ; divideCol
    
    ; Add vertical wall
    mov eax, [rbp-84]           ; StartRow_Par
    mov [rbp-60], eax           ; cellRow
    jmp .L77
    
.L79:
    ; Calculate cellIndex
    mov eax, [MazeColumns_Glob]
    imul eax, [rbp-60]          ; cellRow
    add eax, [rbp-24]           ; + divideCol
    mov [rbp-8], eax            ; cellIndex
    
    ; Check bounds
    mov eax, [MazeRows_Glob]
    imul eax, [MazeColumns_Glob]
    cmp [rbp-8], eax
    jge .L78
    
    ; Get wall index for RIGHT direction
    mov rax, [CellToWalls_Glob]
    mov edx, [rbp-8]            ; cellIndex
    movsxd rdx, edx
    shl rdx, 3
    add rax, rdx
    mov rax, [rax]
    mov eax, [rax+8]            ; [RIGHT]
    mov [rbp-4], eax            ; wallIndex
    
    cmp dword [rbp-4], NO_CELL
    je .L78
    
    ; Set wall up
    mov rax, [WallsUp_Glob]
    mov edx, [rbp-4]            ; wallIndex
    movsxd rdx, edx
    add rax, rdx
    mov byte [rax], 1
    
.L78:
    inc dword [rbp-60]          ; cellRow++
    
.L77:
    mov eax, [rbp-60]           ; cellRow
    cmp eax, [rbp-88]           ; EndRow_Par
    jle .L79
    
    ; Choose random gap in the wall
    call rand
    mov edi, [rbp-88]           ; EndRow_Par
    sub edi, [rbp-84]           ; - StartRow_Par
    inc edi
    cdq
    idiv edi
    add edx, [rbp-84]           ; + StartRow_Par
    mov [rbp-20], edx           ; gapRow
    
    ; Calculate gap cell index
    mov eax, [MazeColumns_Glob]
    imul eax, [rbp-20]          ; gapRow
    add eax, [rbp-24]           ; + divideCol
    mov [rbp-16], eax           ; gapCellIndex
    
    ; Check bounds and remove gap wall
    mov eax, [MazeRows_Glob]
    imul eax, [MazeColumns_Glob]
    cmp [rbp-16], eax
    jge .L80
    
    mov rax, [CellToWalls_Glob]
    mov edx, [rbp-16]           ; gapCellIndex
    movsxd rdx, edx
    shl rdx, 3
    add rax, rdx
    mov rax, [rax]
    mov eax, [rax+8]            ; [RIGHT]
    mov [rbp-12], eax           ; gapWallIndex
    
    cmp dword [rbp-12], NO_CELL
    je .L80
    
    ; Remove gap wall
    mov rax, [WallsUp_Glob]
    mov edx, [rbp-12]           ; gapWallIndex
    movsxd rdx, edx
    add rax, rdx
    mov byte [rax], 0
    
.L80:
    call sleepHalfSecond
    call printMaze
    
    ; Recursively divide the two areas
    mov edi, [rbp-84]           ; StartRow_Par
    mov esi, [rbp-88]           ; EndRow_Par
    mov edx, [rbp-92]           ; StartCol_Par
    mov ecx, [rbp-24]           ; divideCol
    call divideArea
    
    mov edi, [rbp-84]           ; StartRow_Par
    mov esi, [rbp-88]           ; EndRow_Par
    mov eax, [rbp-24]           ; divideCol
    inc eax
    mov edx, eax
    mov ecx, [rbp-96]           ; EndCol_Par
    call divideArea
    
.L81:
    leave
    ret

; Function: buildMazeKruskal()
; Kruskal's algorithm - place each cell in its own group, then process walls in random order
; If cells on either side are in separate groups, remove wall and merge groups
buildMazeKruskal:
    push rbp
    mov rbp, rsp
    push r12
    push rbx
    sub rsp, 144
    
    ; Create cellToGroup array on stack
    mov eax, [MazeRows_Glob]
    imul eax, [MazeColumns_Glob]
    mov r12d, eax               ; Total cells
    mov eax, r12d
    shl eax, 2                  ; * sizeof(int)
    sub rsp, rax
    mov [rbp-72], rsp           ; cellToGroup array
    
    ; Create groupCells 2D array on stack
    mov eax, r12d
    imul eax, r12d
    shl eax, 2                  ; * sizeof(int)
    sub rsp, rax
    mov [rbp-48], rsp           ; groupCells array
    
    ; Initialize cellToGroup and groupCells
    mov dword [rbp-144], 0      ; cellIndex = 0
    jmp .L89
    
.L94:
    ; cellToGroup[cellIndex] = cellIndex
    mov rax, [rbp-72]           ; cellToGroup
    mov edx, [rbp-144]          ; cellIndex
    movsxd rdx, edx
    mov ecx, [rbp-144]          ; cellIndex
    mov [rax+rdx*4], ecx
    
    ; Initialize groupCells[cellIndex]
    mov dword [rbp-140], 0      ; groupCellIndex = 0
    jmp .L90
    
.L93:
    cmp dword [rbp-140], 0      ; groupCellIndex == 0
    jne .L91
    
    ; groupCells[cellIndex][0] = cellIndex
    mov rax, [rbp-48]           ; groupCells
    mov edx, [rbp-144]          ; cellIndex
    movsxd rdx, edx
    mov esi, r12d               ; cells per row
    imul rdx, rsi
    mov ecx, [rbp-140]          ; groupCellIndex
    movsxd rcx, ecx
    add rdx, rcx
    mov eax, [rbp-144]          ; cellIndex
    mov [rax+rdx*4], eax
    jmp .L92
    
.L91:
    ; groupCells[cellIndex][groupCellIndex] = NO_CELL
    mov rax, [rbp-48]           ; groupCells
    mov edx, [rbp-144]          ; cellIndex
    movsxd rdx, edx
    mov esi, r12d               ; cells per row
    imul rdx, rsi
    mov ecx, [rbp-140]          ; groupCellIndex
    movsxd rcx, ecx
    add rdx, rcx
    mov dword [rax+rdx*4], NO_CELL
    
.L92:
    inc dword [rbp-140]         ; groupCellIndex++
    
.L90:
    cmp [rbp-140], r12d         ; groupCellIndex < total cells
    jl .L93
    
    inc dword [rbp-144]         ; cellIndex++
    
.L89:
    cmp [rbp-144], r12d         ; cellIndex < total cells
    jl .L94
    
    mov byte [rbp-145], 0       ; mazeComplete = false
    
    ; Create wallRemoveList array on stack
    mov eax, [InteriorWallCount_Glob]
    shl eax, 2                  ; * sizeof(int)
    sub rsp, rax
    mov [rbp-32], rsp           ; wallRemoveList
    
    ; Initialize wallRemoveList
    mov dword [rbp-136], 0      ; wallIndex = 0
    jmp .L98
    
.L99:
    mov rax, [rbp-32]           ; wallRemoveList
    mov edx, [rbp-136]          ; wallIndex
    movsxd rdx, edx
    mov ecx, [rbp-136]          ; wallIndex
    mov [rax+rdx*4], ecx
    inc dword [rbp-136]         ; wallIndex++
    
.L98:
    mov eax, [InteriorWallCount_Glob]
    cmp [rbp-136], eax
    jl .L99
    
    ; Fisher-Yates shuffle
    mov eax, [InteriorWallCount_Glob]
    dec eax
    mov [rbp-132], eax          ; shuffleIndex
    jmp .L100
    
.L101:
    call rand
    mov edx, [rbp-132]          ; shuffleIndex
    inc edx
    cdq
    idiv edx
    mov [rbp-88], edx           ; randomIndex
    
    ; Swap wallRemoveList[shuffleIndex] with wallRemoveList[randomIndex]
    mov rax, [rbp-32]           ; wallRemoveList
    mov edx, [rbp-132]          ; shuffleIndex
    movsxd rdx, edx
    mov eax, [rax+rdx*4]
    mov [rbp-84], eax           ; temp
    
    mov rax, [rbp-32]           ; wallRemoveList
    mov edx, [rbp-88]           ; randomIndex
    movsxd rdx, edx
    mov ecx, [rax+rdx*4]
    mov rax, [rbp-32]           ; wallRemoveList
    mov edx, [rbp-132]          ; shuffleIndex
    movsxd rdx, edx
    mov [rax+rdx*4], ecx
    
    mov rax, [rbp-32]           ; wallRemoveList
    mov edx, [rbp-88]           ; randomIndex
    movsxd rdx, edx
    mov ecx, [rbp-84]           ; temp
    mov [rax+rdx*4], ecx
    
    dec dword [rbp-132]         ; shuffleIndex--
    
.L100:
    cmp dword [rbp-132], 0
    jg .L101
    
    ; Perform Kruskal's algorithm
    mov dword [rbp-128], 0      ; removeWallIndex = 0
    jmp .L102
    
.L112:
    ; Get next wall to check
    mov rax, [rbp-32]           ; wallRemoveList
    mov edx, [rbp-128]          ; removeWallIndex
    movsxd rdx, edx
    mov eax, [rax+rdx*4]
    mov [rbp-112], eax          ; nextWallToCheck
    
    ; Get the two cells connected to this wall
    mov rax, [WallToCells_Glob]
    mov edx, [rbp-112]          ; nextWallToCheck
    movsxd rdx, edx
    shl rdx, 3
    add rax, rdx
    mov rax, [rax]
    mov eax, [rax]
    mov [rbp-108], eax          ; firstCellIndex
    
    mov rax, [rbp-72]           ; cellToGroup
    mov edx, [rbp-108]          ; firstCellIndex
    movsxd rdx, edx
    mov eax, [rax+rdx*4]
    mov [rbp-104], eax          ; firstCellGroupIndex
    
    mov rax, [WallToCells_Glob]
    mov edx, [rbp-112]          ; nextWallToCheck
    movsxd rdx, edx
    shl rdx, 3
    add rax, rdx
    mov rax, [rax]
    mov eax, [rax+4]
    mov [rbp-100], eax          ; secondCellIndex
    
    mov rax, [rbp-72]           ; cellToGroup
    mov edx, [rbp-100]          ; secondCellIndex
    movsxd rdx, edx
    mov eax, [rax+rdx*4]
    mov [rbp-96], eax           ; secondCellGroupIndex
    
    ; If cells are in different groups, remove wall and merge groups
    mov eax, [rbp-104]          ; firstCellGroupIndex
    cmp eax, [rbp-96]           ; secondCellGroupIndex
    je .L103
    
    ; Remove the wall
    mov rax, [WallsUp_Glob]
    mov edx, [rbp-112]          ; nextWallToCheck
    movsxd rdx, edx
    add rax, rdx
    mov byte [rax], 0
    
    ; Find next empty index in first group
    mov dword [rbp-124], 0      ; nextEmptyFirstGroupIndex = 0
    mov dword [rbp-120], 0      ; cellIndex = 0
    jmp .L104
    
.L107:
    mov rax, [rbp-48]           ; groupCells
    mov edx, [rbp-104]          ; firstCellGroupIndex
    movsxd rdx, edx
    mov esi, r12d               ; cells per row
    imul rdx, rsi
    mov ecx, [rbp-120]          ; cellIndex
    movsxd rcx, ecx
    add rdx, rcx
    mov eax, [rax+rdx*4]
    cmp eax, NO_CELL
    jne .L105
    
    mov eax, [rbp-120]          ; cellIndex
    mov [rbp-124], eax          ; nextEmptyFirstGroupIndex
    jmp .L106
    
.L105:
    inc dword [rbp-120]         ; cellIndex++
    
.L104:
    cmp [rbp-120], r12d         ; cellIndex < total cells
    jl .L107
    
.L106:
    ; Move cells from second group to first group
    mov eax, r12d
    dec eax
    mov [rbp-116], eax          ; groupCellIndex = total cells - 1
    jmp .L108
    
.L110:
    ; Check if cell exists in second group
    mov rax, [rbp-48]           ; groupCells
    mov edx, [rbp-96]           ; secondCellGroupIndex
    movsxd rdx, edx
    mov esi, r12d               ; cells per row
    imul rdx, rsi
    mov ecx, [rbp-116]          ; groupCellIndex
    movsxd rcx, ecx
    add rdx, rcx
    mov eax, [rax+rdx*4]
    cmp eax, NO_CELL
    je .L109
    
    ; Get cell to move
    mov [rbp-92], eax           ; cellToMove
    
    ; Move cell to first group
    mov rax, [rbp-48]           ; groupCells
    mov edx, [rbp-104]          ; firstCellGroupIndex
    movsxd rdx, edx
    mov esi, r12d               ; cells per row
    imul rdx, rsi
    mov ecx, [rbp-124]          ; nextEmptyFirstGroupIndex
    movsxd rcx, ecx
    add rdx, rcx
    mov ecx, [rbp-92]           ; cellToMove
    mov [rax+rdx*4], ecx
    
    inc dword [rbp-124]         ; nextEmptyFirstGroupIndex++
    
    ; Update cellToGroup
    mov rax, [rbp-72]           ; cellToGroup
    mov edx, [rbp-92]           ; cellToMove
    movsxd rdx, edx
    mov ecx, [rbp-104]          ; firstCellGroupIndex
    mov [rax+rdx*4], ecx
    
    ; Remove cell from second group
    mov rax, [rbp-48]           ; groupCells
    mov edx, [rbp-96]           ; secondCellGroupIndex
    movsxd rdx, edx
    mov esi, r12d               ; cells per row
    imul rdx, rsi
    mov ecx, [rbp-116]          ; groupCellIndex
    movsxd rcx, ecx
    add rdx, rcx
    mov dword [rax+rdx*4], NO_CELL
    
    ; Check if maze is complete
    cmp [rbp-124], r12d         ; nextEmptyFirstGroupIndex >= total cells
    jl .L109
    
    mov byte [rbp-145], 1       ; mazeComplete = true
    
.L109:
    dec dword [rbp-116]         ; groupCellIndex--
    
.L108:
    cmp dword [rbp-116], 0
    jge .L110
    
    call sleepHalfSecond
    call printMaze
    
    cmp byte [rbp-145], 1       ; if (mazeComplete)
    je .L114
    
.L103:
    inc dword [rbp-128]         ; removeWallIndex++
    
.L102:
    mov eax, [InteriorWallCount_Glob]
    cmp [rbp-128], eax
    jl .L112
    jmp .L111
    
.L114:
    nop
    
.L111:
    add rsp, 144
    pop rbx
    pop r12
    pop rbp
    ret

; Function: buildMazePrim()
; Prim's algorithm - start with random cell, repeatedly pick random wall from frontier
; that connects to cell not in maze, remove wall and add new cell to maze
buildMazePrim:
    push rbp
    mov rbp, rsp
    sub rsp, 32
    
    ; Initialize algorithm state
    mov dword [FrontierWallCount_Glob], 0
    mov dword [rbp-28], 0       ; cellsInMaze = 0
    
    ; Start with a random cell
    call rand
    mov eax, [MazeRows_Glob]
    imul eax, [MazeColumns_Glob]
    mov ecx, eax
    cdq
    idiv ecx
    mov [rbp-20], edx           ; startCell
    
    ; Mark start cell as in maze
    mov rax, [CellInMaze_Glob]
    mov edx, [rbp-20]           ; startCell
    movsxd rdx, edx
    add rax, rdx
    mov byte [rax], 1
    inc dword [rbp-28]          ; cellsInMaze++
    
    ; Add all walls adjacent to start cell to frontier
    mov edi, [rbp-20]           ; startCell
    call addCellWallsToFrontier
    
    ; Continue until all cells are in the maze
    jmp .L117
    
.L120:
    ; Pick random wall from frontier
    call rand
    mov ecx, [FrontierWallCount_Glob]
    cdq
    idiv ecx
    mov [rbp-16], edx           ; randomWallIndex
    
    ; Get wall to check
    mov rax, [FrontierWalls_Glob]
    mov edx, [rbp-16]           ; randomWallIndex
    movsxd rdx, edx
    shl rdx, 2
    add rax, rdx
    mov eax, [rax]
    mov [rbp-12], eax           ; wallToCheck
    
    ; Remove wall from frontier (replace with last wall)
    mov rax, [FrontierWalls_Glob]
    mov edx, [FrontierWallCount_Glob]
    dec edx
    movsxd rdx, edx
    shl rdx, 2
    add rax, rdx
    mov eax, [rax]              ; last wall
    
    mov rax, [FrontierWalls_Glob]
    mov edx, [rbp-16]           ; randomWallIndex
    movsxd rdx, edx
    shl rdx, 2
    add rax, rdx
    mov [rax], eax              ; replace with last wall
    
    dec dword [FrontierWallCount_Glob]
    
    ; Get the two cells this wall connects
    mov rax, [WallToCells_Glob]
    mov edx, [rbp-12]           ; wallToCheck
    movsxd rdx, edx
    shl rdx, 3
    add rax, rdx
    mov rax, [rax]
    mov eax, [rax]
    mov [rbp-8], eax            ; firstCellIndex
    
    mov rax, [WallToCells_Glob]
    mov edx, [rbp-12]           ; wallToCheck
    movsxd rdx, edx
    shl rdx, 3
    add rax, rdx
    mov rax, [rax]
    mov eax, [rax+4]
    mov [rbp-4], eax            ; secondCellIndex
    
    ; Check if one cell is in maze and other is not
    mov rax, [CellInMaze_Glob]
    mov edx, [rbp-8]            ; firstCellIndex
    movsxd rdx, edx
    add rax, rdx
    movzx edx, byte [rax]
    
    mov rax, [CellInMaze_Glob]
    mov ecx, [rbp-4]            ; secondCellIndex
    movsxd rcx, ecx
    add rax, rcx
    movzx eax, byte [rax]
    
    cmp dl, al                  ; if different states
    je .L117
    
    ; Remove the wall
    mov rax, [WallsUp_Glob]
    mov edx, [rbp-12]           ; wallToCheck
    movsxd rdx, edx
    add rax, rdx
    mov byte [rax], 0
    
    mov dword [rbp-24], -1      ; outerCellIndex = -1
    
    ; Determine which cell is outside the maze
    mov rax, [CellInMaze_Glob]
    mov edx, [rbp-8]            ; firstCellIndex
    movsxd rdx, edx
    add rax, rdx
    movzx eax, byte [rax]
    test eax, eax
    jne .L118
    
    mov eax, [rbp-8]            ; firstCellIndex
    mov [rbp-24], eax           ; outerCellIndex
    jmp .L119
    
.L118:
    mov eax, [rbp-4]            ; secondCellIndex
    mov [rbp-24], eax           ; outerCellIndex
    
.L119:
    ; Add outer cell to maze
    mov rax, [CellInMaze_Glob]
    mov edx, [rbp-24]           ; outerCellIndex
    movsxd rdx, edx
    add rax, rdx
    mov byte [rax], 1
    inc dword [rbp-28]          ; cellsInMaze++
    
    ; Add walls of outer cell to frontier
    mov edi, [rbp-24]           ; outerCellIndex
    call addCellWallsToFrontier
    
    call sleepHalfSecond
    call printMaze
    
.L117:
    mov eax, [MazeRows_Glob]
    imul eax, [MazeColumns_Glob]
    cmp [rbp-28], eax           ; cellsInMaze < total cells
    jl .L120
    
    leave
    ret

; Function: buildMazeDepthFirst()
; Depth-first search maze generation using recursive backtracking
; Start at random cell, randomly walk to neighbors outside maze, removing walls
; When all neighbors are in maze, backtrack to cell with neighbors outside maze
buildMazeDepthFirst:
    push rbp
    mov rbp, rsp
    push rbx
    sub rsp, 120
    
    ; Create cell stack on stack
    mov eax, [MazeRows_Glob]
    imul eax, [MazeColumns_Glob]
    shl eax, 2                  ; * sizeof(int)
    sub rsp, rax
    mov [rbp-56], rsp           ; cellStack
    
    mov dword [rbp-124], 0      ; stackSize = 0
    
    ; Start with random cell
    call rand
    mov eax, [MazeRows_Glob]
    imul eax, [MazeColumns_Glob]
    mov ecx, eax
    cdq
    idiv ecx
    mov [rbp-120], edx          ; currentCellIndex
    
    ; Mark as visited
    mov rax, [CellVisited_Glob]
    mov edx, [rbp-120]          ; currentCellIndex
    movsxd rdx, edx
    add rax, rdx
    mov byte [rax], 1
    
    ; Push starting cell onto stack
    mov rax, [rbp-56]           ; cellStack
    mov edx, [rbp-124]          ; stackSize
    movsxd rdx, edx
    mov ecx, [rbp-120]          ; currentCellIndex
    mov [rax+rdx*4], ecx
    inc dword [rbp-124]         ; stackSize++
    
    jmp .L125
    
.L142:
    ; Create randomized direction list
    mov dword [rbp-48], LEFT    ; randomizedDirections[0] = LEFT
    mov dword [rbp-44], UP      ; randomizedDirections[1] = UP
    mov dword [rbp-40], RIGHT   ; randomizedDirections[2] = RIGHT
    mov dword [rbp-36], DOWN    ; randomizedDirections[3] = DOWN
    
    ; Fisher-Yates shuffle the directions
    mov dword [rbp-116], 3      ; shuffleIndex = 3
    jmp .L126
    
.L127:
    call rand
    mov edx, [rbp-116]          ; shuffleIndex
    inc edx
    cdq
    idiv edx
    mov [rbp-72], edx           ; randomIndex
    
    ; Swap randomizedDirections[shuffleIndex] with randomizedDirections[randomIndex]
    mov eax, [rbp-116]          ; shuffleIndex
    cltq
    mov eax, [rbp-48+rax*4]     ; randomizedDirections[shuffleIndex]
    mov [rbp-68], eax           ; temp
    
    mov eax, [rbp-72]           ; randomIndex
    cltq
    mov edx, [rbp-48+rax*4]     ; randomizedDirections[randomIndex]
    mov eax, [rbp-116]          ; shuffleIndex
    cltq
    mov [rbp-48+rax*4], edx
    
    mov eax, [rbp-72]           ; randomIndex
    cltq
    mov edx, [rbp-68]           ; temp
    mov [rbp-48+rax*4], edx
    
    dec dword [rbp-116]         ; shuffleIndex--
    
.L126:
    cmp dword [rbp-116], 0
    jg .L127
    
    mov byte [rbp-125], 0       ; foundNeighbor = false
    mov dword [rbp-112], NO_CELL ; nextCellIndex = NO_CELL
    mov dword [rbp-108], NO_CELL ; wallIndex = NO_CELL
    
    ; Check directions in random order for unvisited neighbor
    mov dword [rbp-104], 0      ; directionIndex = 0
    jmp .L128
    
.L133:
    mov eax, [rbp-104]          ; directionIndex
    cltq
    mov eax, [rbp-48+rax*4]     ; direction = randomizedDirections[directionIndex]
    mov [rbp-96], eax
    
    ; Get wall index for this direction
    mov rax, [CellToWalls_Glob]
    mov edx, [rbp-120]          ; currentCellIndex
    movsxd rdx, edx
    shl rdx, 3
    add rax, rdx
    mov rax, [rax]
    mov edx, [rbp-96]           ; direction
    movsxd rdx, edx
    shl rdx, 2
    add rax, rdx
    mov eax, [rax]
    mov [rbp-108], eax          ; wallIndex
    
    cmp dword [rbp-108], NO_CELL
    je .L129
    
    ; Find cell on other side of wall
    mov rax, [WallToCells_Glob]
    mov edx, [rbp-108]          ; wallIndex
    movsxd rdx, edx
    shl rdx, 3
    add rax, rdx
    mov rax, [rax]
    mov eax, [rax]
    mov [rbp-92], eax           ; firstCellIndex
    
    mov rax, [WallToCells_Glob]
    mov edx, [rbp-108]          ; wallIndex
    movsxd rdx, edx
    shl rdx, 3
    add rax, rdx
    mov rax, [rax]
    mov eax, [rax+4]
    mov [rbp-88], eax           ; secondCellIndex
    
    ; Determine which is the next cell
    mov eax, [rbp-92]           ; firstCellIndex
    cmp eax, [rbp-120]          ; currentCellIndex
    jne .L130
    
    mov eax, [rbp-88]           ; secondCellIndex
    mov [rbp-112], eax          ; nextCellIndex
    jmp .L131
    
.L130:
    mov eax, [rbp-92]           ; firstCellIndex
    mov [rbp-112], eax          ; nextCellIndex
    
.L131:
    ; Check if neighbor is unvisited
    mov rax, [CellVisited_Glob]
    mov edx, [rbp-112]          ; nextCellIndex
    movsxd rdx, edx
    add rax, rdx
    movzx eax, byte [rax]
    test eax, eax
    jne .L129
    
    mov byte [rbp-125], 1       ; foundNeighbor = true
    jmp .L132
    
.L129:
    inc dword [rbp-104]         ; directionIndex++
    
.L128:
    cmp dword [rbp-104], 4
    jl .L133
    
.L132:
    cmp byte [rbp-125], 1       ; if (foundNeighbor)
    jne .L134
    
    ; Find wall between current and next cell
    mov dword [rbp-108], NO_CELL ; wallIndex = NO_CELL
    mov dword [rbp-100], 0      ; direction = 0
    jmp .L135
    
.L140:
    mov rax, [CellToWalls_Glob]
    mov edx, [rbp-120]          ; currentCellIndex
    movsxd rdx, edx
    shl rdx, 3
    add rax, rdx
    mov rax, [rax]
    mov edx, [rbp-100]          ; direction
    movsxd rdx, edx
    shl rdx, 2
    add rax, rdx
    mov eax, [rax]
    mov [rbp-84], eax           ; wallCandidate
    
    cmp dword [rbp-84], NO_CELL
    je .L136
    
    ; Check if this wall connects current and next cells
    mov rax, [WallToCells_Glob]
    mov edx, [rbp-84]           ; wallCandidate
    movsxd rdx, edx
    shl rdx, 3
    add rax, rdx
    mov rax, [rax]
    mov eax, [rax]
    mov [rbp-80], eax           ; firstCellIndex
    
    mov rax, [WallToCells_Glob]
    mov edx, [rbp-84]           ; wallCandidate
    movsxd rdx, edx
    shl rdx, 3
    add rax, rdx
    mov rax, [rax]
    mov eax, [rax+4]
    mov [rbp-76], eax           ; secondCellIndex
    
    ; Check if wall connects current and next
    mov eax, [rbp-80]           ; firstCellIndex
    cmp eax, [rbp-120]          ; currentCellIndex
    jne .L137
    mov eax, [rbp-76]           ; secondCellIndex
    cmp eax, [rbp-112]          ; nextCellIndex
    je .L138
    
.L137:
    mov eax, [rbp-80]           ; firstCellIndex
    cmp eax, [rbp-112]          ; nextCellIndex
    jne .L136
    mov eax, [rbp-76]           ; secondCellIndex
    cmp eax, [rbp-120]          ; currentCellIndex
    jne .L136
    
.L138:
    mov eax, [rbp-84]           ; wallCandidate
    mov [rbp-108], eax          ; wallIndex
    jmp .L139
    
.L136:
    inc dword [rbp-100]         ; direction++
    
.L135:
    cmp dword [rbp-100], 4
    jl .L140
    
.L139:
    ; Remove wall
    mov rax, [WallsUp_Glob]
    mov edx, [rbp-108]          ; wallIndex
    movsxd rdx, edx
    add rax, rdx
    mov byte [rax], 0
    
    ; Mark next cell as visited
    mov rax, [CellVisited_Glob]
    mov edx, [rbp-112]          ; nextCellIndex
    movsxd rdx, edx
    add rax, rdx
    mov byte [rax], 1
    
    ; Push next cell onto stack
    mov rax, [rbp-56]           ; cellStack
    mov edx, [rbp-124]          ; stackSize
    movsxd rdx, edx
    mov ecx, [rbp-112]          ; nextCellIndex
    mov [rax+rdx*4], ecx
    inc dword [rbp-124]         ; stackSize++
    
    mov eax, [rbp-112]          ; nextCellIndex
    mov [rbp-120], eax          ; currentCellIndex
    
    call sleepHalfSecond
    call printMaze
    jmp .L125
    
.L134:
    ; Backtrack - pop from stack
    dec dword [rbp-124]         ; stackSize--
    cmp dword [rbp-124], 0
    jle .L125
    
    ; Get previous cell from stack
    mov eax, [rbp-124]          ; stackSize
    dec eax
    mov rax, [rbp-56]           ; cellStack
    movsxd rdx, eax
    mov eax, [rax+rdx*4]
    mov [rbp-120], eax          ; currentCellIndex
    
.L125:
    cmp dword [rbp-124], 0      ; while (stackSize > 0)
    jg .L142
    
    add rsp, 120
    pop rbx
    pop rbp
    ret

; Function: buildMazeBinaryTree()
; Binary Tree maze generation algorithm
; For each cell, randomly choose to remove wall to north or east (if they exist)
buildMazeBinaryTree:
    push rbp
    mov rbp, rsp
    sub rsp, 48
    
    mov dword [rbp-48], 0       ; cellRow = 0
    jmp .L145
    
.L151:
    mov dword [rbp-44], 0       ; cellCol = 0
    jmp .L146
    
.L150:
    ; Calculate current cell index
    mov eax, [MazeColumns_Glob]
    imul eax, [rbp-48]          ; cellRow
    add eax, [rbp-44]           ; + cellCol
    mov [rbp-36], eax           ; currentCellIndex
    
    mov dword [rbp-40], 0       ; validWallCount = 0
    
    ; Check if we can go north (UP) - only if not in top row
    cmp dword [rbp-48], 0       ; cellRow > 0
    jle .L147
    
    mov rax, [CellToWalls_Glob]
    mov edx, [rbp-36]           ; currentCellIndex
    movsxd rdx, edx
    shl rdx, 3
    add rax, rdx
    mov rax, [rax]
    mov eax, [rax+4]            ; [UP]
    mov [rbp-32], eax           ; wallIndex
    
    cmp dword [rbp-32], NO_CELL
    je .L147
    
    mov eax, [rbp-40]           ; validWallCount
    cltq
    mov edx, [rbp-32]           ; wallIndex
    mov [rbp-16+rax*4], edx     ; validWalls[validWallCount] = wallIndex
    inc dword [rbp-40]          ; validWallCount++
    
.L147:
    ; Check if we can go east (RIGHT) - only if not in rightmost column
    mov eax, [MazeColumns_Glob]
    dec eax
    cmp [rbp-44], eax           ; cellCol < MazeColumns_Glob - 1
    jge .L148
    
    mov rax, [CellToWalls_Glob]
    mov edx, [rbp-36]           ; currentCellIndex
    movsxd rdx, edx
    shl rdx, 3
    add rax, rdx
    mov rax, [rax]
    mov eax, [rax+8]            ; [RIGHT]
    mov [rbp-28], eax           ; wallIndex
    
    cmp dword [rbp-28], NO_CELL
    je .L148
    
    mov eax, [rbp-40]           ; validWallCount
    cltq
    mov edx, [rbp-28]           ; wallIndex
    mov [rbp-16+rax*4], edx     ; validWalls[validWallCount] = wallIndex
    inc dword [rbp-40]          ; validWallCount++
    
.L148:
    ; If we have at least one valid wall, pick one randomly and remove it
    cmp dword [rbp-40], 0       ; validWallCount > 0
    jle .L149
    
    call rand
    cdq
    idiv dword [rbp-40]         ; % validWallCount
    mov [rbp-24], edx           ; randomWallIndex
    
    mov eax, [rbp-24]           ; randomWallIndex
    cltq
    mov eax, [rbp-16+rax*4]     ; validWalls[randomWallIndex]
    mov [rbp-20], eax           ; wallToRemove
    
    ; Remove the wall
    mov rax, [WallsUp_Glob]
    mov edx, [rbp-20]           ; wallToRemove
    movsxd rdx, edx
    add rax, rdx
    mov byte [rax], 0
    
.L149:
    call sleepHalfSecond
    call printMaze
    
    inc dword [rbp-44]          ; cellCol++
    
.L146:
    mov eax, [MazeColumns_Glob]
    cmp [rbp-44], eax
    jl .L150
    
    inc dword [rbp-48]          ; cellRow++
    
.L145:
    mov eax, [MazeRows_Glob]
    cmp [rbp-48], eax
    jl .L151
    
    leave
    ret

; Function: buildMazeRecursiveDivision()
; Recursive Division maze generation algorithm
; Start with empty area (all walls down), then recursively divide with walls, leaving random gaps
buildMazeRecursiveDivision:
    push rbp
    mov rbp, rsp
    sub rsp, 16
    
    ; Start with all interior walls down
    mov dword [rbp-4], 0        ; wallIndex = 0
    jmp .L154
    
.L155:
    mov rax, [WallsUp_Glob]
    mov edx, [rbp-4]            ; wallIndex
    movsxd rdx, edx
    add rax, rdx
    mov byte [rax], 0
    inc dword [rbp-4]           ; wallIndex++
    
.L154:
    mov eax, [InteriorWallCount_Glob]
    cmp [rbp-4], eax
    jl .L155
    
    call printMaze
    call sleepHalfSecond
    
    ; Recursively divide the entire maze area
    mov eax, [MazeColumns_Glob]
    dec eax
    mov ecx, eax                ; EndCol_Par = MazeColumns_Glob - 1
    mov eax, [MazeRows_Glob]
    dec eax
    mov esi, eax                ; EndRow_Par = MazeRows_Glob - 1
    mov edx, 0                  ; StartCol_Par = 0
    mov edi, 0                  ; StartRow_Par = 0
    call divideArea
    
    leave
    ret

; Function: main()
; Main function - prompts user for maze size and algorithm choice, then builds maze
_start:
    push rbp
    mov rbp, rsp
    push rbx
    sub rsp, 184
    
    ; Seed random number generator
    mov edi, 0
    call time
    mov edi, eax
    call srand
    
    ; Prompt for number of columns
    mov dword [MazeColumns_Glob], 0
    jmp .L157
    
.L166:
    ; Print prompt
    mov rdi, prompt_columns
    mov eax, 0
    call printf
    
    ; Read user input
    mov rdx, [stdin]
    mov rsi, 100
    lea rdi, [rbp-128]          ; userInput
    call fgets
    test rax, rax
    je .L157
    
    ; Remove newline if present
    lea rdi, [rbp-128]
    call strlen
    mov [rbp-132], eax          ; len
    
    cmp dword [rbp-132], 0
    jle .L159
    mov eax, [rbp-132]
    dec eax
    cltq
    cmp byte [rbp-128+rax], 10  ; '\n'
    jne .L159
    mov byte [rbp-128+rax], 0   ; remove newline
    
.L159:
    ; Validate input is numeric
    mov byte [rbp-191], 1       ; isValid = true
    cmp byte [rbp-128], 0       ; empty string check
    jne .L160
    mov byte [rbp-191], 0       ; isValid = false
    jmp .L161
    
.L160:
    mov dword [rbp-188], 0      ; characterIndex = 0
    jmp .L162
    
.L165:
    mov eax, [rbp-188]          ; characterIndex
    cltq
    movzx eax, byte [rbp-128+rax]
    cmp al, 47                  ; < '0'
    jle .L163
    cmp al, 57                  ; > '9'
    jle .L164
    
.L163:
    mov byte [rbp-191], 0       ; isValid = false
    jmp .L161
    
.L164:
    inc dword [rbp-188]         ; characterIndex++
    
.L162:
    mov eax, [rbp-188]          ; characterIndex
    cltq
    cmp byte [rbp-128+rax], 0
    jne .L165
    
.L161:
    cmp byte [rbp-191], 1       ; if (isValid)
    jne .L157
    
    lea rdi, [rbp-128]          ; userInput
    call atoi
    mov [MazeColumns_Glob], eax
    
.L157:
    cmp dword [MazeColumns_Glob], 0
    jle .L166
    
    ; Prompt for number of rows (similar logic as columns)
    mov dword [MazeRows_Glob], 0
    jmp .L167
    
.L176:
    mov rdi, prompt_rows
    mov eax, 0
    call printf
    
    ; Read and validate rows input (similar to columns)
    mov rdx, [stdin]
    mov rsi, 100
    lea rdi, [rbp-128]
    call fgets
    test rax, rax
    je .L167
    
    ; Process input same as columns
    lea rdi, [rbp-128]
    call strlen
    mov [rbp-136], eax
    
    cmp dword [rbp-136], 0
    jle .L169
    mov eax, [rbp-136]
    dec eax
    cltq
    cmp byte [rbp-128+rax], 10
    jne .L169
    mov byte [rbp-128+rax], 0
    
.L169:
    mov byte [rbp-190], 1
    cmp byte [rbp-128], 0
    jne .L170
    mov byte [rbp-190], 0
    jmp .L171
    
.L170:
    mov dword [rbp-184], 0
    jmp .L172
    
.L175:
    mov eax, [rbp-184]
    cltq
    movzx eax, byte [rbp-128+rax]
    cmp al, 47
    jle .L173
    cmp al, 57
    jle .L174
    
.L173:
    mov byte [rbp-190], 0
    jmp .L171
    
.L174:
    inc dword [rbp-184]
    
.L172:
    mov eax, [rbp-184]
    cltq
    cmp byte [rbp-128+rax], 0
    jne .L175
    
.L171:
    cmp byte [rbp-190], 1
    jne .L167
    
    lea rdi, [rbp-128]
    call atoi
    mov [MazeRows_Glob], eax
    
.L167:
    cmp dword [MazeRows_Glob], 0
    jle .L176
    
    ; Prompt for algorithm choice
    mov dword [rbp-180], 0      ; algorithmChoice = 0
    jmp .L177
    
.L186:
    ; Print algorithm menu
    mov rdi, prompt_algorithm
    call printf
    mov rdi, alg_kruskal
    call printf
    mov rdi, alg_prim
    call printf
    mov rdi, alg_depth
    call printf
    mov rdi, alg_binary
    call printf
    mov rdi, alg_recursive
    call printf
    
    ; Read and validate algorithm choice (similar logic)
    mov rdx, [stdin]
    mov rsi, 100
    lea rdi, [rbp-128]
    call fgets
    test rax, rax
    je .L177
    
    ; Process algorithm input
    lea rdi, [rbp-128]
    call strlen
    mov [rbp-140], eax
    
    cmp dword [rbp-140], 0
    jle .L179
    mov eax, [rbp-140]
    dec eax
    cltq
    cmp byte [rbp-128+rax], 10
    jne .L179
    mov byte [rbp-128+rax], 0
    
.L179:
    mov byte [rbp-189], 1
    cmp byte [rbp-128], 0
    jne .L180
    mov byte [rbp-189], 0
    jmp .L181
    
.L180:
    mov dword [rbp-176], 0
    jmp .L182
    
.L185:
    mov eax, [rbp-176]
    cltq
    movzx eax, byte [rbp-128+rax]
    cmp al, 47
    jle .L183
    cmp al, 57
    jle .L184
    
.L183:
    mov byte [rbp-189], 0
    jmp .L181
    
.L184:
    inc dword [rbp-176]
    
.L182:
    mov eax, [rbp-176]
    cltq
    cmp byte [rbp-128+rax], 0
    jne .L185
    
.L181:
    cmp byte [rbp-189], 1
    jne .L177
    
    lea rdi, [rbp-128]
    call atoi
    mov [rbp-180], eax          ; algorithmChoice
    
.L177:
    cmp dword [rbp-180], 1      ; algorithmChoice < 1
    jl .L186
    cmp dword [rbp-180], 5      ; algorithmChoice > 5
    jg .L186
    
    ; Setup maze data structures
    mov eax, [MazeColumns_Glob]
    cltq
    mov rdi, rax
    call malloc
    mov [AllHorizontalWallsUp_Glob], rax
    
    ; Initialize AllHorizontalWallsUp_Glob
    mov dword [rbp-172], 0      ; columnIndex = 0
    jmp .L187
    
.L188:
    mov rax, [AllHorizontalWallsUp_Glob]
    mov edx, [rbp-172]          ; columnIndex
    movsxd rdx, edx
    add rax, rdx
    mov byte [rax], 1
    inc dword [rbp-172]         ; columnIndex++
    
.L187:
    mov eax, [MazeColumns_Glob]
    cmp [rbp-172], eax
    jl .L188
    
    ; Calculate InteriorWallCount_Glob
    mov eax, [MazeColumns_Glob]
    dec eax
    imul eax, [MazeRows_Glob]   ; MazeRows_Glob * (MazeColumns_Glob - 1)
    mov edx, eax
    mov eax, [MazeRows_Glob]
    dec eax
    imul eax, [MazeColumns_Glob] ; (MazeRows_Glob - 1) * MazeColumns_Glob
    add eax, edx
    mov [InteriorWallCount_Glob], eax
    
    ; Allocate WallsUp_Glob
    mov eax, [InteriorWallCount_Glob]
    cltq
    mov rdi, rax
    call malloc
    mov [WallsUp_Glob], rax
    
    ; Initialize WallsUp_Glob to all true
    mov dword [rbp-168], 0      ; wallIndex = 0
    jmp .L189
    
.L190:
    mov rax, [WallsUp_Glob]
    mov edx, [rbp-168]          ; wallIndex
    movsxd rdx, edx
    add rax, rdx
    mov byte [rax], 1
    inc dword [rbp-168]         ; wallIndex++
    
.L189:
    mov eax, [InteriorWallCount_Glob]
    cmp [rbp-168], eax
    jl .L190
    
    ; Allocate algorithm state globals
    mov eax, [MazeRows_Glob]
    imul eax, [MazeColumns_Glob]
    cltq
    mov rdi, rax
    call malloc
    mov [CellVisited_Glob], rax
    
    ; Initialize CellVisited_Glob
    mov dword [rbp-164], 0      ; cellIndex = 0
    jmp .L191
    
.L192:
    mov rax, [CellVisited_Glob]
    mov edx, [rbp-164]          ; cellIndex
    movsxd rdx, edx
    add rax, rdx
    mov byte [rax], 0
    inc dword [rbp-164]         ; cellIndex++
    
.L191:
    mov eax, [MazeRows_Glob]
    imul eax, [MazeColumns_Glob]
    cmp [rbp-164], eax
    jl .L192
    
    ; Allocate FrontierWalls_Glob
    mov eax, [InteriorWallCount_Glob]
    cltq
    shl rax, 2                  ; * sizeof(int)
    mov rdi, rax
    call malloc
    mov [FrontierWalls_Glob], rax
    
    ; Allocate CellInMaze_Glob
    mov eax, [MazeRows_Glob]
    imul eax, [MazeColumns_Glob]
    cltq
    mov rdi, rax
    callax, 4
    mov edx, [rbp-36]           ; rightCell
    mov [rax], edx
    
    inc dword [rbp-76]          ; wallIndex++
    inc dword [rbp-68]          ; verticalWallIndex++
    
.L34:
    mov eax, [MazeColumns_Glob]
    dec eax
    cmp [rbp-68], eax
    jl .L35
    
    ; Check if we should process horizontal walls
    mov eax, [InteriorWallCount_Glob]
    cmp [rbp-76], eax           ; wallIndex < InteriorWallCount_Glob
    jge .L36
    
    ; Process horizontal walls
    mov dword [rbp-64], 0       ; horizontalWallIndex = 0
    jmp .L37
    
.L38:
    mov eax, [rbp-52]           ; firstCellInRow
    add eax, [rbp-64]           ; + horizontalWallIndex
    mov [rbp-48], eax           ; upperCell
    mov eax, [MazeColumns_Glob]
    add eax, [rbp-48]
    mov [rbp-44], eax           ; lowerCell = upperCell + MazeColumns_Glob
    
    ; WallToCells_Glob[wallIndex][FIRST_CELL] = upperCell
    mov rax, [WallToCells_Glob]
    mov edx, [rbp-76]           ; wallIndex
    movsxd rdx, edx
    shl rdx, 3
    add rax, rdx
    mov rax, [rax]
    mov edx, [rbp-48]           ; upperCell
    mov [rax], edx
    
    ; WallToCells_Glob[wallIndex][SECOND_CELL] = lowerCell
    mov rax, [WallToCells_Glob]
    mov edx, [rbp-76]           ; wallIndex
    movsxd rdx, edx
    shl rdx, 3
    add rax, rdx
    mov rax, [rax]
    add rax, 4
    mov edx, [rbp-44]           ; lowerCell
    mov [rax], edx
    
    inc dword [rbp-76]          ; wallIndex++
    inc dword [rbp-64]          ; horizontalWallIndex++
    
.L37:
    mov eax, [MazeColumns_Glob]
    cmp [rbp-64], eax
    jl .L38
    
.L36:
    inc dword [rbp-72]          ; rowIndex++
    
.L33:
    mov eax, [MazeRows_Glob]
    cmp [rbp-72], eax
    jl .L39
    
.L32:
    ; Build CellToWalls_Glob for algorithms that need cell-to-wall lookups
    cmp dword [rbp-84], PRIM_ALGORITHM
    je .L40
    cmp dword [rbp-84], DEPTH_FIRST_ALGORITHM
    je .L40
    cmp dword [rbp-84], BINARY_TREE_ALGORITHM
    je .L40
    cmp dword [rbp-84], RECURSIVE_DIVISION_ALGORITHM
    jne .L52
    
.L40:
    mov dword [rbp-60], 0       ; cellIndex = 0
    jmp .L42
    
.L51:
    ; Calculate cellRow and cellCol
    mov eax, [MazeColumns_Glob]
    mov ecx, eax
    mov eax, [rbp-60]           ; cellIndex
    cdq
    idiv ecx
    mov [rbp-32], eax           ; cellRow = cellIndex / MazeColumns_Glob
    mov [rbp-28], edx           ; cellCol = cellIndex % MazeColumns_Glob
    
    ; Initialize all directions to NO_CELL
    mov rax, [CellToWalls_Glob]
    mov edx, [rbp-60]           ; cellIndex
    movsxd rdx, edx
    shl rdx, 3
    add rax, rdx
    mov rax, [rax]
    mov dword [rax], NO_CELL    ; CellToWalls_Glob[cellIndex][LEFT]
    
    mov rax, [CellToWalls_Glob]
    mov edx, [rbp-60]           ; cellIndex
    movsxd rdx, edx
    shl rdx, 3
    add rax, rdx
    mov rax, [rax]
    add rax, 4
    mov dword [rax], NO_CELL    ; CellToWalls_Glob[cellIndex][UP]
    
    mov rax, [CellToWalls_Glob]
    mov edx, [rbp-60]           ; cellIndex
    movsxd rdx, edx
    shl rdx, 3
    add rax, rdx
    mov rax, [rax]
    add rax, 8
    mov dword [rax], NO_CELL    ; CellToWalls_Glob[cellIndex][RIGHT]
    
    mov rax, [CellToWalls_Glob]
    mov edx, [rbp-60]           ; cellIndex
    movsxd rdx, edx
    shl rdx, 3
    add rax, rdx
    mov rax, [rax]
    add rax, 12
    mov dword [rax], NO_CELL    ; CellToWalls_Glob[cellIndex][DOWN]
    
    ; Find walls by checking which walls connect to this cell
    mov dword [rbp-56], 0       ; wallIndex = 0
    jmp .L43
    
.L50:
    ; Get the two cells this wall connects
    mov rax, [WallToCells_Glob]
    mov edx, [rbp-56]           ; wallIndex
    movsxd rdx, edx
    shl rdx, 3
    add rax, rdx
    mov rax, [rax]
    mov eax, [rax]
    mov [rbp-24], eax           ; firstCellIndex
    
    mov rax, [WallToCells_Glob]
    mov edx, [rbp-56]           ; wallIndex
    movsxd rdx, edx
    shl rdx, 3
    add rax, rdx
    mov rax, [rax]
    mov eax, [rax+4]
    mov [rbp-20], eax           ; secondCellIndex
    
    ; Check if firstCellIndex == cellIndex
    mov eax, [rbp-24]           ; firstCellIndex
    cmp eax, [rbp-60]           ; cellIndex
    jne .L44
    
    ; This wall connects from our cell to secondCell
    ; Calculate secondRow and secondCol
    mov eax, [MazeColumns_Glob]
    mov edi, eax
    mov eax, [rbp-20]           ; secondCellIndex
    cdq
    idiv edi
    mov [rbp-8], eax            ; secondRow
    mov [rbp-4], edx            ; secondCol
    
    ; Check if wall goes RIGHT (secondRow == cellRow && secondCol == cellCol + 1)
    mov eax, [rbp-8]            ; secondRow
    cmp eax, [rbp-32]           ; cellRow
    jne .L45
    mov eax, [rbp-28]           ; cellCol
    inc eax
    cmp [rbp-4], eax            ; secondCol
    jne .L45
    
    ; Wall goes RIGHT
    mov rax, [CellToWalls_Glob]
    mov edx, [rbp-60]           ; cellIndex
    movsxd rdx, edx
    shl rdx, 3
    add rax, rdx
    mov rax, [rax]
    add rax, 8
    mov edx, [rbp-56]           ; wallIndex
    mov [rax], edx
    jmp .L47
    
.L45:
    ; Check if wall goes DOWN (secondRow == cellRow + 1 && secondCol == cellCol)
    mov eax, [rbp-32]           ; cellRow
    inc eax
    cmp [rbp-8], eax            ; secondRow
    jne .L47
    mov eax, [rbp-4]            ; secondCol
    cmp eax, [rbp-28]           ; cellCol
    jne .L47
    
    ; Wall goes DOWN
    mov rax, [CellToWalls_Glob]
    mov edx, [rbp-60]           ; cellIndex
    movsxd rdx, edx
    shl rdx, 3
    add rax, rdx
    mov rax, [rax]
    add rax, 12
    mov edx, [rbp-56]           ; wallIndex
    mov [rax], edx
    jmp .L47
    
.L44:
    ; Check if secondCellIndex == cellIndex
    mov eax, [rbp-20]           ; secondCellIndex
    cmp eax, [rbp-60]           ; cellIndex
    jne .L47
    
    ; This wall connects from firstCellIndex to our cell
    ; Calculate firstRow and firstCol
    mov eax, [MazeColumns_Glob]
    mov esi, eax
    mov eax, [rbp-24]           ; firstCellIndex
    cdq
    idiv esi
    mov [rbp-16], eax           ; firstRow
    mov [rbp-12], edx           ; firstCol
    
    ; Check if wall comes from LEFT (firstRow == cellRow && firstCol == cellCol - 1)
    mov eax, [rbp-16]           ; firstRow
    cmp eax, [rbp-32]           ; cellRow
    jne .L48
    mov eax, [rbp-28]           ; cellCol
    dec eax
    cmp [rbp-12], eax           ; firstCol
    jne .L48
    
    ; Wall comes from LEFT
    mov rax, [CellToWalls_Glob]
    mov edx, [rbp-60]           ; cellIndex
    movsxd rdx, edx
    shl rdx, 3
    add rax, rdx
    mov rax, [rax]
    mov edx, [rbp-56]           ; wallIndex
    mov [rax], edx
    jmp .L47
    
.L48:
    ; Check if wall comes from UP (firstRow == cellRow - 1 && firstCol == cellCol)
    mov eax, [rbp-32]           ; cellRow
    dec eax
    cmp [rbp-16], eax           ; firstRow
    jne .L47
    mov eax, [rbp-12]           ; firstCol
    cmp eax, [rbp-28]           ; cellCol
    jne .L47
    
    ; Wall comes from UP
    mov rax, [CellToWalls_Glob]
    mov edx, [rbp-60]           ; cellIndex
    movsxd rdx, edx
    shl rdx, 3
    add rax, rdx
    mov rax, [rax]
    add rax, 4
    mov edx, [rbp-56]           ; wallIndex
    mov [rax], edx
    
.L47:
    inc dword [rbp-56]          ; wallIndex++
    
.L43:
    mov eax, [InteriorWallCount_Glob]
    cmp [rbp-56], eax
    jl .L50
    
    inc dword [rbp-60]          ; cellIndex++
    
.L42:
    mov eax, [MazeRows_Glob]
    mov edx, [MazeColumns_Glob]
    imul eax, edx
    cmp [rbp-60], eax
    jl .L51
    
.L52:
    pop rbp
    ret

; Function: addWallToFrontier(int WallIndex_Par)
; Add a wall to frontier if not already there
addWallToFrontier:
    push rbp
    mov rbp, rsp
    mov [rbp-20], edi           ; WallIndex_Par
    
    mov byte [rbp-5], 0         ; alreadyInFrontier = false
    mov dword [rbp-4], 0        ; wallIndex = 0
    jmp .L54
    
.L57:
    mov rax, [FrontierWalls_Glob]
    mov edx, [rbp-4]            ; wallIndex
    movsxd rdx, edx
    shl rdx, 2
    add rax, rdx
    mov eax, [rax]
    cmp eax, [rbp-20]           ; WallIndex_Par
    jne .L55
    
    mov byte [rbp-5], 1         ; alreadyInFrontier = true
    jmp .L56
    
.L55:
    inc dword [rbp-4]           ; wallIndex++
    
.L54:
    mov eax, [FrontierWallCount_Glob]
    cmp [rbp-4], eax
    jl .L57
    
.L56:
    cmp byte [rbp-5], 0         ; if (alreadyInFrontier == false)
    jne .L59
    
    ; Add wall to frontier
    mov rax, [FrontierWalls_Glob]
    mov edx, [FrontierWallCount_Glob]
    movsxd rdx, edx
    shl rdx, 2
    add rax, rdx
    mov edx, [rbp-20]           ; WallIndex_Par
    mov [rax], edx
    inc dword [FrontierWallCount_Glob]
    
.L59:
    pop rbp
    ret

; Function: addCellWallsToFrontier(int CellIndex_Par)
; Add all walls adjacent to a cell to the frontier list
addCellWallsToFrontier:
    push rbp
    mov rbp, rsp
    sub rsp, 8
    mov [rbp-4], edi            ; CellIndex_Par
    
    ; Check UP direction
    mov rax, [CellToWalls_Glob]
    mov edx, [rbp-4]            ; CellIndex_Par
    movsxd rdx, edx
    shl rdx, 3
    add rax, rdx
    mov rax, [rax]
    add rax, 4                  ; [UP]
    mov eax, [rax]
    cmp eax, NO_CELL
    je .L61
    
    mov edi, eax
    call addWallToFrontier
    
.L61:
    ; Check DOWN direction
    mov rax, [CellToWalls_Glob]
    mov edx, [rbp-4]            ; CellIndex_Par
    movsxd rdx, edx
    shl rdx, 3
    add rax, rdx
    mov rax, [rax]
    add rax, 12                 ; [DOWN]
    mov eax, [rax]
    cmp eax, NO_CELL
    je .L62
    
    mov edi, eax
    call addWallToFrontier
    
.L62:
    ; Check LEFT direction
    mov rax, [CellToWalls_Glob]
    mov edx, [rbp-4]            ; CellIndex_Par
    movsxd rdx, edx
    shl rdx, 3
    add rax, rdx
    mov rax, [rax]              ; [LEFT]
    mov eax, [rax]
    cmp eax, NO_CELL
    je .L63
    
    mov edi, eax
    call addWallToFrontier
    
.L63:
    ; Check RIGHT direction
    mov rax, [CellToWalls_Glob]
    mov edx, [rbp-4]            ; CellIndex_Par
    movsxd rdx, edx
    shl rdx, 3
    add rax, rdx
    mov rax, [rax]
    add r
