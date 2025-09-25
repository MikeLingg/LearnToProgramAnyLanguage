section .data
    ; Define directions
    LEFT equ 0
    UP equ 1
    RIGHT equ 2
    DOWN equ 3
    
    FIRST_CELL equ 0
    SECOND_CELL equ 1
    
    NO_CELL equ -1
    
    ; Maximum sizes to avoid dynamic allocation complexity
    MAX_SIZE equ 20
    MAX_WALLS equ 800        ; 20*19 + 19*20 = 760 walls max
    MAX_CELLS equ 400        ; 20*20 = 400 cells max
    
    ; Global arrays
    allHorizontalWallsUp times MAX_SIZE db 1
    wallsUp times MAX_WALLS db 1
    wallConnections times MAX_WALLS*2 dd 0    ; 2 integers per wall
    wallRemoveList times MAX_WALLS dd 0
    cellToGroup times MAX_CELLS dd 0
    groupCells times MAX_CELLS*MAX_CELLS dd NO_CELL
    
    ; Sleep timespec structure
    sleepTime:
        dq 1, 0         ; 1 second, 0 nanoseconds
    
    ; Output characters
    plus_char db "+"
    dash_char db "-"
    pipe_char db "|"
    space_char db " "
    newline_char db 10
    
    ; Input prompts
    prompt_cols db "Please enter number of columns for maze (2-20): "
    prompt_cols_len equ $ - prompt_cols
    prompt_rows db "Please enter number of rows for maze (2-20): "
    prompt_rows_len equ $ - prompt_rows

section .bss
    ; Global variables - matching C++ names
    mazeColumns resd 1
    mazeRows resd 1
    interiorWallCount resd 1
    
    ; Loop counters matching C++ variable names  
    rowIndex resd 1
    columnIndex resd 1
    horizontalWallIndex resd 1
    verticalWallIndex resd 1
    
    ; Kruskal's algorithm variables matching C++
    interiorWallIndex resd 1
    wallIndex resd 1
    firstCellInRow resd 1
    leftCell resd 1
    rightCell resd 1
    upperCell resd 1
    lowerCell resd 1
    cellIndex resd 1
    innerCellIndex resd 1
    wallIndexLoop resd 1
    removeWallIndex resd 1
    nextWallToCheck resd 1
    firstCell resd 1
    firstCellGroupIndex resd 1
    secondCell resd 1
    secondCellGroupIndex resd 1
    nextEmptyFirstGroupIndex resd 1
    groupCellIndex resd 1
    cellToMove resd 1
    totalCells resd 1
    mazeComplete resd 1
    
    ; Random seed
    randomSeed resd 1
    
    ; Input buffer
    inputBuffer resb 20

section .text
    global _start

_start:
    call getUserInput
    call setupMaze
    call printMaze
    call buildMazeKruskal
    
    ; Exit
    mov rax, 60
    mov rdi, 0
    syscall

; Get user input for maze dimensions
getUserInput:
    ; Get columns
    mov rax, 1
    mov rdi, 1
    mov rsi, prompt_cols
    mov rdx, prompt_cols_len
    syscall
    
    call readInteger
    mov [mazeColumns], eax
    
    ; Get rows
    mov rax, 1
    mov rdi, 1
    mov rsi, prompt_rows
    mov rdx, prompt_rows_len
    syscall
    
    call readInteger
    mov [mazeRows], eax
; Setup maze data structures
setupMaze:
    ; Calculate interior wall count
    mov eax, [mazeRows]
    mov ebx, [mazeColumns]
    dec ebx
    mul ebx             ; eax = rows * (cols - 1)
    mov [interiorWallCount], eax
    
    mov eax, [mazeRows]
    dec eax
    mov ebx, [mazeColumns]
    mul ebx             ; eax = (rows - 1) * cols
    add eax, [interiorWallCount]
    mov [interiorWallCount], eax
    
    ; Calculate total cells
    mov eax, [mazeRows]
    mul dword [mazeColumns]
    mov [totalCells], eax
    
    ; Initialize allHorizontalWallsUp
    mov dword [horizontalWallIndex], 0
.initHorizontalLoop:
    mov eax, [horizontalWallIndex]
    cmp eax, [mazeColumns]
    jge .initWallsUp
    
    mov byte [allHorizontalWallsUp + eax], 1
    inc dword [horizontalWallIndex]
    jmp .initHorizontalLoop
    
.initWallsUp:
    ; Initialize wallsUp - make sure all are 1
    mov dword [wallIndex], 0
.initWallsLoop:
    mov eax, [wallIndex]
    cmp eax, [interiorWallCount]
    jge .initSeed
    
    mov byte [wallsUp + eax], 1
    inc dword [wallIndex]
    jmp .initWallsLoop
    
.initSeed:
    ; Initialize random seed with current time
    mov rax, 201        ; sys_time
    mov rdi, 0
    syscall
    mov [randomSeed], eax
    
; Build wall connections array
buildWallConnections:
    mov dword [wallIndex], 0
    mov dword [rowIndex], 0
    
.rowLoop:
    mov eax, [rowIndex]
    cmp eax, [mazeRows]
    jge .done
    
    ; Calculate first cell in row = rowIndex * mazeColumns
    mov eax, [rowIndex]
    mul dword [mazeColumns]
    mov [firstCellInRow], eax
    
    ; Vertical walls
    mov dword [verticalWallIndex], 0
.verticalLoop:
    mov eax, [verticalWallIndex]
    mov ebx, [mazeColumns]
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
    
    ; Store connection: wallConnections[wallIndex][0] = leftCell
    mov eax, [wallIndex]
    shl eax, 3          ; multiply by 8 (2 * 4 bytes)
    mov ebx, [leftCell]
    mov [wallConnections + eax], ebx
    mov ebx, [rightCell]
    mov [wallConnections + eax + 4], ebx
    
    inc dword [wallIndex]
    inc dword [verticalWallIndex]
    jmp .verticalLoop
    
.horizontalWalls:
    ; Check if we have more walls to process
    mov eax, [wallIndex]
    cmp eax, [interiorWallCount]
    jge .nextRow
    
    ; Horizontal walls
    mov dword [horizontalWallIndex], 0
.horizontalLoop:
    mov eax, [horizontalWallIndex]
    cmp eax, [mazeColumns]
    jge .nextRow
    
    ; upperCell = firstCellInRow + horizontalWallIndex
    mov eax, [firstCellInRow]
    add eax, [horizontalWallIndex]
    mov [upperCell], eax
    
    ; lowerCell = upperCell + mazeColumns
    add eax, [mazeColumns]
    mov [lowerCell], eax
    
    ; Store connection
    mov eax, [wallIndex]
    shl eax, 3          ; multiply by 8
    mov ebx, [upperCell]
    mov [wallConnections + eax], ebx
    mov ebx, [lowerCell]
    mov [wallConnections + eax + 4], ebx
    
    inc dword [wallIndex]
    inc dword [horizontalWallIndex]
    jmp .horizontalLoop
    
.nextRow:
    inc dword [rowIndex]
    jmp .rowLoop
    
.done:
    ret

; Initialize cell groups
initializeCellGroups:
    ; Initialize cellToGroup
    mov dword [cellIndex], 0
.initCellToGroup:
    mov eax, [cellIndex]
    cmp eax, [totalCells]
    jge .initGroupCells
    
    mov [cellToGroup + eax*4], eax
    inc dword [cellIndex]
    jmp .initCellToGroup
    
.initGroupCells:
    ; Initialize groupCells - each cell in its own group
    mov dword [cellIndex], 0
.cellLoop:
    mov eax, [cellIndex]
    cmp eax, [totalCells]
    jge .done
    
    ; groupCells[cell][0] = cell
    mov ebx, [totalCells]
    mul ebx             ; eax = cellIndex * totalCells
    shl eax, 2          ; multiply by 4 for dword addressing
    mov ebx, [cellIndex]
    mov [groupCells + eax], ebx
    
    inc dword [cellIndex]
    jmp .cellLoop
    
.done:
    ret

; Initialize wall remove list
initializeWallRemoveList:
    mov dword [wallIndexLoop], 0
.loop:
    mov eax, [wallIndexLoop]
    cmp eax, [interiorWallCount]
    jge .done
    
    mov [wallRemoveList + eax*4], eax
    inc dword [wallIndexLoop]
    jmp .loop
    
.done:
    ret

; Generate random number using linear congruential generator
getRandom:
    mov eax, [randomSeed]
    mov ebx, 1103515245
    mul ebx
    add eax, 12345
    mov [randomSeed], eax
    ret

; Shuffle wall list using Fisher-Yates
shuffleWallList:
    mov eax, [interiorWallCount]
    dec eax
    mov [wallIndexLoop], eax
    
.loop:
    mov eax, [wallIndexLoop]
    cmp eax, 1
    jle .done
    
    ; Generate random index 0..wallIndexLoop
    call getRandom
    mov eax, [randomSeed]
    xor edx, edx
    mov ebx, [wallIndexLoop]
    inc ebx
    div ebx             ; edx = random % (wallIndexLoop + 1)
    
    ; Swap wallRemoveList[wallIndexLoop] with wallRemoveList[edx]
    mov eax, [wallIndexLoop]
    mov ebx, [wallRemoveList + eax*4]
    mov ecx, [wallRemoveList + edx*4]
    mov [wallRemoveList + eax*4], ecx
    mov [wallRemoveList + edx*4], ebx
    
    dec dword [wallIndexLoop]
    jmp .loop
    
.done:
    ret

; Sleep for one second
sleepHalfSecond:
    mov rax, 35         ; sys_nanosleep
    mov rdi, sleepTime
    mov rsi, 0
    syscall
    ret

; Perform Kruskal's algorithm main loop
performKruskalAlgorithm:
    mov dword [mazeComplete], 0
    mov dword [removeWallIndex], 0
    
.algorithmLoop:
    mov eax, [removeWallIndex]
    cmp eax, [interiorWallCount]
    jge .done
    
    ; Get next wall to check
    mov eax, [removeWallIndex]
    mov ebx, [wallRemoveList + eax*4]
    mov [nextWallToCheck], ebx
    
    ; Get connected cells
    shl ebx, 3          ; multiply by 8
    mov eax, [wallConnections + ebx]
    mov [firstCell], eax
    mov eax, [wallConnections + ebx + 4]
    mov [secondCell], eax
    
    ; Get group indices
    mov eax, [firstCell]
    mov ebx, [cellToGroup + eax*4]
    mov [firstCellGroupIndex], ebx
    
    mov eax, [secondCell]
    mov ebx, [cellToGroup + eax*4]
    mov [secondCellGroupIndex], ebx
    
    ; Check if different groups
    mov eax, [firstCellGroupIndex]
    cmp eax, [secondCellGroupIndex]
    je .nextWall
    
    ; Remove wall
    mov eax, [nextWallToCheck]
    mov byte [wallsUp + eax], 0
    
    ; Merge groups (simplified version)
    call mergeGroups
    
    ; Sleep and print maze
    call sleepHalfSecond
    call printMaze
    
.nextWall:
    inc dword [removeWallIndex]
    jmp .algorithmLoop
    
.done:
    ret

; Simplified merge groups
mergeGroups:
    ; Update all cells in second group to point to first group
    mov dword [cellIndex], 0
    
.updateLoop:
    mov eax, [cellIndex]
    cmp eax, [totalCells]
    jge .done
    
    ; Check if this cell belongs to second group
    mov ebx, [cellToGroup + eax*4]
    cmp ebx, [secondCellGroupIndex]
    jne .nextCell
    
    ; Update to first group
    mov ebx, [firstCellGroupIndex]
    mov [cellToGroup + eax*4], ebx
    
.nextCell:
    inc dword [cellIndex]
    jmp .updateLoop
    
.done:
    ret

; Build maze using Kruskal's algorithm
buildMazeKruskal:
    call buildWallConnections
    call initializeCellGroups
    call initializeWallRemoveList
    call shuffleWallList
    call performKruskalAlgorithm
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

; Print vertical walls: | | | | (checking wallsUp array)
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
    mov ebx, [mazeColumns]
    dec ebx             ; columns - 1 interior vertical positions
    cmp eax, ebx
    jge .printLast
    
    ; Print space for cell content
    mov rax, 1
    mov rdi, 1
    mov rsi, space_char
    mov rdx, 1
    syscall
    
    ; Check if this wall is up in wallsUp array
    mov eax, [interiorWallIndex]
    movzx ebx, byte [wallsUp + eax]
    cmp ebx, 1
    je .printPipe
    
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

; Print horizontal walls between rows (checking wallsUp array)
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
    cmp eax, [mazeColumns]
    jge .done
    
    ; Check if this wall is up in wallsUp array
    mov eax, [interiorWallIndex]
    movzx ebx, byte [wallsUp + eax]
    cmp ebx, 1
    je .printDash
    
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

; Print maze - now checks wallsUp array for actual wall states
printMaze:
    ; Reset interior wall index  
    mov dword [interiorWallIndex], 0
    
    ; Print top border (always all walls up)
    call printTopBorder
    
    ; Initialize row counter
    mov dword [rowIndex], 0
    
.rowLoop:
    mov eax, [rowIndex]
    cmp eax, [mazeRows]
    jge .done
    
    ; Print vertical walls for this row
    call printVerticalWalls
    
    ; Check if this is the last row
    mov eax, [rowIndex]
    mov ebx, [mazeRows]
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
    cmp eax, [mazeColumns]
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
printFirstHorizontalWall:
    ; Print "+"
    mov rax, 1
    mov rdi, 1
    mov rsi, plus_char
    mov rdx, 1
    syscall
    
    ; Print columns number of "-+"
    mov ecx, [mazeColumns]
    xor ebx, ebx
    
.loop:
    cmp ebx, ecx
    jge .done
    
    ; Save registers before syscall
    push rbx
    push rcx
    
    ; Print "-"
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
    
    ; Restore registers after syscall
    pop rcx
    pop rbx
    
    inc ebx
    jmp .loop
    
.done:
    ; Print newline
    mov rax, 1
    mov rdi, 1
    mov rsi, newline_char
    mov rdx, 1
    syscall
    ret
