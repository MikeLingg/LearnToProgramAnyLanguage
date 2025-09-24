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
    ; Global variables
    mazeColumns resd 1
    mazeRows resd 1
    interiorWallCount resd 1
    
    ; Working variables
    interiorWallIndex resd 1
    
    ; Random seed
    randomSeed resd 1
    
    ; Input buffer
    inputBuffer resb 20

section .text
    global _start

_start:
    call getUserInput
    call setupMaze
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
    cmp eax, 2
    jl .getColsAgain
    cmp eax, MAX_SIZE
    jg .getColsAgain
    mov [mazeColumns], eax
    jmp .getRows
    
.getColsAgain:
    jmp getUserInput
    
.getRows:
    ; Get rows
    mov rax, 1
    mov rdi, 1
    mov rsi, prompt_rows
    mov rdx, prompt_rows_len
    syscall
    
    call readInteger
    cmp eax, 2
    jl .getRowsAgain
    cmp eax, MAX_SIZE
    jg .getRowsAgain
    mov [mazeRows], eax
    ret
    
.getRowsAgain:
    jmp .getRows

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

; Setup maze data structures
setupMaze:
    ; Calculate interior wall count
    mov eax, [mazeRows]
    mov ebx, [mazeColumns]
    dec ebx
    mul ebx             ; eax = rows * (cols - 1)
    mov ecx, eax
    
    mov eax, [mazeRows]
    dec eax
    mov ebx, [mazeColumns]
    mul ebx             ; eax = (rows - 1) * cols
    add eax, ecx
    mov [interiorWallCount], eax
    
    ; Initialize allHorizontalWallsUp
    mov ecx, [mazeColumns]
    mov edi, allHorizontalWallsUp
    mov al, 1
    rep stosb
    
    ; Initialize wallsUp
    mov ecx, [interiorWallCount]
    mov edi, wallsUp
    mov al, 1
    rep stosb
    
    ; Initialize random seed with current time
    mov rax, 201        ; sys_time
    mov rdi, 0
    syscall
    mov [randomSeed], eax
    
    ret

; Print horizontal walls
printHorizontalWalls:
    ; rdi points to wall array
    push rdi
    
    ; Print "+"
    mov rax, 1
    mov rdi, 1
    mov rsi, plus_char
    mov rdx, 1
    syscall
    
    mov ecx, [mazeColumns]
    xor ebx, ebx
    pop rsi             ; wall array
    
.loop:
    cmp ebx, ecx
    jge .done
    
    ; Check wall state
    movzx eax, byte [rsi + rbx]  ; Use movzx to clear upper bits
    cmp eax, 1
    je .printDash
    
    ; Print space
    mov rax, 1
    mov rdi, 1
    push rsi
    push rbx
    push rcx
    mov rsi, space_char
    mov rdx, 1
    syscall
    pop rcx
    pop rbx
    pop rsi
    jmp .printPlus
    
.printDash:
    ; Print dash
    mov rax, 1
    mov rdi, 1
    push rsi
    push rbx
    push rcx
    mov rsi, dash_char
    mov rdx, 1
    syscall
    pop rcx
    pop rbx
    pop rsi
    
.printPlus:
    ; Print "+"
    mov rax, 1
    mov rdi, 1
    push rsi
    push rbx
    push rcx
    mov rsi, plus_char
    mov rdx, 1
    syscall
    pop rcx
    pop rbx
    pop rsi
    
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

; Print all horizontal walls (top/bottom borders)
printHorizontalWallsAll:
    mov rdi, allHorizontalWallsUp
    call printHorizontalWalls
    ret

; Print vertical walls
printVerticalWalls:
    ; rdi points to wall array
    push rdi
    
    ; Print first "|"
    mov rax, 1
    mov rdi, 1
    mov rsi, pipe_char
    mov rdx, 1
    syscall
    
    mov ecx, [mazeColumns]
    dec ecx             ; columns - 1 vertical walls
    xor ebx, ebx
    pop rsi             ; wall array
    
.loop:
    cmp ebx, ecx
    jge .printLast
    
    ; Print space
    mov rax, 1
    mov rdi, 1
    push rsi
    mov rsi, space_char
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall state
    cmp byte [rsi + rbx], 1
    je .printPipe
    
    ; Print space
    mov rax, 1
    mov rdi, 1
    push rsi
    mov rsi, space_char
    mov rdx, 1
    syscall
    pop rsi
    jmp .next
    
.printPipe:
    ; Print pipe
    mov rax, 1
    mov rdi, 1
    push rsi
    mov rsi, pipe_char
    mov rdx, 1
    syscall
    pop rsi
    
.next:
    inc ebx
    jmp .loop
    
.printLast:
    ; Print final space and "|"
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

; Print complete maze
printMaze:
    mov dword [interiorWallIndex], 0
    
    ; Print top border - always all walls up
    call printTopBorder
    
    ; Print maze rows
    xor eax, eax        ; row index
    
.rowLoop:
    cmp eax, [mazeRows]
    jge .done
    push rax            ; save row index
    
    ; Print vertical walls for this row
    call printVerticalWallsForRow
    
    pop rax             ; restore row index
    push rax            ; save again
    
    ; Check if last row
    mov ebx, [mazeRows]
    dec ebx
    cmp eax, ebx
    je .printBottom
    
    ; Print horizontal walls below this row
    call printHorizontalWallsForRow
    jmp .nextRow
    
.printBottom:
    call printTopBorder  ; Bottom border same as top
    
.nextRow:
    pop rax             ; restore row index
    inc eax             ; next row
    jmp .rowLoop
    
.done:
    ; Clean up stack if needed
    test eax, eax
    jz .skipPop
    pop rax
.skipPop:
    ; Print extra newline
    mov rax, 1
    mov rdi, 1
    mov rsi, newline_char
    mov rdx, 1
    syscall
    ret

; Print top/bottom border (all walls up)
printTopBorder:
    ; Print "+"
    mov rax, 1
    mov rdi, 1
    mov rsi, plus_char
    mov rdx, 1
    syscall
    
    mov ecx, [mazeColumns]
    xor ebx, ebx
    
.loop:
    cmp ebx, ecx
    jge .done
    
    ; Print "-"
    mov rax, 1
    mov rdi, 1
    push rbx
    push rcx
    mov rsi, dash_char
    mov rdx, 1
    syscall
    pop rcx
    pop rbx
    
    ; Print "+"
    mov rax, 1
    mov rdi, 1
    push rbx
    push rcx
    mov rsi, plus_char
    mov rdx, 1
    syscall
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

; Print vertical walls for current row
printVerticalWallsForRow:
    ; Print first "|"
    mov rax, 1
    mov rdi, 1
    mov rsi, pipe_char
    mov rdx, 1
    syscall
    
    mov ecx, [mazeColumns]
    dec ecx             ; columns - 1 vertical walls
    xor ebx, ebx        ; column index
    
.loop:
    cmp ebx, ecx
    jge .printLast
    
    ; Print space for cell content
    mov rax, 1
    mov rdi, 1
    push rbx
    push rcx
    mov rsi, space_char
    mov rdx, 1
    syscall
    pop rcx
    pop rbx
    
    ; Check if wall is up
    mov eax, [interiorWallIndex]
    movzx edx, byte [wallsUp + rax]
    cmp edx, 1
    je .printPipe
    
    ; Print space (no wall)
    mov rax, 1
    mov rdi, 1
    push rbx
    push rcx
    mov rsi, space_char
    mov rdx, 1
    syscall
    pop rcx
    pop rbx
    jmp .next
    
.printPipe:
    ; Print pipe (wall)
    mov rax, 1
    mov rdi, 1
    push rbx
    push rcx
    mov rsi, pipe_char
    mov rdx, 1
    syscall
    pop rcx
    pop rbx
    
.next:
    inc dword [interiorWallIndex]
    inc ebx
    jmp .loop
    
.printLast:
    ; Print final space and "|"
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

; Print horizontal walls for current row
printHorizontalWallsForRow:
    ; Print "+"
    mov rax, 1
    mov rdi, 1
    mov rsi, plus_char
    mov rdx, 1
    syscall
    
    mov ecx, [mazeColumns]
    xor ebx, ebx
    
.loop:
    cmp ebx, ecx
    jge .done
    
    ; Check if wall is up
    mov eax, [interiorWallIndex]
    movzx edx, byte [wallsUp + rax]
    cmp edx, 1
    je .printDash
    
    ; Print space (no wall)
    mov rax, 1
    mov rdi, 1
    push rbx
    push rcx
    mov rsi, space_char
    mov rdx, 1
    syscall
    pop rcx
    pop rbx
    jmp .printPlus
    
.printDash:
    ; Print dash (wall)
    mov rax, 1
    mov rdi, 1
    push rbx
    push rcx
    mov rsi, dash_char
    mov rdx, 1
    syscall
    pop rcx
    pop rbx
    
.printPlus:
    ; Print "+"
    mov rax, 1
    mov rdi, 1
    push rbx
    push rcx
    mov rsi, plus_char
    mov rdx, 1
    syscall
    pop rcx
    pop rbx
    
    inc dword [interiorWallIndex]
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

; Sleep for half second
sleepHalfSecond:
    mov rax, 35         ; sys_nanosleep
    mov rdi, sleepTime
    mov rsi, 0
    syscall
    ret

; Generate random number using linear congruential generator
getRandom:
    mov eax, [randomSeed]
    mov ebx, 1103515245
    mul ebx
    add eax, 12345
    mov [randomSeed], eax
    ret

; Build maze using Kruskal's algorithm
buildMazeKruskal:
    ; Build wall connections
    call buildWallConnections
    
    ; Initialize cell groups
    call initializeCellGroups
    
    ; Initialize wall remove list
    call initializeWallRemoveList
    
    ; Shuffle wall list
    call shuffleWallList
    
    ; Perform Kruskal's algorithm
    call performKruskalAlgorithm
    
    ret

; Build wall connections array
buildWallConnections:
    xor eax, eax        ; wall index
    xor ebx, ebx        ; row index
    
.rowLoop:
    cmp ebx, [mazeRows]
    jge .done
    
    ; Calculate first cell in row
    mov ecx, ebx
    imul ecx, [mazeColumns]
    
    ; Vertical walls
    xor edx, edx        ; vertical wall index
.verticalLoop:
    mov esi, [mazeColumns]
    dec esi
    cmp edx, esi
    jge .horizontalWalls
    
    ; Left cell = first cell + vertical wall index
    mov esi, ecx
    add esi, edx
    mov [wallConnections + rax*8], esi
    
    ; Right cell = left cell + 1
    inc esi
    mov [wallConnections + rax*8 + 4], esi
    
    inc eax
    inc edx
    jmp .verticalLoop
    
.horizontalWalls:
    ; Check if we have more walls to process
    cmp eax, [interiorWallCount]
    jge .nextRow
    
    ; Horizontal walls
    xor edx, edx        ; horizontal wall index
.horizontalLoop:
    cmp edx, [mazeColumns]
    jge .nextRow
    
    ; Upper cell = first cell + horizontal wall index
    mov esi, ecx
    add esi, edx
    mov [wallConnections + rax*8], esi
    
    ; Lower cell = upper cell + maze columns
    add esi, [mazeColumns]
    mov [wallConnections + rax*8 + 4], esi
    
    inc eax
    inc edx
    jmp .horizontalLoop
    
.nextRow:
    inc ebx
    jmp .rowLoop
    
.done:
    ret

; Initialize cell groups
initializeCellGroups:
    ; Calculate total cells
    mov eax, [mazeRows]
    imul eax, [mazeColumns]
    
    ; Initialize cellToGroup
    xor ebx, ebx
.initCellToGroup:
    cmp ebx, eax
    jge .initGroupCells
    mov [cellToGroup + rbx*4], ebx
    inc ebx
    jmp .initCellToGroup
    
.initGroupCells:
    ; Initialize groupCells - each cell in its own group
    xor ebx, ebx        ; cell index
    push rax            ; save total cells
.cellLoop:
    pop rax
    push rax
    cmp ebx, eax
    jge .done
    
    ; groupCells[cell][0] = cell
    mov ecx, ebx
    imul ecx, eax       ; cell * total cells
    shl ecx, 2          ; multiply by 4 for dword
    mov [groupCells + rcx], ebx
    
    ; Rest of group is NO_CELL (already initialized in .data)
    
    inc ebx
    jmp .cellLoop
    
.done:
    pop rax             ; clean up stack
    ret

; Initialize wall remove list
initializeWallRemoveList:
    mov ecx, [interiorWallCount]
    xor eax, eax
    
.loop:
    cmp eax, ecx
    jge .done
    mov [wallRemoveList + rax*4], eax
    inc eax
    jmp .loop
    
.done:
    ret

; Shuffle wall list using Fisher-Yates
shuffleWallList:
    mov ecx, [interiorWallCount]
    dec ecx
    
.loop:
    cmp ecx, 1
    jle .done
    
    ; Generate random index 0..ecx
    call getRandom
    xor edx, edx
    mov ebx, ecx
    inc ebx
    div ebx             ; edx = random % (ecx + 1)
    
    ; Swap wallRemoveList[ecx] with wallRemoveList[edx]
    mov eax, [wallRemoveList + rcx*4]
    mov ebx, [wallRemoveList + rdx*4]
    mov [wallRemoveList + rcx*4], ebx
    mov [wallRemoveList + rdx*4], eax
    
    dec ecx
    jmp .loop
    
.done:
    ret

; Perform Kruskal's algorithm main loop
performKruskalAlgorithm:
    mov ecx, [interiorWallCount]
    xor eax, eax        ; remove wall index
    
    ; Add bounds check
    cmp ecx, 0
    jle .done
    
.algorithmLoop:
    cmp eax, ecx
    jge .done
    push rax
    push rcx
    
    ; Bounds check for wall index
    cmp eax, MAX_WALLS
    jge .nextWall
    
    ; Get next wall to check
    mov ebx, [wallRemoveList + rax*4]
    
    ; Bounds check for wall connection
    cmp ebx, MAX_WALLS
    jge .nextWall
    
    ; Get connected cells
    mov edx, [wallConnections + rbx*8]      ; first cell
    mov esi, [wallConnections + rbx*8 + 4]  ; second cell
    
    ; Bounds check for cells
    mov edi, MAX_CELLS
    cmp edx, edi
    jge .nextWall
    cmp esi, edi
    jge .nextWall
    
    ; Get group indices
    mov edi, [cellToGroup + rdx*4]          ; first cell group
    mov r8d, [cellToGroup + rsi*4]          ; second cell group
    
    ; Check if different groups
    cmp edi, r8d
    je .nextWall
    
    ; Remove wall
    mov byte [wallsUp + rbx], 0
    
    ; Merge groups
    call mergeGroups    ; edi = first group, r8d = second group
    
    ; Sleep and print maze
    call sleepHalfSecond
    call printMaze
    
    ; Simple termination check - limit iterations
    pop rcx
    pop rax
    inc eax
    cmp eax, 50         ; Limit to 50 iterations to prevent infinite loop
    jge .done
    jmp .algorithmLoop
    
.nextWall:
    pop rcx
    pop rax
    inc eax
    cmp eax, 50         ; Same limit here
    jge .done
    jmp .algorithmLoop
    
.done:
    ret

; Merge groups (edi = first group, r8d = second group) - simplified version
mergeGroups:
    ; For now, just update cellToGroup for the second cell to point to first group
    ; This is a simplified merge that avoids the complex groupCells array manipulation
    
    ; Calculate total cells for bounds checking
    mov eax, [mazeRows]
    imul eax, [mazeColumns]
    
    ; Update all cells in second group to point to first group
    xor ebx, ebx        ; cell index
    
.updateLoop:
    cmp ebx, eax
    jge .done
    
    ; Check if this cell belongs to second group
    mov ecx, [cellToGroup + rbx*4]
    cmp ecx, r8d        ; compare with second group
    jne .nextCell
    
    ; Update to first group
    mov [cellToGroup + rbx*4], edi
    
.nextCell:
    inc ebx
    jmp .updateLoop
    
.done:
    ret
