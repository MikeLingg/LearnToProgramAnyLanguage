section .data
    ; Constants
    FIRST_CELL equ 0
    SECOND_CELL equ 1
    NO_CELL equ -1
    
    ; Start with all the walls up.
    wallsUp times 19800 db 1    ; 100*99 + 99*100 max walls for 100x100
    
    ; Identify the cells each wall connects.
    wallConnections times 39600 db 0   ; 19800 walls * 2 cells
    
    ; Identify which group each cell is a part of.
    cellToGroup times 10000 db 0       ; 100*100 max cells
    
    ; Wall removal list
    wallRemoveList times 19800 db 0
    
    ; Group cells array - 10000 groups of 10000 cells each
    groupCells times 100000000 db NO_CELL

    timespec:
        dq 0            ; seconds = 0
        dq 500000000    ; nanoseconds = 500,000,000 (500ms)
        
    plus_char db "+"
    dash_char db "-"
    pipe_char db "|"
    space_char db " "
    newline_char db 10

    prompt_cols db "Enter maze columns (capped at 8 due to 8-bit variable use): "
    prompt_cols_len equ $ - prompt_cols
    prompt_rows db "Enter maze rows (capped at 8 due to 8-bit variable use): "
    prompt_rows_len equ $ - prompt_rows

    debug_cols_msg db "mazeColumns: "
    debug_cols_msg_len equ $ - debug_cols_msg
    debug_rows_msg db "mazeRows: "
    debug_rows_msg_len equ $ - debug_rows_msg

section .bss
    ; Variables
    currentInteriorWall resb 1
    mazeComplete resb 1
    removeWallIndex resb 1
    nextWallToCheck resb 1
    firstCell resb 1
    firstCellGroupIndex resb 1
    secondCell resb 1
    secondCellGroupIndex resb 1
    nextEmptyFirstGroupIndex resb 1
    cellToMove resb 1
    
    ; Loop variables
    cellIndex resb 1
    rowIndex resb 1
    groupCellIndex resb 1

    shuffleIndex_var resb 1
    otherIndex_var resb 1
    random_seed resq 1
    
    ; Maze dimensions
    mazeColumns resb 1
    mazeRows resb 1
    interiorWallCount resb 1
    totalCells resb 1
    wallIndex resb 1

    input_buffer resb 10

    temp_char resb 1

section .text
    global _start

_start:
; Prompt for maze columns
mov rax, 1
mov rdi, 1
mov rsi, prompt_cols
mov rdx, prompt_cols_len
syscall

; Read maze columns
mov rax, 0
mov rdi, 0
mov rsi, input_buffer
mov rdx, 10
syscall

; Convert multi-digit ASCII to number inline
xor eax, eax            ; Clear result
xor ebx, ebx            ; Clear digit
mov rsi, input_buffer   ; Point to input buffer

convert_cols_loop:
    mov bl, [rsi]           ; Get next character
    cmp bl, 10              ; Check for newline
    je convert_cols_done
    cmp bl, '0'             ; Check if valid digit
    jb convert_cols_done
    cmp bl, '9'
    ja convert_cols_done
    
    sub bl, '0'             ; Convert ASCII to digit
    imul eax, 10            ; Multiply current result by 10
    add eax, ebx            ; Add new digit
    
    ; Check if value exceeds 8
    cmp eax, 8
    jg cols_too_large
    
    inc rsi                 ; Next character
    jmp convert_cols_loop

cols_too_large:
    mov eax, 8              ; Cap at 8
    jmp convert_cols_done

convert_cols_done:
mov [mazeColumns], al

; Prompt for maze rows
mov rax, 1
mov rdi, 1
mov rsi, prompt_rows
mov rdx, prompt_rows_len
syscall

; Read maze rows
mov rax, 0
mov rdi, 0
mov rsi, input_buffer
mov rdx, 10
syscall

; Convert multi-digit ASCII to number inline
xor eax, eax            ; Clear result
xor ebx, ebx            ; Clear digit
mov rsi, input_buffer   ; Point to input buffer

convert_rows_loop:
    mov bl, [rsi]           ; Get next character
    cmp bl, 10              ; Check for newline
    je convert_rows_done
    cmp bl, '0'             ; Check if valid digit
    jb convert_rows_done
    cmp bl, '9'
    ja convert_rows_done
    
    sub bl, '0'             ; Convert ASCII to digit
    imul eax, 10            ; Multiply current result by 10
    add eax, ebx            ; Add new digit
    
    ; Check if value exceeds 8
    cmp eax, 8
    jg rows_too_large
    
    inc rsi                 ; Next character
    jmp convert_rows_loop

rows_too_large:
    mov eax, 8              ; Cap at 8
    jmp convert_rows_done

convert_rows_done:
mov [mazeRows], al

    ; Calculate interiorWallCount = mazeRows * (mazeColumns - 1) + (mazeRows - 1) * mazeColumns
    movzx eax, byte [mazeRows]
    movzx ebx, byte [mazeColumns]
    dec ebx
    mul ebx
    mov ecx, eax
    
    movzx eax, byte [mazeRows]
    dec eax
    movzx ebx, byte [mazeColumns]
    mul ebx
    add eax, ecx
    mov [interiorWallCount], al
    
    ; Calculate totalCells = mazeRows * mazeColumns
    movzx eax, byte [mazeRows]
    movzx ebx, byte [mazeColumns]
    mul ebx
    mov [totalCells], al
    
    ; Build wall connections dynamically
    mov byte [wallIndex], 0
    mov byte [rowIndex], 0
    
build_rows_loop:
    movzx eax, byte [rowIndex]
    movzx ebx, byte [mazeRows]
    cmp eax, ebx
    jge build_connections_done
    
    ; firstCellInRow = rowIndex * mazeColumns
    movzx eax, byte [rowIndex]
    movzx ebx, byte [mazeColumns]
    mul ebx
    mov [cellIndex], al  ; reusing as firstCellInRow
    
    ; Vertical walls
    mov byte [groupCellIndex], 0  ; reusing as verticalWallIndex
vertical_walls_loop:
    movzx eax, byte [groupCellIndex]
    movzx ebx, byte [mazeColumns]
    dec ebx
    cmp eax, ebx
    jge vertical_walls_done
    
    ; leftCell = firstCellInRow + verticalWallIndex
    mov al, [cellIndex]
    add al, [groupCellIndex]
    mov [firstCell], al
    
    ; rightCell = leftCell + 1
    inc al
    mov [secondCell], al
    
    ; wallConnections[wallIndex * 2] = leftCell
    movzx eax, byte [wallIndex]
    shl eax, 1
    mov bl, [firstCell]
    mov [wallConnections + rax], bl
    mov bl, [secondCell]
    mov [wallConnections + rax + 1], bl
    
    inc byte [wallIndex]
    inc byte [groupCellIndex]
    jmp vertical_walls_loop
    
vertical_walls_done:
    ; Horizontal walls
    movzx eax, byte [wallIndex]
    movzx ebx, byte [interiorWallCount]
    cmp eax, ebx
    jge skip_horizontal_walls
    
    mov byte [groupCellIndex], 0  ; reusing as horizontalWallIndex
horizontal_walls_loop:
    movzx eax, byte [groupCellIndex]
    movzx ebx, byte [mazeColumns]
    cmp eax, ebx
    jge horizontal_walls_done
    
    ; upperCell = firstCellInRow + horizontalWallIndex
    mov al, [cellIndex]
    add al, [groupCellIndex]
    mov [firstCell], al
    
    ; lowerCell = upperCell + mazeColumns
    movzx ebx, byte [mazeColumns]
    add eax, ebx
    mov [secondCell], al
    
    ; wallConnections[wallIndex * 2] = upperCell
    movzx eax, byte [wallIndex]
    shl eax, 1
    mov bl, [firstCell]
    mov [wallConnections + rax], bl
    mov bl, [secondCell]
    mov [wallConnections + rax + 1], bl
    
    inc byte [wallIndex]
    inc byte [groupCellIndex]
    jmp horizontal_walls_loop
    
horizontal_walls_done:
skip_horizontal_walls:
    inc byte [rowIndex]
    jmp build_rows_loop
    
build_connections_done:

    ; Initialize cellToGroup
    movzx ecx, byte [totalCells]
    xor eax, eax
init_cell_groups:
    mov [cellToGroup + rax], al
    inc eax
    dec ecx
    jnz init_cell_groups
    
    ; Initialize groupCells array - each cell starts in its own group
    ; Set each group's first cell: groupCells[group][0] = group
    movzx ecx, byte [totalCells]
    xor eax, eax                    ; eax = current cell/group index

init_groups_loop:
    cmp eax, ecx
    jge init_groups_done
    
    ; Calculate groupCells[group][0] address = group * totalCells + 0
    push rax                    ; Save current cell number
    movzx ebx, byte [totalCells]
    mul ebx                     ; eax = group * totalCells
    pop rbx                     ; ebx = cell number to store
    mov [groupCells + rax], bl  ; Store cell number in its own group
    
    mov eax, ebx                ; Restore cell counter
    inc eax                     ; Next cell/group
    jmp init_groups_loop
    
init_groups_done:

    ; Initialize wallRemoveList
    movzx ecx, byte [interiorWallCount]
    xor eax, eax
init_wall_list:
    mov [wallRemoveList + rax], al
    inc eax
    dec ecx
    jnz init_wall_list

    ; Print initial maze
    mov byte [currentInteriorWall], 0
    
    ; Print top border: +-+-+-+
    mov rax, 1      ; sys_write
    mov rdi, 1      ; stdout
    mov rsi, plus_char
    mov rdx, 1
    syscall
    
    movzx ecx, byte [mazeColumns]
    mov byte [cellIndex], 0
top_border_loop:
    mov al, [cellIndex]
    cmp al, [mazeColumns]
    jge top_border_done
    
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
    
    inc byte [cellIndex]
    jmp top_border_loop
    
top_border_done:
    ; Print newline
    mov rax, 1
    mov rdi, 1
    mov rsi, newline_char
    mov rdx, 1
    syscall

    ; Print maze rows
    movzx ecx, byte [mazeRows]
    mov byte [rowIndex], 0
row_loop:
    mov al, [rowIndex]
    cmp al, [mazeRows]
    jge row_loop_done
    
    ; Print "|"
    mov rax, 1
    mov rdi, 1
    mov rsi, pipe_char
    mov rdx, 1
    syscall
    
    movzx ecx, byte [mazeColumns]
    mov byte [cellIndex], 0
cell_loop:
    mov al, [cellIndex]
    cmp al, [mazeColumns]
    jge cell_loop_done
    
    ; Print " " (space)
    mov rax, 1
    mov rdi, 1
    mov rsi, space_char
    mov rdx, 1
    syscall
    
    ; Check if should print vertical wall
    movzx eax, byte [mazeColumns]
    dec eax
    cmp byte [cellIndex], al
    je print_pipe
    
    ; Check wallsUp[currentInteriorWall]
    movzx eax, byte [currentInteriorWall]
    mov bl, [wallsUp + rax]
    cmp bl, 1
    je print_pipe
    
    ; Print space
    mov rax, 1
    mov rdi, 1
    mov rsi, space_char
    mov rdx, 1
    syscall
    jmp check_increment
    
print_pipe:
    ; Print "|"
    mov rax, 1
    mov rdi, 1
    mov rsi, pipe_char
    mov rdx, 1
    syscall
    
check_increment:
    movzx eax, byte [mazeColumns]
    dec eax
    cmp byte [cellIndex], al
    jge no_increment
    inc byte [currentInteriorWall]
    
no_increment:
    inc byte [cellIndex]
    jmp cell_loop
    
cell_loop_done:
    ; Print newline
    mov rax, 1
    mov rdi, 1
    mov rsi, newline_char
    mov rdx, 1
    syscall
    
    ; Print horizontal walls if not last row
    movzx eax, byte [mazeRows]
    dec eax
    cmp byte [rowIndex], al
    jge skip_horizontal
    
    ; Print "+"
    mov rax, 1
    mov rdi, 1
    mov rsi, plus_char
    mov rdx, 1
    syscall
    
    movzx ecx, byte [mazeColumns]
    mov byte [cellIndex], 0
horizontal_loop:
    mov al, [cellIndex]
    cmp al, [mazeColumns]
    jge horizontal_done
    
    ; Check wallsUp[currentInteriorWall]
    movzx eax, byte [currentInteriorWall]
    mov bl, [wallsUp + rax]
    cmp bl, 1
    je print_dash
    
    ; Print space
    mov rax, 1
    mov rdi, 1
    mov rsi, space_char
    mov rdx, 1
    syscall
    jmp print_plus
    
print_dash:
    ; Print "-"
    mov rax, 1
    mov rdi, 1
    mov rsi, dash_char
    mov rdx, 1
    syscall
    
print_plus:
    ; Print "+"
    mov rax, 1
    mov rdi, 1
    mov rsi, plus_char
    mov rdx, 1
    syscall
    
    inc byte [cellIndex]
    inc byte [currentInteriorWall]
    jmp horizontal_loop
    
horizontal_done:
    ; Print newline
    mov rax, 1
    mov rdi, 1
    mov rsi, newline_char
    mov rdx, 1
    syscall
    
skip_horizontal:
    inc byte [rowIndex]
    jmp row_loop
    
row_loop_done:
    ; Print bottom border: +-+-+-+
    mov rax, 1
    mov rdi, 1
    mov rsi, plus_char
    mov rdx, 1
    syscall
    
    movzx ecx, byte [mazeColumns]
    mov byte [cellIndex], 0
bottom_border_loop:
    mov al, [cellIndex]
    cmp al, [mazeColumns]
    jge bottom_border_done
    
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
    
    inc byte [cellIndex]
    jmp bottom_border_loop
    
bottom_border_done:
    ; Print newline
    mov rax, 1
    mov rdi, 1
    mov rsi, newline_char
    mov rdx, 1
    syscall

; Use getrandom system call for true randomness
mov rax, 318        ; sys_getrandom
mov rdi, random_seed ; buffer
mov rsi, 8          ; 8 bytes
mov rdx, 0          ; flags
syscall
mov rbx, [random_seed] ; Use as seed

; Fisher-Yates shuffle
movzx eax, byte [interiorWallCount]
dec eax
mov byte [shuffleIndex_var], al

fisher_yates_loop:
    cmp byte [shuffleIndex_var], 0
    jle fisher_yates_done
    
    ; Generate random number: rand()
    mov rax, [random_seed]
    mov rbx, 1103515245
    mul rbx
    add rax, 12345
    mov [random_seed], rax
    
    ; Calculate: rand() % (shuffleIndex + 1)
    movzx rbx, byte [shuffleIndex_var]
    inc rbx                    ; rbx = shuffleIndex + 1
    xor rdx, rdx              ; Clear for division
    div rbx                   ; rdx = rax % rbx
    mov [otherIndex_var], dl  ; otherIndex = rand() % (shuffleIndex + 1)
    
    ; Swap: wallRemoveList[shuffleIndex] with wallRemoveList[otherIndex]
    movzx rax, byte [shuffleIndex_var]    ; rax = shuffleIndex
    movzx rbx, byte [otherIndex_var]      ; rbx = otherIndex
    
    mov cl, [wallRemoveList + rax]        ; temp = wallRemoveList[shuffleIndex]
    mov dl, [wallRemoveList + rbx]        ; get wallRemoveList[otherIndex]
    mov [wallRemoveList + rax], dl        ; wallRemoveList[shuffleIndex] = wallRemoveList[otherIndex]
    mov [wallRemoveList + rbx], cl        ; wallRemoveList[otherIndex] = temp
    
    ; shuffleIndex--
    dec byte [shuffleIndex_var]
    jmp fisher_yates_loop

fisher_yates_done:
    ; Clear registers after shuffle
    xor rax, rax
    xor rbx, rbx
    xor rcx, rcx
    xor rdx, rdx

    ; Kruskal's algorithm main loop
    mov byte [mazeComplete], 0
    mov byte [removeWallIndex], 0
    
main_algorithm_loop:
    movzx eax, byte [removeWallIndex]
    movzx ebx, byte [interiorWallCount]
    cmp eax, ebx
    jge algorithm_done
    
    ; Get next wall to check
    movzx eax, byte [removeWallIndex]
    mov bl, [wallRemoveList + rax]
    mov [nextWallToCheck], bl
    
    ; Get cells connected by this wall
    movzx eax, byte [nextWallToCheck]
    mov cl, 2           ; 2 bytes per wall connection
    mul cl              ; al = wall * 2
    mov bl, [wallConnections + rax]     ; firstCell
    mov [firstCell], bl
    mov bl, [wallConnections + rax + 1] ; secondCell
    mov [secondCell], bl
    
    ; Get group indices
    movzx eax, byte [firstCell]
    mov bl, [cellToGroup + rax]
    mov [firstCellGroupIndex], bl
    
    movzx eax, byte [secondCell]
    mov bl, [cellToGroup + rax]
    mov [secondCellGroupIndex], bl
    
    ; Check if different groups
    mov al, [firstCellGroupIndex]
    mov bl, [secondCellGroupIndex]
    cmp al, bl
    je skip_wall
    
    ; Remove wall
    movzx eax, byte [nextWallToCheck]
    mov byte [wallsUp + rax], 0
    
    ; Find empty slot in first group
    mov byte [nextEmptyFirstGroupIndex], 0
    mov byte [cellIndex], 0
    
find_empty_slot:
    movzx eax, byte [cellIndex]
    movzx ebx, byte [totalCells]
    cmp eax, ebx
    jge found_slot
    
    ; Calculate groupCells[firstCellGroupIndex][cellIndex]
    movzx eax, byte [firstCellGroupIndex]
    movzx ebx, byte [totalCells]
    mul ebx
    add al, [cellIndex]
    mov bl, [groupCells + rax]
    cmp bl, NO_CELL
    je found_empty
    
    inc byte [cellIndex]
    jmp find_empty_slot
    
found_empty:
    mov al, [cellIndex]
    mov [nextEmptyFirstGroupIndex], al
    
found_slot:
    ; Move cells from second group to first group
    movzx eax, byte [totalCells]
    dec eax
    mov [groupCellIndex], al
    
merge_loop:
    cmp byte [groupCellIndex], 255  ; -1 as unsigned byte
    je merge_done
    
    ; Check if valid cell in second group
    movzx eax, byte [secondCellGroupIndex]
    movzx ebx, byte [totalCells]
    mul ebx
    add al, [groupCellIndex]
    mov bl, [groupCells + rax]
    cmp bl, NO_CELL
    je continue_merge
    
    ; Move cell to first group
    mov [cellToMove], bl
    
    ; Add to first group
    movzx eax, byte [firstCellGroupIndex]
    movzx ebx, byte [totalCells]
    mul ebx
    add al, [nextEmptyFirstGroupIndex]
    mov bl, [cellToMove]
    mov [groupCells + rax], bl
    
    ; Update cell's group
    movzx eax, byte [cellToMove]
    mov bl, [firstCellGroupIndex]
    mov [cellToGroup + rax], bl
    
    ; Remove from second group
    movzx eax, byte [secondCellGroupIndex]
    movzx ebx, byte [totalCells]
    mul ebx
    add al, [groupCellIndex]
    mov byte [groupCells + rax], NO_CELL
    
    inc byte [nextEmptyFirstGroupIndex]
    
    ; Check if maze complete
    movzx eax, byte [nextEmptyFirstGroupIndex]
    movzx ebx, byte [totalCells]
    cmp eax, ebx
    jl continue_merge
    mov byte [mazeComplete], 1
    
continue_merge:
    dec byte [groupCellIndex]
    jmp merge_loop
    
merge_done:
    ; Use nanosleep for exactly 500ms
    mov rax, 35         ; sys_nanosleep
    mov rdi, timespec   ; Pointer to timespec structure
    mov rsi, 0          ; No remaining time needed
    syscall

    ; Print maze again (copy of maze printing code above)
    mov byte [currentInteriorWall], 0
    
    ; Print newline first
    mov rax, 1
    mov rdi, 1
    mov rsi, newline_char
    mov rdx, 1
    syscall
    
    ; Print top border: +-+-+-+
    mov rax, 1
    mov rdi, 1
    mov rsi, plus_char
    mov rdx, 1
    syscall
    
    movzx ecx, byte [mazeColumns]
    mov byte [cellIndex], 0
top_border_loop2:
    mov al, [cellIndex]
    cmp al, [mazeColumns]
    jge top_border_done2
    
    mov rax, 1
    mov rdi, 1
    mov rsi, dash_char
    mov rdx, 1
    syscall
    
    mov rax, 1
    mov rdi, 1
    mov rsi, plus_char
    mov rdx, 1
    syscall
    
    inc byte [cellIndex]
    jmp top_border_loop2
    
top_border_done2:
    mov rax, 1
    mov rdi, 1
    mov rsi, newline_char
    mov rdx, 1
    syscall

    ; Print maze rows
    movzx ecx, byte [mazeRows]
    mov byte [rowIndex], 0
row_loop2:
    mov al, [rowIndex]
    cmp al, [mazeRows]
    jge row_loop_done2
    
    mov rax, 1
    mov rdi, 1
    mov rsi, pipe_char
    mov rdx, 1
    syscall
    
    movzx ecx, byte [mazeColumns]
    mov byte [cellIndex], 0
cell_loop2:
    mov al, [cellIndex]
    cmp al, [mazeColumns]
    jge cell_loop_done2
    
    mov rax, 1
    mov rdi, 1
    mov rsi, space_char
    mov rdx, 1
    syscall
    
    movzx eax, byte [mazeColumns]
    dec eax
    cmp byte [cellIndex], al
    je print_pipe2
    
    movzx eax, byte [currentInteriorWall]
    mov bl, [wallsUp + rax]
    cmp bl, 1
    je print_pipe2
    
    mov rax, 1
    mov rdi, 1
    mov rsi, space_char
    mov rdx, 1
    syscall
    jmp check_increment2
    
print_pipe2:
    mov rax, 1
    mov rdi, 1
    mov rsi, pipe_char
    mov rdx, 1
    syscall
    
check_increment2:
    movzx eax, byte [mazeColumns]
    dec eax
    cmp byte [cellIndex], al
    jge no_increment2
    inc byte [currentInteriorWall]
    
no_increment2:
    inc byte [cellIndex]
    jmp cell_loop2
    
cell_loop_done2:
    mov rax, 1
    mov rdi, 1
    mov rsi, newline_char
    mov rdx, 1
    syscall
    
    movzx eax, byte [mazeRows]
    dec eax
    cmp byte [rowIndex], al
    jge skip_horizontal2
    
    mov rax, 1
    mov rdi, 1
    mov rsi, plus_char
    mov rdx, 1
    syscall
    
    movzx ecx, byte [mazeColumns]
    mov byte [cellIndex], 0
horizontal_loop2:
    mov al, [cellIndex]
    cmp al, [mazeColumns]
    jge horizontal_done2
    
    movzx eax, byte [currentInteriorWall]
    mov bl, [wallsUp + rax]
    cmp bl, 1
    je print_dash2
    
    mov rax, 1
    mov rdi, 1
    mov rsi, space_char
    mov rdx, 1
    syscall
    jmp print_plus2
    
print_dash2:
    mov rax, 1
    mov rdi, 1
    mov rsi, dash_char
    mov rdx, 1
    syscall
    
print_plus2:
    mov rax, 1
    mov rdi, 1
    mov rsi, plus_char
    mov rdx, 1
    syscall
    
    inc byte [cellIndex]
    inc byte [currentInteriorWall]
    jmp horizontal_loop2
    
horizontal_done2:
    mov rax, 1
    mov rdi, 1
    mov rsi, newline_char
    mov rdx, 1
    syscall
    
skip_horizontal2:
    inc byte [rowIndex]
    jmp row_loop2
    
row_loop_done2:
    ; Print bottom border
    mov rax, 1
    mov rdi, 1
    mov rsi, plus_char
    mov rdx, 1
    syscall
    
    movzx ecx, byte [mazeColumns]
    mov byte [cellIndex], 0
bottom_border_loop2:
    mov al, [cellIndex]
    cmp al, [mazeColumns]
    jge bottom_border_done2
    
    mov rax, 1
    mov rdi, 1
    mov rsi, dash_char
    mov rdx, 1
    syscall
    
    mov rax, 1
    mov rdi, 1
    mov rsi, plus_char
    mov rdx, 1
    syscall
    
    inc byte [cellIndex]
    jmp bottom_border_loop2
    
bottom_border_done2:
    mov rax, 1
    mov rdi, 1
    mov rsi, newline_char
    mov rdx, 1
    syscall
    
    ; Check if maze complete
    cmp byte [mazeComplete], 1
    je algorithm_done
    
skip_wall:
    inc byte [removeWallIndex]
    jmp main_algorithm_loop
    
algorithm_done:
    ; Exit
    mov rax, 60
    mov rdi, 0
    syscall
