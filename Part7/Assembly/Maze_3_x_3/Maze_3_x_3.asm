section .data
    ; Constants
    FIRST_CELL equ 0
    SECOND_CELL equ 1
    NO_CELL equ -1
    
    ; 12 interior walls in a 3x3 maze. Start with all the walls up.
    wallsUp db 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
    
    ; Identify the cells each wall connects.
    wallConnections db 0,1, 1,2, 0,3, 1,4, 2,5, 3,4, 4,5, 3,6, 4,7, 5,8, 6,7, 7,8
    
    ; Identify which group each cell is a part of.
    cellToGroup db 0, 1, 2, 3, 4, 5, 6, 7, 8
    
    ; Wall removal list
    wallRemoveList db 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11
    
    ; Group cells array - 9 groups of 9 cells each
    groupCells times 81 db NO_CELL

    timespec:
        dq 0            ; seconds = 0
        dq 500000000    ; nanoseconds = 500,000,000 (500ms)

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

section .text
    global _start

_start:
    ; Initialize groupCells array
    ; Set each group's first cell: groupCells[group][0] = group
    mov al, 0
init_groups_loop:
    cmp al, 9
    jge init_groups_done
    
    ; Calculate groupCells[group][0] address
    mov bl, al
    mov cl, 9
    mul cl          ; al = group * 9
    mov [groupCells + rax], bl  ; groupCells[group * 9 + 0] = group
    
    mov al, bl      ; Restore group counter
    inc al
    jmp init_groups_loop
    
init_groups_done:

    ; Print initial maze
    mov byte [currentInteriorWall], 0
    
    ; Print top border: +-+-+-+
    mov rax, 1      ; sys_write
    mov rdi, 1      ; stdout
    mov rsi, plus_char
    mov rdx, 1
    syscall
    
    mov byte [cellIndex], 0
top_border_loop:
    cmp byte [cellIndex], 3
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
    mov byte [rowIndex], 0
row_loop:
    cmp byte [rowIndex], 3
    jge row_loop_done
    
    ; Print "|"
    mov rax, 1
    mov rdi, 1
    mov rsi, pipe_char
    mov rdx, 1
    syscall
    
    mov byte [cellIndex], 0
cell_loop:
    cmp byte [cellIndex], 3
    jge cell_loop_done
    
    ; Print " " (space)
    mov rax, 1
    mov rdi, 1
    mov rsi, space_char
    mov rdx, 1
    syscall
    
    ; Check if should print vertical wall
    cmp byte [cellIndex], 2
    je print_pipe
    
    ; Check wallsUp[currentInteriorWall]
    mov al, [currentInteriorWall]
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
    cmp byte [cellIndex], 2
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
    cmp byte [rowIndex], 2
    jge skip_horizontal
    
    ; Print "+"
    mov rax, 1
    mov rdi, 1
    mov rsi, plus_char
    mov rdx, 1
    syscall
    
    mov byte [cellIndex], 0
horizontal_loop:
    cmp byte [cellIndex], 3
    jge horizontal_done
    
    ; Check wallsUp[currentInteriorWall]
    mov al, [currentInteriorWall]
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
    
    mov byte [cellIndex], 0
bottom_border_loop:
    cmp byte [cellIndex], 3
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

; Fisher-Yates shuffle: for (int shuffleIndex = 11; shuffleIndex > 0; shuffleIndex--)
mov byte [shuffleIndex_var], 11

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
    cmp byte [removeWallIndex], 12
    jge algorithm_done
    
    ; Get next wall to check
    mov al, [removeWallIndex]
    mov bl, [wallRemoveList + rax]
    mov [nextWallToCheck], bl
    
    ; Get cells connected by this wall
    mov al, [nextWallToCheck]
    mov cl, 2           ; 2 bytes per wall connection
    mul cl              ; al = wall * 2
    mov bl, [wallConnections + rax]     ; firstCell
    mov [firstCell], bl
    mov bl, [wallConnections + rax + 1] ; secondCell
    mov [secondCell], bl
    
    ; Get group indices
    mov al, [firstCell]
    mov bl, [cellToGroup + rax]
    mov [firstCellGroupIndex], bl
    
    mov al, [secondCell]
    mov bl, [cellToGroup + rax]
    mov [secondCellGroupIndex], bl
    
    ; Check if different groups
    mov al, [firstCellGroupIndex]
    mov bl, [secondCellGroupIndex]
    cmp al, bl
    je skip_wall
    
    ; Remove wall
    mov al, [nextWallToCheck]
    mov byte [wallsUp + rax], 0
    
    ; Find empty slot in first group
    mov byte [nextEmptyFirstGroupIndex], 0

    mov byte [cellIndex], 0
    
find_empty_slot:
    cmp byte [cellIndex], 9
    jge found_slot
    
    ; Calculate groupCells[firstCellGroupIndex][cellIndex]
    mov al, [firstCellGroupIndex]
    mov cl, 9
    mul cl
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
    mov byte [groupCellIndex], 8
    
merge_loop:
    cmp byte [groupCellIndex], 255  ; -1 as unsigned byte
    je merge_done
    
    ; Check if valid cell in second group
    mov al, [secondCellGroupIndex]
    mov cl, 9
    mul cl
    add al, [groupCellIndex]
    mov bl, [groupCells + rax]
    cmp bl, NO_CELL
    je continue_merge
    
    ; Move cell to first group
    mov [cellToMove], bl
    
    ; Add to first group
    mov al, [firstCellGroupIndex]
    mov cl, 9
    mul cl
    add al, [nextEmptyFirstGroupIndex]
    mov bl, [cellToMove]
    mov [groupCells + rax], bl
    
    ; Update cell's group
    mov al, [cellToMove]
    mov bl, [firstCellGroupIndex]
    mov [cellToGroup + rax], bl
    
    ; Clear from second group
    mov al, [secondCellGroupIndex]
    mov cl, 9
    mul cl
    add al, [groupCellIndex]
    mov byte [groupCells + rax], NO_CELL
    
    inc byte [nextEmptyFirstGroupIndex]
    
    ; Check if maze complete (simplified: if we've merged 8 cells total)
    cmp byte [nextEmptyFirstGroupIndex], 9
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
    
    mov byte [cellIndex], 0
top_border_loop2:
    cmp byte [cellIndex], 3
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
    mov byte [rowIndex], 0
row_loop2:
    cmp byte [rowIndex], 3
    jge row_loop_done2
    
    mov rax, 1
    mov rdi, 1
    mov rsi, pipe_char
    mov rdx, 1
    syscall
    
    mov byte [cellIndex], 0
cell_loop2:
    cmp byte [cellIndex], 3
    jge cell_loop_done2
    
    mov rax, 1
    mov rdi, 1
    mov rsi, space_char
    mov rdx, 1
    syscall
    
    cmp byte [cellIndex], 2
    je print_pipe2
    
    mov al, [currentInteriorWall]
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
    cmp byte [cellIndex], 2
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
    
    cmp byte [rowIndex], 2
    jge skip_horizontal2
    
    mov rax, 1
    mov rdi, 1
    mov rsi, plus_char
    mov rdx, 1
    syscall
    
    mov byte [cellIndex], 0
horizontal_loop2:
    cmp byte [cellIndex], 3
    jge horizontal_done2
    
    mov al, [currentInteriorWall]
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
    
    mov byte [cellIndex], 0
bottom_border_loop2:
    cmp byte [cellIndex], 3
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

section .data
    plus_char db "+", 0
    dash_char db "-", 0
    pipe_char db "|", 0
    space_char db " ", 0
    newline_char db 10, 0
