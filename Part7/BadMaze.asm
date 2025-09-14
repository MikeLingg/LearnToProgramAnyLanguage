section .data
    ; Constants
    FIRST_CELL equ 0
    SECOND_CELL equ 1
    NO_CELL equ -1
    INTERIOR_WALL_COUNT equ 12

    ; 12 interior walls in a 3x3 maze. Start with all the walls up.
    wallsUp db 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1

    ; Identify the cells each wall connects.
    wallConnections dw 0,1, 1,2, 0,3, 1,4, 2,5, 3,4, 4,5, 3,6, 4,7, 5,8, 6,7, 7,8

    ; Identify which group each cell is a part of.
    cellToGroup dw 0, 1, 2, 3, 4, 5, 6, 7, 8

    ; Wall removal list
    wallRemoveList dw 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11

    ; String constants for printing
    plus_str db "+", 0
    dash_str db "-", 0
    pipe_str db "|", 0
    space_str db " ", 0
    newline_str db 10, 0

    ; Group cells array - 9 groups of 9 cells each
    groupCells times 81 dw NO_CELL
    
    ; Maze complete flag
    mazeComplete db 0

section .bss
    currentInteriorWall resd 1
    nextWallToCheck resd 1
    firstCell resd 1
    firstCellGroupIndex resd 1
    secondCell resd 1
    secondCellGroupIndex resd 1
    nextEmptyFirstGroupIndex resd 1
    cellToMove resd 1
    
    ; Loop variables
    cellIndex resd 1
    rowIndex resd 1
    removeWallIndex resd 1
    shuffleIndex resd 1
    otherIndex resd 1
    groupCellIndex resd 1
    temp resd 1

section .text
    global _start

_start:
    ; Initialize groupCells array
    call init_group_cells
    
    ; Print initial maze
    call print_maze
    
    ; Randomize wall removal list
    call randomize_walls
    
    ; Main algorithm loop
    call kruskal_algorithm
    
    ; Exit program
    mov rax, 60          ; sys_exit
    mov rdi, 0           ; exit status
    syscall

; Initialize group cells array
init_group_cells:
    push rbp
    mov rbp, rsp
    
    mov rcx, 0           ; group counter
init_group_loop:
    cmp rcx, 9
    jge init_group_done
    
    ; Set first cell of each group
    mov rax, rcx
    mov rbx, 9
    mul rbx              ; rax = group * 9
    mov rbx, groupCells
    add rbx, rax
    add rbx, rax         ; word addressing
    mov word [rbx], cx   ; set group[i][0] = i
    
    inc rcx
    jmp init_group_loop
    
init_group_done:
    pop rbp
    ret

; Print a string
print_string:
    push rbp
    mov rbp, rsp
    
    ; rdi contains pointer to string
    mov rsi, rdi         ; message
    mov rax, 1           ; sys_write
    mov rdi, 1           ; stdout
    mov rdx, 1           ; length
    syscall
    
    pop rbp
    ret

; Print maze
print_maze:
    push rbp
    mov rbp, rsp
    
    ; Reset currentInteriorWall
    mov dword [currentInteriorWall], 0
    
    ; Print top border
    mov rdi, plus_str
    call print_string
    
    mov dword [cellIndex], 0
top_border_loop:
    cmp dword [cellIndex], 2
    jg top_border_done
    
    mov rdi, dash_str
    call print_string
    mov rdi, plus_str
    call print_string
    
    inc dword [cellIndex]
    jmp top_border_loop
    
top_border_done:
    mov rdi, newline_str
    call print_string
    
    ; Print maze rows
    mov dword [rowIndex], 0
row_loop:
    cmp dword [rowIndex], 2
    jg row_loop_done
    
    ; Print vertical walls and cells
    mov rdi, pipe_str
    call print_string
    
    mov dword [cellIndex], 0
cell_loop:
    cmp dword [cellIndex], 2
    jg cell_loop_done
    
    mov rdi, space_str
    call print_string
    
    ; Check if we should print vertical wall
    cmp dword [cellIndex], 2
    je print_wall
    
    ; Check wallsUp[currentInteriorWall]
    mov eax, [currentInteriorWall]
    mov bl, byte [wallsUp + rax]
    cmp bl, 1
    je print_wall
    
    mov rdi, space_str
    call print_string
    jmp check_increment
    
print_wall:
    mov rdi, pipe_str
    call print_string
    
check_increment:
    cmp dword [cellIndex], 2
    jge no_increment
    cmp dword [currentInteriorWall], 11
    jge no_increment
    inc dword [currentInteriorWall]
    
no_increment:
    inc dword [cellIndex]
    jmp cell_loop
    
cell_loop_done:
    mov rdi, newline_str
    call print_string
    
    ; Print horizontal walls if not last row
    cmp dword [rowIndex], 2
    jge skip_horizontal
    
    mov rdi, plus_str
    call print_string
    
    mov dword [cellIndex], 0
horizontal_loop:
    cmp dword [cellIndex], 2
    jg horizontal_done
    
    ; Check wallsUp[currentInteriorWall]
    mov eax, [currentInteriorWall]
    mov bl, byte [wallsUp + rax]
    cmp bl, 1
    je print_dash
    
    mov rdi, space_str
    call print_string
    jmp print_plus
    
print_dash:
    mov rdi, dash_str
    call print_string
    
print_plus:
    mov rdi, plus_str
    call print_string
    
    inc dword [cellIndex]
    jmp horizontal_loop
    
horizontal_done:
    mov rdi, newline_str
    call print_string
    
skip_horizontal:
    inc dword [currentInteriorWall]
    inc dword [rowIndex]
    jmp row_loop
    
row_loop_done:
    ; Print bottom border
    mov rdi, plus_str
    call print_string
    
    mov dword [cellIndex], 0
bottom_border_loop:
    cmp dword [cellIndex], 2
    jg bottom_border_done
    
    mov rdi, dash_str
    call print_string
    mov rdi, plus_str
    call print_string
    
    inc dword [cellIndex]
    jmp bottom_border_loop
    
bottom_border_done:
    mov rdi, newline_str
    call print_string
    
    pop rbp
    ret

; Simple random number generator using linear congruential generator
; Input: none
; Output: eax = random number
get_random:
    push rbp
    mov rbp, rsp
    
    ; Get current time as seed
    mov rax, 201         ; sys_time
    mov rdi, 0
    syscall
    
    ; Simple LCG: next = (seed * 1103515245 + 12345) % 2^31
    mov rbx, 1103515245
    mul rbx
    add rax, 12345
    and rax, 0x7FFFFFFF
    
    pop rbp
    ret

; Randomize wall removal list using Fisher-Yates shuffle
randomize_walls:
    push rbp
    mov rbp, rsp
    
    mov dword [shuffleIndex], 11
shuffle_loop:
    cmp dword [shuffleIndex], 0
    jle shuffle_done
    
    ; Get random index from 0 to shuffleIndex
    call get_random
    mov ebx, [shuffleIndex]
    inc ebx
    xor edx, edx
    div ebx              ; edx = random % (shuffleIndex + 1)
    mov [otherIndex], edx
    
    ; Swap wallRemoveList[shuffleIndex] with wallRemoveList[otherIndex]
    mov eax, [shuffleIndex]
    mov ebx, [otherIndex]
    mov cx, word [wallRemoveList + rax*2]
    mov dx, word [wallRemoveList + rbx*2]
    mov word [wallRemoveList + rax*2], dx
    mov word [wallRemoveList + rbx*2], cx
    
    dec dword [shuffleIndex]
    jmp shuffle_loop
    
shuffle_done:
    pop rbp
    ret

; Sleep function (approximate 500ms using busy wait)
sleep_500ms:
    push rbp
    mov rbp, rsp
    
    mov rcx, 50000000    ; Approximate loop count for 500ms
sleep_loop:
    dec rcx
    jnz sleep_loop
    
    pop rbp
    ret

; Main Kruskal's algorithm
kruskal_algorithm:
    push rbp
    mov rbp, rsp
    
    mov dword [removeWallIndex], 0
main_loop:
    cmp dword [removeWallIndex], INTERIOR_WALL_COUNT
    jge main_loop_done
    
    ; Get next wall to check
    mov eax, [removeWallIndex]
    mov bx, word [wallRemoveList + rax*2]
    mov [nextWallToCheck], ebx
    
    ; Get cells connected by this wall
    mov eax, [nextWallToCheck]
    mov ebx, eax
    shl ebx, 2           ; multiply by 4 (2 words per connection)
    mov cx, word [wallConnections + rbx]
    mov [firstCell], ecx
    mov cx, word [wallConnections + rbx + 2]
    mov [secondCell], ecx
    
    ; Get group indices
    mov eax, [firstCell]
    mov bx, word [cellToGroup + rax*2]
    mov [firstCellGroupIndex], ebx
    mov eax, [secondCell]
    mov bx, word [cellToGroup + rax*2]
    mov [secondCellGroupIndex], ebx
    
    ; Check if cells are in different groups
    mov eax, [firstCellGroupIndex]
    mov ebx, [secondCellGroupIndex]
    cmp eax, ebx
    je skip_wall
    
    ; Remove wall
    mov eax, [nextWallToCheck]
    mov byte [wallsUp + rax], 0
    
    ; Find empty slot in first group
    call find_empty_slot
    
    ; Move cells from second group to first group
    call merge_groups
    
    ; Check if maze is complete
    cmp dword [nextEmptyFirstGroupIndex], 9
    jge maze_complete_flag
    
    ; Sleep and print maze
    call sleep_500ms
    call print_maze
    
maze_complete_flag:
    cmp dword [nextEmptyFirstGroupIndex], 9
    jl skip_wall
    mov byte [mazeComplete], 1
    jmp main_loop_done
    
skip_wall:
    inc dword [removeWallIndex]
    jmp main_loop
    
main_loop_done:
    pop rbp
    ret

; Find empty slot in first group
find_empty_slot:
    push rbp
    mov rbp, rsp
    
    mov dword [nextEmptyFirstGroupIndex], 0
    mov dword [cellIndex], 0
    
find_slot_loop:
    cmp dword [cellIndex], 8
    jg find_slot_done
    
    ; Calculate address: groupCells[firstCellGroupIndex][cellIndex]
    mov eax, [firstCellGroupIndex]
    mov ebx, 9
    mul ebx
    add eax, [cellIndex]
    mov bx, word [groupCells + rax*2]
    cmp bx, NO_CELL
    jne continue_find
    
    mov eax, [cellIndex]
    mov [nextEmptyFirstGroupIndex], eax
    jmp find_slot_done
    
continue_find:
    inc dword [cellIndex]
    jmp find_slot_loop
    
find_slot_done:
    pop rbp
    ret

; Merge groups
merge_groups:
    push rbp
    mov rbp, rsp
    
    mov dword [groupCellIndex], 8
merge_loop:
    cmp dword [groupCellIndex], -1
    jle merge_done
    
    ; Check if cell exists in second group
    mov eax, [secondCellGroupIndex]
    mov ebx, 9
    mul ebx
    add eax, [groupCellIndex]
    mov bx, word [groupCells + rax*2]
    cmp bx, NO_CELL
    je continue_merge
    
    ; Move cell to first group
    mov [cellToMove], ebx
    
    ; Set in first group
    mov eax, [firstCellGroupIndex]
    mov ebx, 9
    mul ebx
    add eax, [nextEmptyFirstGroupIndex]
    mov ebx, [cellToMove]
    mov word [groupCells + rax*2], bx
    
    ; Update cell's group
    mov eax, [cellToMove]
    mov ebx, [firstCellGroupIndex]
    mov word [cellToGroup + rax*2], bx
    
    ; Clear from second group
    mov eax, [secondCellGroupIndex]
    mov ebx, 9
    mul ebx
    add eax, [groupCellIndex]
    mov word [groupCells + rax*2], NO_CELL
    
    inc dword [nextEmptyFirstGroupIndex]
    
continue_merge:
    dec dword [groupCellIndex]
    jmp merge_loop
    
merge_done:
    pop rbp
    ret
