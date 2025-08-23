section .data
    ; String constants
    initialMsg db "Initial Maze:", 10, 0
    initialMsgLen equ $ - initialMsg - 1
    removingMsg db 10, "Removing wall from cell 0:", 10, 0
    removingMsgLen equ $ - removingMsg - 1
    removingMsg1 db 10, "Removing wall from cell 1:", 10, 0
    removingMsg1Len equ $ - removingMsg1 - 1
    removingMsg2 db 10, "Removing wall from cell 2:", 10, 0
    removingMsg2Len equ $ - removingMsg2 - 1
    removingMsg3 db 10, "Removing wall from cell 3:", 10, 0
    removingMsg3Len equ $ - removingMsg3 - 1
    removingMsg4 db 10, "Removing wall from cell 4:", 10, 0
    removingMsg4Len equ $ - removingMsg4 - 1
    removingMsg5 db 10, "Removing wall from cell 5:", 10, 0
    removingMsg5Len equ $ - removingMsg5 - 1
    removingMsg6 db 10, "Removing wall from cell 6:", 10, 0
    removingMsg6Len equ $ - removingMsg6 - 1
    removingMsg7 db 10, "Removing wall from cell 7:", 10, 0
    removingMsg7Len equ $ - removingMsg7 - 1
    removingMsg8 db 10, "Removing wall from cell 8:", 10, 0
    removingMsg8Len equ $ - removingMsg8 - 1
    
    ; Cell to wall lookup table (9 cells x 4 directions)
    ; For each cell 0-8, walls in directions LEFT(0), UP(1), RIGHT(2), DOWN(3)
    ; -1 means exterior wall, >= 0 means interior wall index
    cellToWallLUT:
        dd -1, -1,  0,  2   ; cell 0
        dd  0, -1,  1,  3   ; cell 1  
        dd  1, -1, -1,  4   ; cell 2
        dd -1,  2,  5,  7   ; cell 3
        dd  5,  3,  6,  8   ; cell 4
        dd  6,  4, -1,  9   ; cell 5
        dd -1,  7, 10, -1   ; cell 6
        dd 10,  8, 11, -1   ; cell 7
        dd 11,  9, -1, -1   ; cell 8
    
    ; Wall list - 12 interior walls, all initially true (1)
    wallList db 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
    
    ; Direction constants
    LEFT equ 0
    UP equ 1
    RIGHT equ 2
    DOWN equ 3

    ; timespec structure for nanosleep - defined in data section
    timespecSec dq 0                    ; 0 seconds
    timespecNsec dq 500000000          ; 500,000,000 nanoseconds = 500ms

section .bss
    currentInteriorWall resd 1
    cellIndex resd 1
    wallToRemove resd 1
    wallRemoved resd 1
    direction resd 1

section .text
    global _start

_start:
    ; Print initial message
    mov rax, 1         ; sys_write
    mov rdi, 1         ; stdout
    mov rsi, initialMsg
    mov rdx, initialMsgLen
    syscall
    
    ; Print initial maze
    mov dword [currentInteriorWall], 0
    
    ; Print top border: +-+-+-+
    mov rax, 1
    mov rdi, 1
    mov rsi, '+'
    mov rdx, 1
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    syscall
    pop rsi
    
    mov rsi, '-'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '-'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '-'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, 10   ; newline
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Row 1: |_|_|_| (with possible openings)
    mov rsi, '|'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, ' '
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 0
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1
    jne row1Open1
    mov rsi, '|'
    jmp row1Print1
row1Open1:
    mov rsi, ' '
row1Print1:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, ' '
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 1  
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1
    jne row1Open2
    mov rsi, '|'
    jmp row1Print2
row1Open2:
    mov rsi, ' '
row1Print2:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, ' '
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '|'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, 10   ; newline
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Horizontal walls above row 2: +-+-+-+
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 2
    mov ebx, [currentInteriorWall] 
    cmp byte [wallList + ebx], 1
    jne h2Open1
    mov rsi, '-'
    jmp h2Print1
h2Open1:
    mov rsi, ' '
h2Print1:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 3
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1  
    jne h2Open2
    mov rsi, '-'
    jmp h2Print2
h2Open2:
    mov rsi, ' '
h2Print2:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 4
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1
    jne h2Open3  
    mov rsi, '-'
    jmp h2Print3
h2Open3:
    mov rsi, ' '
h2Print3:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, 10   ; newline
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Row 2: |_|_|_| (with possible openings)
    mov rsi, '|'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, ' '
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 5
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1
    jne row2Open1
    mov rsi, '|'
    jmp row2Print1
row2Open1:
    mov rsi, ' '
row2Print1:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, ' '
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 6
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1
    jne row2Open2
    mov rsi, '|'
    jmp row2Print2  
row2Open2:
    mov rsi, ' '
row2Print2:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, ' '
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '|'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, 10   ; newline
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Horizontal walls above row 3: +-+-+-+
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 7
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1
    jne h3Open1
    mov rsi, '-'
    jmp h3Print1
h3Open1:
    mov rsi, ' '
h3Print1:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 8
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1
    jne h3Open2
    mov rsi, '-'
    jmp h3Print2
h3Open2:
    mov rsi, ' '
h3Print2:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 9
    mov ebx, [currentInteriorWall] 
    cmp byte [wallList + ebx], 1
    jne h3Open3
    mov rsi, '-'
    jmp h3Print3
h3Open3:
    mov rsi, ' '
h3Print3:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, 10   ; newline
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Row 3: |_|_|_| (with possible openings)
    mov rsi, '|'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, ' '
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 10
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1
    jne row3Open1
    mov rsi, '|'
    jmp row3Print1
row3Open1:
    mov rsi, ' '
row3Print1:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, ' '
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 11
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1
    jne row3Open2
    mov rsi, '|'
    jmp row3Print2
row3Open2:
    mov rsi, ' '
row3Print2:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, ' '
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '|'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, 10   ; newline
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Bottom border: +-+-+-+
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '-'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '-'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '-'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, 10   ; newline
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi

    ; Now process cell 0 - remove wall

    ; Sleep for 500ms using nanosleep system call
    mov rax, 35              ; sys_nanosleep
    mov rdi, timespecSec    ; pointer to timespec structure
    mov rsi, 0               ; NULL (don't care about remaining time)
    syscall

    ; Print removing message
    mov rax, 1         ; sys_write
    mov rdi, 1         ; stdout
    mov rsi, removingMsg
    mov rdx, removingMsgLen
    syscall
    
    ; Remove wall code for cell 0:
    mov dword [cellIndex], 0
    mov dword [wallRemoved], 0
    mov dword [direction], LEFT
    
    ; Try each direction until we find a removable wall
tryDirection0:
    ; Check if we've tried all directions
    cmp dword [direction], 4
    jge done0
    
    ; Calculate lookup table index: cellIndex * 4 + direction
    mov eax, [cellIndex]
    mov ebx, 4
    mul ebx
    add eax, [direction]
    
    ; Get wall index from lookup table
    mov ebx, eax
    mov eax, [cellToWallLUT + ebx * 4]
    
    ; Check if it's an exterior wall (-1)
    cmp eax, -1
    je nextDirection0
    
    ; Store wall to remove
    mov [wallToRemove], eax
    
    ; Check if wall is still up
    mov ebx, eax
    cmp byte [wallList + ebx], 1
    jne nextDirection0
    
    ; Remove the wall
    mov byte [wallList + ebx], 0
    mov dword [wallRemoved], 1
    jmp done0
    
nextDirection0:
    inc dword [direction]
    jmp tryDirection0
    
done0:
    
; Print maze after removing wall from cell 0
    mov dword [currentInteriorWall], 0
    
    ; Print top border: +-+-+-+
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '-'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '-'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '-'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, 10   ; newline
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Row 1: |_|_|_| (with possible openings)
    mov rsi, '|'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, ' '
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 0
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1
    jne row1Open1Cell0
    mov rsi, '|'
    jmp row1Print1Cell0
row1Open1Cell0:
    mov rsi, ' '
row1Print1Cell0:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, ' '
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 1  
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1
    jne row1Open2Cell0
    mov rsi, '|'
    jmp row1Print2Cell0
row1Open2Cell0:
    mov rsi, ' '
row1Print2Cell0:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, ' '
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '|'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, 10   ; newline
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Horizontal walls above row 2: +-+-+-+
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 2
    mov ebx, [currentInteriorWall] 
    cmp byte [wallList + ebx], 1
    jne h2Open1Cell0
    mov rsi, '-'
    jmp h2Print1Cell0
h2Open1Cell0:
    mov rsi, ' '
h2Print1Cell0:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 3
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1  
    jne h2Open2Cell0
    mov rsi, '-'
    jmp h2Print2Cell0
h2Open2Cell0:
    mov rsi, ' '
h2Print2Cell0:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 4
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1
    jne h2Open3Cell0  
    mov rsi, '-'
    jmp h2Print3Cell0
h2Open3Cell0:
    mov rsi, ' '
h2Print3Cell0:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, 10   ; newline
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Row 2: |_|_|_| (with possible openings)
    mov rsi, '|'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, ' '
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 5
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1
    jne row2Open1Cell0
    mov rsi, '|'
    jmp row2Print1Cell0
row2Open1Cell0:
    mov rsi, ' '
row2Print1Cell0:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, ' '
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 6
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1
    jne row2Open2Cell0
    mov rsi, '|'
    jmp row2Print2Cell0  
row2Open2Cell0:
    mov rsi, ' '
row2Print2Cell0:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, ' '
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '|'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, 10   ; newline
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Horizontal walls above row 3: +-+-+-+
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 7
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1
    jne h3Open1Cell0
    mov rsi, '-'
    jmp h3Print1Cell0
h3Open1Cell0:
    mov rsi, ' '
h3Print1Cell0:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 8
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1
    jne h3Open2Cell0
    mov rsi, '-'
    jmp h3Print2Cell0
h3Open2Cell0:
    mov rsi, ' '
h3Print2Cell0:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 9
    mov ebx, [currentInteriorWall] 
    cmp byte [wallList + ebx], 1
    jne h3Open3Cell0
    mov rsi, '-'
    jmp h3Print3Cell0
h3Open3Cell0:
    mov rsi, ' '
h3Print3Cell0:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, 10   ; newline
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Row 3: |_|_|_| (with possible openings)
    mov rsi, '|'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, ' '
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 10
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1
    jne row3Open1Cell0
    mov rsi, '|'
    jmp row3Print1Cell0
row3Open1Cell0:
    mov rsi, ' '
row3Print1Cell0:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, ' '
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 11
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1
    jne row3Open2Cell0
    mov rsi, '|'
    jmp row3Print2Cell0
row3Open2Cell0:
    mov rsi, ' '
row3Print2Cell0:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, ' '
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '|'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, 10   ; newline
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Bottom border: +-+-+-+
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '-'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '-'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '-'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, 10   ; newline
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi

; Now process cell 1 - remove wall

    ; Sleep for 500ms using nanosleep system call
    mov rax, 35              ; sys_nanosleep
    mov rdi, timespecSec    ; pointer to timespec structure
    mov rsi, 0               ; NULL (don't care about remaining time)
    syscall

    ; Print removing message
    mov rax, 1         ; sys_write
    mov rdi, 1         ; stdout
    mov rsi, removingMsg1
    mov rdx, removingMsg1Len
    syscall
    
    ; Remove wall code for cell 1:
    mov dword [cellIndex], 1
    mov dword [wallRemoved], 0
    mov dword [direction], LEFT
    
    ; Try each direction until we find a removable wall
tryDirection1:
    ; Check if we've tried all directions
    cmp dword [direction], 4
    jge done1
    
    ; Calculate lookup table index: cellIndex * 4 + direction
    mov eax, [cellIndex]
    mov ebx, 4
    mul ebx
    add eax, [direction]
    
    ; Get wall index from lookup table
    mov ebx, eax
    mov eax, [cellToWallLUT + ebx * 4]
    
    ; Check if it's an exterior wall (-1)
    cmp eax, -1
    je nextDirection1
    
    ; Store wall to remove
    mov [wallToRemove], eax
    
    ; Check if wall is still up
    mov ebx, eax
    cmp byte [wallList + ebx], 1
    jne nextDirection1
    
    ; Remove the wall
    mov byte [wallList + ebx], 0
    mov dword [wallRemoved], 1
    jmp done1
    
nextDirection1:
    inc dword [direction]
    jmp tryDirection1
    
done1:

; Print maze after removing wall from cell 0
    mov dword [currentInteriorWall], 0
    
    ; Print top border: +-+-+-+
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '-'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '-'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '-'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, 10   ; newline
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Row 1: |_|_|_| (with possible openings)
    mov rsi, '|'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, ' '
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 0
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1
    jne row1Open1Cell1
    mov rsi, '|'
    jmp row1Print1Cell1
row1Open1Cell1:
    mov rsi, ' '
row1Print1Cell1:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, ' '
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 1  
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1
    jne row1Open2Cell1
    mov rsi, '|'
    jmp row1Print2Cell1
row1Open2Cell1:
    mov rsi, ' '
row1Print2Cell1:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, ' '
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '|'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, 10   ; newline
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Horizontal walls above row 2: +-+-+-+
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 2
    mov ebx, [currentInteriorWall] 
    cmp byte [wallList + ebx], 1
    jne h2Open1Cell1
    mov rsi, '-'
    jmp h2Print1Cell1
h2Open1Cell1:
    mov rsi, ' '
h2Print1Cell1:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 3
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1  
    jne h2Open2Cell1
    mov rsi, '-'
    jmp h2Print2Cell1
h2Open2Cell1:
    mov rsi, ' '
h2Print2Cell1:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 4
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1
    jne h2Open3Cell1  
    mov rsi, '-'
    jmp h2Print3Cell1
h2Open3Cell1:
    mov rsi, ' '
h2Print3Cell1:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, 10   ; newline
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Row 2: |_|_|_| (with possible openings)
    mov rsi, '|'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, ' '
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 5
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1
    jne row2Open1Cell1
    mov rsi, '|'
    jmp row2Print1Cell1
row2Open1Cell1:
    mov rsi, ' '
row2Print1Cell1:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, ' '
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 6
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1
    jne row2Open2Cell1
    mov rsi, '|'
    jmp row2Print2Cell1  
row2Open2Cell1:
    mov rsi, ' '
row2Print2Cell1:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, ' '
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '|'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, 10   ; newline
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Horizontal walls above row 3: +-+-+-+
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 7
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1
    jne h3Open1Cell1
    mov rsi, '-'
    jmp h3Print1Cell1
h3Open1Cell1:
    mov rsi, ' '
h3Print1Cell1:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 8
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1
    jne h3Open2Cell1
    mov rsi, '-'
    jmp h3Print2Cell1
h3Open2Cell1:
    mov rsi, ' '
h3Print2Cell1:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 9
    mov ebx, [currentInteriorWall] 
    cmp byte [wallList + ebx], 1
    jne h3Open3Cell1
    mov rsi, '-'
    jmp h3Print3Cell1
h3Open3Cell1:
    mov rsi, ' '
h3Print3Cell1:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, 10   ; newline
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Row 3: |_|_|_| (with possible openings)
    mov rsi, '|'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, ' '
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 10
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1
    jne row3Open1Cell1
    mov rsi, '|'
    jmp row3Print1Cell1
row3Open1Cell1:
    mov rsi, ' '
row3Print1Cell1:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, ' '
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 11
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1
    jne row3Open2Cell1
    mov rsi, '|'
    jmp row3Print2Cell1
row3Open2Cell1:
    mov rsi, ' '
row3Print2Cell1:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, ' '
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '|'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, 10   ; newline
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Bottom border: +-+-+-+
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '-'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '-'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '-'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, 10   ; newline
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi

; Now process cell 2 - remove wall

    ; Sleep for 500ms using nanosleep system call
    mov rax, 35              ; sys_nanosleep
    mov rdi, timespecSec    ; pointer to timespec structure
    mov rsi, 0               ; NULL (don't care about remaining time)
    syscall

    ; Print removing message
    mov rax, 1         ; sys_write
    mov rdi, 1         ; stdout
    mov rsi, removingMsg2
    mov rdx, removingMsg2Len
    syscall
    
    ; Remove wall code for cell 2:
    mov dword [cellIndex], 2
    mov dword [wallRemoved], 0
    mov dword [direction], LEFT
    
    ; Try each direction until we find a removable wall
tryDirectionCell2:
    ; Check if we've tried all directions
    cmp dword [direction], 4
    jge doneCell2
    
    ; Calculate lookup table index: cellIndex * 4 + direction
    mov eax, [cellIndex]
    mov ebx, 4
    mul ebx
    add eax, [direction]
    
    ; Get wall index from lookup table
    mov ebx, eax
    mov eax, [cellToWallLUT + ebx * 4]
    
    ; Check if it's an exterior wall (-1)
    cmp eax, -1
    je nextDirectionCell2
    
    ; Store wall to remove
    mov [wallToRemove], eax
    
    ; Check if wall is still up
    mov ebx, eax
    cmp byte [wallList + ebx], 1
    jne nextDirectionCell2
    
    ; Remove the wall
    mov byte [wallList + ebx], 0
    mov dword [wallRemoved], 1
    jmp doneCell2
    
nextDirectionCell2:
    inc dword [direction]
    jmp tryDirectionCell2
    
doneCell2:

; Print maze after removing wall from cell 2
    mov dword [currentInteriorWall], 0
    
    ; Print top border: +-+-+-+
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '-'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '-'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '-'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, 10   ; newline
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Row 1: |_|_|_| (with possible openings)
    mov rsi, '|'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, ' '
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 0
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1
    jne row1Open1Cell2
    mov rsi, '|'
    jmp row1Print1Cell2
row1Open1Cell2:
    mov rsi, ' '
row1Print1Cell2:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, ' '
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 1  
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1
    jne row1Open2Cell2
    mov rsi, '|'
    jmp row1Print2Cell2
row1Open2Cell2:
    mov rsi, ' '
row1Print2Cell2:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, ' '
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '|'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, 10   ; newline
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Horizontal walls above row 2: +-+-+-+
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 2
    mov ebx, [currentInteriorWall] 
    cmp byte [wallList + ebx], 1
    jne h2Open1Cell2
    mov rsi, '-'
    jmp h2Print1Cell2
h2Open1Cell2:
    mov rsi, ' '
h2Print1Cell2:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 3
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1  
    jne h2Open2Cell2
    mov rsi, '-'
    jmp h2Print2Cell2
h2Open2Cell2:
    mov rsi, ' '
h2Print2Cell2:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 4
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1
    jne h2Open3Cell2  
    mov rsi, '-'
    jmp h2Print3Cell2
h2Open3Cell2:
    mov rsi, ' '
h2Print3Cell2:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, 10   ; newline
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Row 2: |_|_|_| (with possible openings)
    mov rsi, '|'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, ' '
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 5
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1
    jne row2Open1Cell2
    mov rsi, '|'
    jmp row2Print1Cell2
row2Open1Cell2:
    mov rsi, ' '
row2Print1Cell2:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, ' '
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 6
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1
    jne row2Open2Cell2
    mov rsi, '|'
    jmp row2Print2Cell2  
row2Open2Cell2:
    mov rsi, ' '
row2Print2Cell2:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, ' '
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '|'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, 10   ; newline
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Horizontal walls above row 3: +-+-+-+
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 7
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1
    jne h3Open1Cell2
    mov rsi, '-'
    jmp h3Print1Cell2
h3Open1Cell2:
    mov rsi, ' '
h3Print1Cell2:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 8
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1
    jne h3Open2Cell2
    mov rsi, '-'
    jmp h3Print2Cell2
h3Open2Cell2:
    mov rsi, ' '
h3Print2Cell2:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 9
    mov ebx, [currentInteriorWall] 
    cmp byte [wallList + ebx], 1
    jne h3Open3Cell2
    mov rsi, '-'
    jmp h3Print3Cell2
h3Open3Cell2:
    mov rsi, ' '
h3Print3Cell2:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, 10   ; newline
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Row 3: |_|_|_| (with possible openings)
    mov rsi, '|'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, ' '
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 10
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1
    jne row3Open1Cell2
    mov rsi, '|'
    jmp row3Print1Cell2
row3Open1Cell2:
    mov rsi, ' '
row3Print1Cell2:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, ' '
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 11
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1
    jne row3Open2Cell2
    mov rsi, '|'
    jmp row3Print2Cell2
row3Open2Cell2:
    mov rsi, ' '
row3Print2Cell2:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, ' '
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '|'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, 10   ; newline
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Bottom border: +-+-+-+
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '-'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '-'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '-'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, 10   ; newline
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi

; Now process cell 3 - remove wall

    ; Sleep for 500ms using nanosleep system call
    mov rax, 35              ; sys_nanosleep
    mov rdi, timespecSec    ; pointer to timespec structure
    mov rsi, 0               ; NULL (don't care about remaining time)
    syscall

    ; Print removing message
    mov rax, 1         ; sys_write
    mov rdi, 1         ; stdout
    mov rsi, removingMsg3
    mov rdx, removingMsg3Len
    syscall
    
    ; Remove wall code for cell 3:
    mov dword [cellIndex], 3
    mov dword [wallRemoved], 0
    mov dword [direction], LEFT
    
    ; Try each direction until we find a removable wall
tryDirectionCell3:
    ; Check if we've tried all directions
    cmp dword [direction], 4
    jge doneCell3
    
    ; Calculate lookup table index: cellIndex * 4 + direction
    mov eax, [cellIndex]
    mov ebx, 4
    mul ebx
    add eax, [direction]
    
    ; Get wall index from lookup table
    mov ebx, eax
    mov eax, [cellToWallLUT + ebx * 4]
    
    ; Check if it's an exterior wall (-1)
    cmp eax, -1
    je nextDirectionCell3
    
    ; Store wall to remove
    mov [wallToRemove], eax
    
    ; Check if wall is still up
    mov ebx, eax
    cmp byte [wallList + ebx], 1
    jne nextDirectionCell3
    
    ; Remove the wall
    mov byte [wallList + ebx], 0
    mov dword [wallRemoved], 1
    jmp doneCell3
    
nextDirectionCell3:
    inc dword [direction]
    jmp tryDirectionCell3
    
doneCell3:

; Print maze after removing wall from cell 3
    mov dword [currentInteriorWall], 0
    
    ; Print top border: +-+-+-+
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '-'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '-'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '-'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, 10   ; newline
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Row 1: |_|_|_| (with possible openings)
    mov rsi, '|'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, ' '
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 0
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1
    jne row1Open1Cell3
    mov rsi, '|'
    jmp row1Print1Cell3
row1Open1Cell3:
    mov rsi, ' '
row1Print1Cell3:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, ' '
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 1  
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1
    jne row1Open2Cell3
    mov rsi, '|'
    jmp row1Print2Cell3
row1Open2Cell3:
    mov rsi, ' '
row1Print2Cell3:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, ' '
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '|'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, 10   ; newline
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Horizontal walls above row 2: +-+-+-+
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 2
    mov ebx, [currentInteriorWall] 
    cmp byte [wallList + ebx], 1
    jne h2Open1Cell3
    mov rsi, '-'
    jmp h2Print1Cell3
h2Open1Cell3:
    mov rsi, ' '
h2Print1Cell3:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 3
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1  
    jne h2Open2Cell3
    mov rsi, '-'
    jmp h2Print2Cell3
h2Open2Cell3:
    mov rsi, ' '
h2Print2Cell3:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 4
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1
    jne h2Open3Cell3  
    mov rsi, '-'
    jmp h2Print3Cell3
h2Open3Cell3:
    mov rsi, ' '
h2Print3Cell3:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, 10   ; newline
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Row 2: |_|_|_| (with possible openings)
    mov rsi, '|'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, ' '
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 5
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1
    jne row2Open1Cell3
    mov rsi, '|'
    jmp row2Print1Cell3
row2Open1Cell3:
    mov rsi, ' '
row2Print1Cell3:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, ' '
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 6
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1
    jne row2Open2Cell3
    mov rsi, '|'
    jmp row2Print2Cell3  
row2Open2Cell3:
    mov rsi, ' '
row2Print2Cell3:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, ' '
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '|'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, 10   ; newline
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Horizontal walls above row 3: +-+-+-+
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 7
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1
    jne h3Open1Cell3
    mov rsi, '-'
    jmp h3Print1Cell3
h3Open1Cell3:
    mov rsi, ' '
h3Print1Cell3:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 8
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1
    jne h3Open2Cell3
    mov rsi, '-'
    jmp h3Print2Cell3
h3Open2Cell3:
    mov rsi, ' '
h3Print2Cell3:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 9
    mov ebx, [currentInteriorWall] 
    cmp byte [wallList + ebx], 1
    jne h3Open3Cell3
    mov rsi, '-'
    jmp h3Print3Cell3
h3Open3Cell3:
    mov rsi, ' '
h3Print3Cell3:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, 10   ; newline
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Row 3: |_|_|_| (with possible openings)
    mov rsi, '|'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, ' '
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 10
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1
    jne row3Open1Cell3
    mov rsi, '|'
    jmp row3Print1Cell3
row3Open1Cell3:
    mov rsi, ' '
row3Print1Cell3:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, ' '
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 11
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1
    jne row3Open2Cell3
    mov rsi, '|'
    jmp row3Print2Cell3
row3Open2Cell3:
    mov rsi, ' '
row3Print2Cell3:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, ' '
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '|'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, 10   ; newline
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Bottom border: +-+-+-+
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '-'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '-'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '-'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, 10   ; newline
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi

; Now process cell 4 - remove wall

    ; Sleep for 500ms using nanosleep system call
    mov rax, 35              ; sys_nanosleep
    mov rdi, timespecSec    ; pointer to timespec structure
    mov rsi, 0               ; NULL (don't care about remaining time)
    syscall

    ; Print removing message
    mov rax, 1         ; sys_write
    mov rdi, 1         ; stdout
    mov rsi, removingMsg4
    mov rdx, removingMsg4Len
    syscall
    
    ; Remove wall code for cell 4:
    mov dword [cellIndex], 4
    mov dword [wallRemoved], 0
    mov dword [direction], LEFT
    
    ; Try each direction until we find a removable wall
tryDirectionCell4:
    ; Check if we've tried all directions
    cmp dword [direction], 4
    jge doneCell4
    
    ; Calculate lookup table index: cellIndex * 4 + direction
    mov eax, [cellIndex]
    mov ebx, 4
    mul ebx
    add eax, [direction]
    
    ; Get wall index from lookup table
    mov ebx, eax
    mov eax, [cellToWallLUT + ebx * 4]
    
    ; Check if it's an exterior wall (-1)
    cmp eax, -1
    je nextDirectionCell4
    
    ; Store wall to remove
    mov [wallToRemove], eax
    
    ; Check if wall is still up
    mov ebx, eax
    cmp byte [wallList + ebx], 1
    jne nextDirectionCell4
    
    ; Remove the wall
    mov byte [wallList + ebx], 0
    mov dword [wallRemoved], 1
    jmp doneCell4
    
nextDirectionCell4:
    inc dword [direction]
    jmp tryDirectionCell4
    
doneCell4:

; Print maze after removing wall from cell 4
    mov dword [currentInteriorWall], 0
    
    ; Print top border: +-+-+-+
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '-'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '-'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '-'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, 10   ; newline
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Row 1: |_|_|_| (with possible openings)
    mov rsi, '|'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, ' '
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 0
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1
    jne row1Open1Cell4
    mov rsi, '|'
    jmp row1Print1Cell4
row1Open1Cell4:
    mov rsi, ' '
row1Print1Cell4:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, ' '
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 1  
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1
    jne row1Open2Cell4
    mov rsi, '|'
    jmp row1Print2Cell4
row1Open2Cell4:
    mov rsi, ' '
row1Print2Cell4:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, ' '
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '|'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, 10   ; newline
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Horizontal walls above row 2: +-+-+-+
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 2
    mov ebx, [currentInteriorWall] 
    cmp byte [wallList + ebx], 1
    jne h2Open1Cell4
    mov rsi, '-'
    jmp h2Print1Cell4
h2Open1Cell4:
    mov rsi, ' '
h2Print1Cell4:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 3
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1  
    jne h2Open2Cell4
    mov rsi, '-'
    jmp h2Print2Cell4
h2Open2Cell4:
    mov rsi, ' '
h2Print2Cell4:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 4
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1
    jne h2Open3Cell4  
    mov rsi, '-'
    jmp h2Print3Cell4
h2Open3Cell4:
    mov rsi, ' '
h2Print3Cell4:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, 10   ; newline
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Row 2: |_|_|_| (with possible openings)
    mov rsi, '|'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, ' '
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 5
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1
    jne row2Open1Cell4
    mov rsi, '|'
    jmp row2Print1Cell4
row2Open1Cell4:
    mov rsi, ' '
row2Print1Cell4:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, ' '
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 6
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1
    jne row2Open2Cell4
    mov rsi, '|'
    jmp row2Print2Cell4  
row2Open2Cell4:
    mov rsi, ' '
row2Print2Cell4:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, ' '
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '|'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, 10   ; newline
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Horizontal walls above row 3: +-+-+-+
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 7
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1
    jne h3Open1Cell4
    mov rsi, '-'
    jmp h3Print1Cell4
h3Open1Cell4:
    mov rsi, ' '
h3Print1Cell4:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 8
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1
    jne h3Open2Cell4
    mov rsi, '-'
    jmp h3Print2Cell4
h3Open2Cell4:
    mov rsi, ' '
h3Print2Cell4:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 9
    mov ebx, [currentInteriorWall] 
    cmp byte [wallList + ebx], 1
    jne h3Open3Cell4
    mov rsi, '-'
    jmp h3Print3Cell4
h3Open3Cell4:
    mov rsi, ' '
h3Print3Cell4:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, 10   ; newline
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Row 3: |_|_|_| (with possible openings)
    mov rsi, '|'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, ' '
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 10
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1
    jne row3Open1Cell4
    mov rsi, '|'
    jmp row3Print1Cell4
row3Open1Cell4:
    mov rsi, ' '
row3Print1Cell4:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, ' '
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 11
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1
    jne row3Open2Cell4
    mov rsi, '|'
    jmp row3Print2Cell4
row3Open2Cell4:
    mov rsi, ' '
row3Print2Cell4:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, ' '
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '|'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, 10   ; newline
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Bottom border: +-+-+-+
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '-'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '-'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '-'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, 10   ; newline
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi

; Now process cell 5 - remove wall

    ; Sleep for 500ms using nanosleep system call
    mov rax, 500              ; sys_nanosleep
    mov rdi, timespecSec    ; pointer to timespec structure
    mov rsi, 0               ; NULL (don't care about remaining time)
    syscall

    ; Print removing message
    mov rax, 1         ; sys_write
    mov rdi, 1         ; stdout
    mov rsi, removingMsg5
    mov rdx, removingMsg5Len
    syscall
    
    ; Remove wall code for cell 5:
    mov dword [cellIndex], 5
    mov dword [wallRemoved], 0
    mov dword [direction], LEFT
    
    ; Try each direction until we find a removable wall
tryDirectionCell5:
    ; Check if we've tried all directions
    cmp dword [direction], 4
    jge doneCell5
    
    ; Calculate lookup table index: cellIndex * 4 + direction
    mov eax, [cellIndex]
    mov ebx, 4
    mul ebx
    add eax, [direction]
    
    ; Get wall index from lookup table
    mov ebx, eax
    mov eax, [cellToWallLUT + ebx * 4]
    
    ; Check if it's an exterior wall (-1)
    cmp eax, -1
    je nextDirectionCell5
    
    ; Store wall to remove
    mov [wallToRemove], eax
    
    ; Check if wall is still up
    mov ebx, eax
    cmp byte [wallList + ebx], 1
    jne nextDirectionCell5
    
    ; Remove the wall
    mov byte [wallList + ebx], 0
    mov dword [wallRemoved], 1
    jmp doneCell5
    
nextDirectionCell5:
    inc dword [direction]
    jmp tryDirectionCell5
    
doneCell5:

; Print maze after removing wall from cell 5
    mov dword [currentInteriorWall], 0
    
    ; Print top border: +-+-+-+
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '-'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '-'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '-'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, 10   ; newline
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Row 1: |_|_|_| (with possible openings)
    mov rsi, '|'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, ' '
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 0
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1
    jne row1Open1Cell5
    mov rsi, '|'
    jmp row1Print1Cell5
row1Open1Cell5:
    mov rsi, ' '
row1Print1Cell5:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, ' '
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 1  
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1
    jne row1Open2Cell5
    mov rsi, '|'
    jmp row1Print2Cell5
row1Open2Cell5:
    mov rsi, ' '
row1Print2Cell5:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, ' '
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '|'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, 10   ; newline
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Horizontal walls above row 2: +-+-+-+
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 2
    mov ebx, [currentInteriorWall] 
    cmp byte [wallList + ebx], 1
    jne h2Open1Cell5
    mov rsi, '-'
    jmp h2Print1Cell5
h2Open1Cell5:
    mov rsi, ' '
h2Print1Cell5:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 3
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1  
    jne h2Open2Cell5
    mov rsi, '-'
    jmp h2Print2Cell5
h2Open2Cell5:
    mov rsi, ' '
h2Print2Cell5:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 4
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1
    jne h2Open3Cell5  
    mov rsi, '-'
    jmp h2Print3Cell5
h2Open3Cell5:
    mov rsi, ' '
h2Print3Cell5:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, 10   ; newline
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Row 2: |_|_|_| (with possible openings)
    mov rsi, '|'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, ' '
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 5
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1
    jne row2Open1Cell5
    mov rsi, '|'
    jmp row2Print1Cell5
row2Open1Cell5:
    mov rsi, ' '
row2Print1Cell5:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, ' '
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 6
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1
    jne row2Open2Cell5
    mov rsi, '|'
    jmp row2Print2Cell5  
row2Open2Cell5:
    mov rsi, ' '
row2Print2Cell5:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, ' '
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '|'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, 10   ; newline
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Horizontal walls above row 3: +-+-+-+
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 7
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1
    jne h3Open1Cell5
    mov rsi, '-'
    jmp h3Print1Cell5
h3Open1Cell5:
    mov rsi, ' '
h3Print1Cell5:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 8
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1
    jne h3Open2Cell5
    mov rsi, '-'
    jmp h3Print2Cell5
h3Open2Cell5:
    mov rsi, ' '
h3Print2Cell5:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 9
    mov ebx, [currentInteriorWall] 
    cmp byte [wallList + ebx], 1
    jne h3Open3Cell5
    mov rsi, '-'
    jmp h3Print3Cell5
h3Open3Cell5:
    mov rsi, ' '
h3Print3Cell5:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, 10   ; newline
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Row 3: |_|_|_| (with possible openings)
    mov rsi, '|'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, ' '
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 10
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1
    jne row3Open1Cell5
    mov rsi, '|'
    jmp row3Print1Cell5
row3Open1Cell5:
    mov rsi, ' '
row3Print1Cell5:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, ' '
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 11
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1
    jne row3Open2Cell5
    mov rsi, '|'
    jmp row3Print2Cell5
row3Open2Cell5:
    mov rsi, ' '
row3Print2Cell5:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, ' '
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '|'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, 10   ; newline
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Bottom border: +-+-+-+
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '-'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '-'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '-'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, 10   ; newline
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi

; Now process cell 6 - remove wall

    ; Sleep for 500ms using nanosleep system call
    mov rax, 500              ; sys_nanosleep
    mov rdi, timespecSec    ; pointer to timespec structure
    mov rsi, 0               ; NULL (don't care about remaining time)
    syscall

    ; Print removing message
    mov rax, 1         ; sys_write
    mov rdi, 1         ; stdout
    mov rsi, removingMsg6
    mov rdx, removingMsg6Len
    syscall
    
    ; Remove wall code for cell 6:
    mov dword [cellIndex], 6
    mov dword [wallRemoved], 0
    mov dword [direction], LEFT
    
    ; Try each direction until we find a removable wall
tryDirectionCell6:
    ; Check if we've tried all directions
    cmp dword [direction], 4
    jge doneCell6
    
    ; Calculate lookup table index: cellIndex * 4 + direction
    mov eax, [cellIndex]
    mov ebx, 4
    mul ebx
    add eax, [direction]
    
    ; Get wall index from lookup table
    mov ebx, eax
    mov eax, [cellToWallLUT + ebx * 4]
    
    ; Check if it's an exterior wall (-1)
    cmp eax, -1
    je nextDirectionCell6
    
    ; Store wall to remove
    mov [wallToRemove], eax
    
    ; Check if wall is still up
    mov ebx, eax
    cmp byte [wallList + ebx], 1
    jne nextDirectionCell6
    
    ; Remove the wall
    mov byte [wallList + ebx], 0
    mov dword [wallRemoved], 1
    jmp doneCell6
    
nextDirectionCell6:
    inc dword [direction]
    jmp tryDirectionCell6
    
doneCell6:

; Print maze after removing wall from cell 6
    mov dword [currentInteriorWall], 0
    
    ; Print top border: +-+-+-+
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '-'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '-'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '-'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, 10   ; newline
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Row 1: |_|_|_| (with possible openings)
    mov rsi, '|'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, ' '
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 0
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1
    jne row1Open1Cell6
    mov rsi, '|'
    jmp row1Print1Cell6
row1Open1Cell6:
    mov rsi, ' '
row1Print1Cell6:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, ' '
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 1  
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1
    jne row1Open2Cell6
    mov rsi, '|'
    jmp row1Print2Cell6
row1Open2Cell6:
    mov rsi, ' '
row1Print2Cell6:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, ' '
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '|'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, 10   ; newline
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Horizontal walls above row 2: +-+-+-+
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 2
    mov ebx, [currentInteriorWall] 
    cmp byte [wallList + ebx], 1
    jne h2Open1Cell6
    mov rsi, '-'
    jmp h2Print1Cell6
h2Open1Cell6:
    mov rsi, ' '
h2Print1Cell6:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 3
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1  
    jne h2Open2Cell6
    mov rsi, '-'
    jmp h2Print2Cell6
h2Open2Cell6:
    mov rsi, ' '
h2Print2Cell6:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 4
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1
    jne h2Open3Cell6  
    mov rsi, '-'
    jmp h2Print3Cell6
h2Open3Cell6:
    mov rsi, ' '
h2Print3Cell6:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, 10   ; newline
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Row 2: |_|_|_| (with possible openings)
    mov rsi, '|'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, ' '
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 5
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1
    jne row2Open1Cell6
    mov rsi, '|'
    jmp row2Print1Cell6
row2Open1Cell6:
    mov rsi, ' '
row2Print1Cell6:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, ' '
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 6
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1
    jne row2Open2Cell6
    mov rsi, '|'
    jmp row2Print2Cell6  
row2Open2Cell6:
    mov rsi, ' '
row2Print2Cell6:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, ' '
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '|'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, 10   ; newline
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Horizontal walls above row 3: +-+-+-+
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 7
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1
    jne h3Open1Cell6
    mov rsi, '-'
    jmp h3Print1Cell6
h3Open1Cell6:
    mov rsi, ' '
h3Print1Cell6:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 8
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1
    jne h3Open2Cell6
    mov rsi, '-'
    jmp h3Print2Cell6
h3Open2Cell6:
    mov rsi, ' '
h3Print2Cell6:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 9
    mov ebx, [currentInteriorWall] 
    cmp byte [wallList + ebx], 1
    jne h3Open3Cell6
    mov rsi, '-'
    jmp h3Print3Cell6
h3Open3Cell6:
    mov rsi, ' '
h3Print3Cell6:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, 10   ; newline
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Row 3: |_|_|_| (with possible openings)
    mov rsi, '|'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, ' '
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 10
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1
    jne row3Open1Cell6
    mov rsi, '|'
    jmp row3Print1Cell6
row3Open1Cell6:
    mov rsi, ' '
row3Print1Cell6:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, ' '
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 11
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1
    jne row3Open2Cell6
    mov rsi, '|'
    jmp row3Print2Cell6
row3Open2Cell6:
    mov rsi, ' '
row3Print2Cell6:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, ' '
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '|'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, 10   ; newline
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Bottom border: +-+-+-+
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '-'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '-'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '-'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, 10   ; newline
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi

; Now process cell 7 - remove wall

    ; Sleep for 500ms using nanosleep system call
    mov rax, 500              ; sys_nanosleep
    mov rdi, timespecSec    ; pointer to timespec structure
    mov rsi, 0               ; NULL (don't care about remaining time)
    syscall

    ; Print removing message
    mov rax, 1         ; sys_write
    mov rdi, 1         ; stdout
    mov rsi, removingMsg7
    mov rdx, removingMsg7Len
    syscall
    
    ; Remove wall code for cell 7:
    mov dword [cellIndex], 7
    mov dword [wallRemoved], 0
    mov dword [direction], LEFT
    
    ; Try each direction until we find a removable wall
tryDirectionCell7:
    ; Check if we've tried all directions
    cmp dword [direction], 4
    jge doneCell7
    
    ; Calculate lookup table index: cellIndex * 4 + direction
    mov eax, [cellIndex]
    mov ebx, 4
    mul ebx
    add eax, [direction]
    
    ; Get wall index from lookup table
    mov ebx, eax
    mov eax, [cellToWallLUT + ebx * 4]
    
    ; Check if it's an exterior wall (-1)
    cmp eax, -1
    je nextDirectionCell7
    
    ; Store wall to remove
    mov [wallToRemove], eax
    
    ; Check if wall is still up
    mov ebx, eax
    cmp byte [wallList + ebx], 1
    jne nextDirectionCell7
    
    ; Remove the wall
    mov byte [wallList + ebx], 0
    mov dword [wallRemoved], 1
    jmp doneCell7
    
nextDirectionCell7:
    inc dword [direction]
    jmp tryDirectionCell7
    
doneCell7:

; Print maze after removing wall from cell 7
    mov dword [currentInteriorWall], 0
    
    ; Print top border: +-+-+-+
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '-'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '-'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '-'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, 10   ; newline
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Row 1: |_|_|_| (with possible openings)
    mov rsi, '|'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, ' '
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 0
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1
    jne row1Open1Cell7
    mov rsi, '|'
    jmp row1Print1Cell7
row1Open1Cell7:
    mov rsi, ' '
row1Print1Cell7:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, ' '
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 1  
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1
    jne row1Open2Cell7
    mov rsi, '|'
    jmp row1Print2Cell7
row1Open2Cell7:
    mov rsi, ' '
row1Print2Cell7:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, ' '
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '|'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, 10   ; newline
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Horizontal walls above row 2: +-+-+-+
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 2
    mov ebx, [currentInteriorWall] 
    cmp byte [wallList + ebx], 1
    jne h2Open1Cell7
    mov rsi, '-'
    jmp h2Print1Cell7
h2Open1Cell7:
    mov rsi, ' '
h2Print1Cell7:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 3
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1  
    jne h2Open2Cell7
    mov rsi, '-'
    jmp h2Print2Cell7
h2Open2Cell7:
    mov rsi, ' '
h2Print2Cell7:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 4
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1
    jne h2Open3Cell7  
    mov rsi, '-'
    jmp h2Print3Cell7
h2Open3Cell7:
    mov rsi, ' '
h2Print3Cell7:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, 10   ; newline
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Row 2: |_|_|_| (with possible openings)
    mov rsi, '|'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, ' '
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 5
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1
    jne row2Open1Cell7
    mov rsi, '|'
    jmp row2Print1Cell7
row2Open1Cell7:
    mov rsi, ' '
row2Print1Cell7:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, ' '
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 6
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1
    jne row2Open2Cell7
    mov rsi, '|'
    jmp row2Print2Cell7  
row2Open2Cell7:
    mov rsi, ' '
row2Print2Cell7:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, ' '
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '|'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, 10   ; newline
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Horizontal walls above row 3: +-+-+-+
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 7
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1
    jne h3Open1Cell7
    mov rsi, '-'
    jmp h3Print1Cell7
h3Open1Cell7:
    mov rsi, ' '
h3Print1Cell7:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 8
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1
    jne h3Open2Cell7
    mov rsi, '-'
    jmp h3Print2Cell7
h3Open2Cell7:
    mov rsi, ' '
h3Print2Cell7:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 9
    mov ebx, [currentInteriorWall] 
    cmp byte [wallList + ebx], 1
    jne h3Open3Cell7
    mov rsi, '-'
    jmp h3Print3Cell7
h3Open3Cell7:
    mov rsi, ' '
h3Print3Cell7:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, 10   ; newline
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Row 3: |_|_|_| (with possible openings)
    mov rsi, '|'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, ' '
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 10
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1
    jne row3Open1Cell7
    mov rsi, '|'
    jmp row3Print1Cell7
row3Open1Cell7:
    mov rsi, ' '
row3Print1Cell7:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, ' '
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 11
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1
    jne row3Open2Cell7
    mov rsi, '|'
    jmp row3Print2Cell7
row3Open2Cell7:
    mov rsi, ' '
row3Print2Cell7:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, ' '
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '|'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, 10   ; newline
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Bottom border: +-+-+-+
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '-'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '-'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '-'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, 10   ; newline
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi

; Now process cell 8 - remove wall

    ; Sleep for 500ms using nanosleep system call
    mov rax, 500              ; sys_nanosleep
    mov rdi, timespecSec    ; pointer to timespec structure
    mov rsi, 0               ; NULL (don't care about remaining time)
    syscall

    ; Print removing message
    mov rax, 1         ; sys_write
    mov rdi, 1         ; stdout
    mov rsi, removingMsg8
    mov rdx, removingMsg8Len
    syscall
    
    ; Remove wall code for cell 8:
    mov dword [cellIndex], 8
    mov dword [wallRemoved], 0
    mov dword [direction], LEFT
    
    ; Try each direction until we find a removable wall
tryDirectionCell8:
    ; Check if we've tried all directions
    cmp dword [direction], 4
    jge doneCell8
    
    ; Calculate lookup table index: cellIndex * 4 + direction
    mov eax, [cellIndex]
    mov ebx, 4
    mul ebx
    add eax, [direction]
    
    ; Get wall index from lookup table
    mov ebx, eax
    mov eax, [cellToWallLUT + ebx * 4]
    
    ; Check if it's an exterior wall (-1)
    cmp eax, -1
    je nextDirectionCell8
    
    ; Store wall to remove
    mov [wallToRemove], eax
    
    ; Check if wall is still up
    mov ebx, eax
    cmp byte [wallList + ebx], 1
    jne nextDirectionCell8
    
    ; Remove the wall
    mov byte [wallList + ebx], 0
    mov dword [wallRemoved], 1
    jmp doneCell8
    
nextDirectionCell8:
    inc dword [direction]
    jmp tryDirectionCell8
    
doneCell8:

; Print maze after removing wall from cell 8
    mov dword [currentInteriorWall], 0
    
    ; Print top border: +-+-+-+
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '-'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '-'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '-'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, 10   ; newline
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Row 1: |_|_|_| (with possible openings)
    mov rsi, '|'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, ' '
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 0
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1
    jne row1Open1Cell8
    mov rsi, '|'
    jmp row1Print1Cell8
row1Open1Cell8:
    mov rsi, ' '
row1Print1Cell8:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, ' '
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 1  
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1
    jne row1Open2Cell8
    mov rsi, '|'
    jmp row1Print2Cell8
row1Open2Cell8:
    mov rsi, ' '
row1Print2Cell8:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, ' '
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '|'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, 10   ; newline
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Horizontal walls above row 2: +-+-+-+
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 2
    mov ebx, [currentInteriorWall] 
    cmp byte [wallList + ebx], 1
    jne h2Open1Cell8
    mov rsi, '-'
    jmp h2Print1Cell8
h2Open1Cell8:
    mov rsi, ' '
h2Print1Cell8:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 3
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1  
    jne h2Open2Cell8
    mov rsi, '-'
    jmp h2Print2Cell8
h2Open2Cell8:
    mov rsi, ' '
h2Print2Cell8:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 4
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1
    jne h2Open3Cell8  
    mov rsi, '-'
    jmp h2Print3Cell8
h2Open3Cell8:
    mov rsi, ' '
h2Print3Cell8:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, 10   ; newline
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Row 2: |_|_|_| (with possible openings)
    mov rsi, '|'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, ' '
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 5
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1
    jne row2Open1Cell8
    mov rsi, '|'
    jmp row2Print1Cell8
row2Open1Cell8:
    mov rsi, ' '
row2Print1Cell8:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, ' '
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 6
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1
    jne row2Open2Cell8
    mov rsi, '|'
    jmp row2Print2Cell8  
row2Open2Cell8:
    mov rsi, ' '
row2Print2Cell8:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, ' '
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '|'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, 10   ; newline
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Horizontal walls above row 3: +-+-+-+
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 7
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1
    jne h3Open1Cell8
    mov rsi, '-'
    jmp h3Print1Cell8
h3Open1Cell8:
    mov rsi, ' '
h3Print1Cell8:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 8
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1
    jne h3Open2Cell8
    mov rsi, '-'
    jmp h3Print2Cell8
h3Open2Cell8:
    mov rsi, ' '
h3Print2Cell8:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 9
    mov ebx, [currentInteriorWall] 
    cmp byte [wallList + ebx], 1
    jne h3Open3Cell8
    mov rsi, '-'
    jmp h3Print3Cell8
h3Open3Cell8:
    mov rsi, ' '
h3Print3Cell8:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, 10   ; newline
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Row 3: |_|_|_| (with possible openings)
    mov rsi, '|'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, ' '
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 10
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1
    jne row3Open1Cell8
    mov rsi, '|'
    jmp row3Print1Cell8
row3Open1Cell8:
    mov rsi, ' '
row3Print1Cell8:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, ' '
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Check wall 11
    mov ebx, [currentInteriorWall]
    cmp byte [wallList + ebx], 1
    jne row3Open2Cell8
    mov rsi, '|'
    jmp row3Print2Cell8
row3Open2Cell8:
    mov rsi, ' '
row3Print2Cell8:
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    inc dword [currentInteriorWall]
    
    mov rsi, ' '
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '|'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, 10   ; newline
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    ; Bottom border: +-+-+-+
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '-'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '-'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '-'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, '+'
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi
    
    mov rsi, 10   ; newline
    push rsi
    mov rsi, rsp
    mov rax, 1
    mov rdi, 1
    mov rdx, 1
    syscall
    pop rsi

    ; Exit program
    mov rax, 60        ; sys_exit
    mov rdi, 0         ; exit status
    syscall
