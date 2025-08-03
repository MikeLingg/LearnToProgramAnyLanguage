section .data
    ; This maze program does not necessarily progress the program, 
    ; but provides a construct we will use eventually in the program.
    
    ; mazeWalls - 5 rows x 3 columns of characters
    mazeWalls db '|', ' ', 0, ' ', ' ', ' ', ' ', '|', 0, ' ', '-', ' ', '|', ' ', 0
    
    ; Constants for directions
    LEFT equ 0
    UP equ 1
    RIGHT equ 2
    DOWN equ 3
    
    ; neighborLookup - 9 rows x 4 columns of 32-bit integers
    neighborLookup dd -1, -1, -1, 3,  ; Cell 0
                   dd -1, -1, 2, 4,   ; Cell 1
                   dd 1, -1, -1, 5,   ; Cell 2
                   dd -1, 0, 4, 6,    ; Cell 3
                   dd 3, 1, -1, -1,   ; Cell 4
                   dd -1, 2, -1, 8,   ; Cell 5
                   dd -1, 3, -1, -1,  ; Cell 6
                   dd -1, -1, 8, -1,  ; Cell 7
                   dd 7, 5, -1, -1    ; Cell 8
    
    ; topBottomRow - 7 characters
    topBottomRow db '+', '-', '+', '-', '+', '-', '+'
    
    ; Format strings for output
    char_fmt db "%c", 0
    newline_fmt db 10, 0
    string_fmt db "%s", 0
    int_fmt db "%d", 10, 0
    
    ; String literals for maze output
    cell_0 db "|0", 0
    cell_1 db "1", 0
    cell_2 db "2|", 0
    plus_char db "+", 0
    cell_3 db "|3", 0
    cell_4 db "4", 0
    cell_5 db "5|", 0
    cell_6 db "|6", 0
    cell_7 db "7", 0
    cell_8 db "8|", 0
    
    ; Output messages
    msg_left db "Cell to left of cell 4 is: %d", 10, 0
    msg_up db "Cell to up of cell 4 is: %d", 10, 0
    msg_right db "Cell to right of cell 4 is: %d", 10, 0
    msg_down db "Cell to down of cell 4 is: %d", 10, 0

section .text
    global _start
    extern printf
    extern exit

_start:
    ; This sort of code will look better when we have loops and functions
    ; We see how a 3x3 maze is created from the array of data, and using the 
    ; neighbor lookup provides -1 where a wall is in that direction, otherwise 
    ; provides the cell number in that direction.
    
    ; Print top border: topBottomRow[0] through topBottomRow[6]
    movzx esi, byte [topBottomRow + 0]
    mov rdi, char_fmt
    call printf
    
    movzx esi, byte [topBottomRow + 1]
    mov rdi, char_fmt
    call printf
    
    movzx esi, byte [topBottomRow + 2]
    mov rdi, char_fmt
    call printf
    
    movzx esi, byte [topBottomRow + 3]
    mov rdi, char_fmt
    call printf
    
    movzx esi, byte [topBottomRow + 4]
    mov rdi, char_fmt
    call printf
    
    movzx esi, byte [topBottomRow + 5]
    mov rdi, char_fmt
    call printf
    
    movzx esi, byte [topBottomRow + 6]
    mov rdi, char_fmt
    call printf
    
    mov rdi, newline_fmt
    call printf
    
    ; Print first row: |0 + mazeWalls[0][0] + 1 + mazeWalls[0][1] + 2|
    mov rsi, cell_0
    mov rdi, string_fmt
    call printf
    
    ; mazeWalls[0][0] - first row, first column
    movzx esi, byte [mazeWalls + 0*3 + 0]
    mov rdi, char_fmt
    call printf
    
    mov rsi, cell_1
    mov rdi, string_fmt
    call printf
    
    ; mazeWalls[0][1] - first row, second column
    movzx esi, byte [mazeWalls + 0*3 + 1]
    mov rdi, char_fmt
    call printf
    
    mov rsi, cell_2
    mov rdi, string_fmt
    call printf
    
    mov rdi, newline_fmt
    call printf
    
    ; Print second row: + + mazeWalls[1][0] + + + mazeWalls[1][1] + + + mazeWalls[1][2] + +
    mov rsi, plus_char
    mov rdi, string_fmt
    call printf
    
    movzx esi, byte [mazeWalls + 1*3 + 0]
    mov rdi, char_fmt
    call printf
    
    mov rsi, plus_char
    mov rdi, string_fmt
    call printf
    
    movzx esi, byte [mazeWalls + 1*3 + 1]
    mov rdi, char_fmt
    call printf
    
    mov rsi, plus_char
    mov rdi, string_fmt
    call printf
    
    movzx esi, byte [mazeWalls + 1*3 + 2]
    mov rdi, char_fmt
    call printf
    
    mov rsi, plus_char
    mov rdi, string_fmt
    call printf
    
    mov rdi, newline_fmt
    call printf
    
    ; Print third row: |3 + mazeWalls[2][0] + 4 + mazeWalls[2][1] + 5|
    mov rsi, cell_3
    mov rdi, string_fmt
    call printf
    
    movzx esi, byte [mazeWalls + 2*3 + 0]
    mov rdi, char_fmt
    call printf
    
    mov rsi, cell_4
    mov rdi, string_fmt
    call printf
    
    movzx esi, byte [mazeWalls + 2*3 + 1]
    mov rdi, char_fmt
    call printf
    
    mov rsi, cell_5
    mov rdi, string_fmt
    call printf
    
    mov rdi, newline_fmt
    call printf
    
    ; Print fourth row: + + mazeWalls[3][0] + + + mazeWalls[3][1] + + + mazeWalls[3][2] + +
    mov rsi, plus_char
    mov rdi, string_fmt
    call printf
    
    movzx esi, byte [mazeWalls + 3*3 + 0]
    mov rdi, char_fmt
    call printf
    
    mov rsi, plus_char
    mov rdi, string_fmt
    call printf
    
    movzx esi, byte [mazeWalls + 3*3 + 1]
    mov rdi, char_fmt
    call printf
    
    mov rsi, plus_char
    mov rdi, string_fmt
    call printf
    
    movzx esi, byte [mazeWalls + 3*3 + 2]
    mov rdi, char_fmt
    call printf
    
    mov rsi, plus_char
    mov rdi, string_fmt
    call printf
    
    mov rdi, newline_fmt
    call printf
    
    ; Print fifth row: |6 + mazeWalls[4][0] + 7 + mazeWalls[4][1] + 8|
    mov rsi, cell_6
    mov rdi, string_fmt
    call printf
    
    movzx esi, byte [mazeWalls + 4*3 + 0]
    mov rdi, char_fmt
    call printf
    
    mov rsi, cell_7
    mov rdi, string_fmt
    call printf
    
    movzx esi, byte [mazeWalls + 4*3 + 1]
    mov rdi, char_fmt
    call printf
    
    mov rsi, cell_8
    mov rdi, string_fmt
    call printf
    
    mov rdi, newline_fmt
    call printf
    
    ; Print bottom border: topBottomRow[0] through topBottomRow[6]
    movzx esi, byte [topBottomRow + 0]
    mov rdi, char_fmt
    call printf
    
    movzx esi, byte [topBottomRow + 1]
    mov rdi, char_fmt
    call printf
    
    movzx esi, byte [topBottomRow + 2]
    mov rdi, char_fmt
    call printf
    
    movzx esi, byte [topBottomRow + 3]
    mov rdi, char_fmt
    call printf
    
    movzx esi, byte [topBottomRow + 4]
    mov rdi, char_fmt
    call printf
    
    movzx esi, byte [topBottomRow + 5]
    mov rdi, char_fmt
    call printf
    
    movzx esi, byte [topBottomRow + 6]
    mov rdi, char_fmt
    call printf
    
    mov rdi, newline_fmt
    call printf
    
    ; So take cell number 4 and see what rooms are around it, 
    ; cell 3 is to the left and cell 1 is up, but walls are right and down.
    
    ; neighborLookup[4][LEFT] - row 4, column LEFT (0)
    mov esi, [neighborLookup + 4*4*4 + LEFT*4]  ; 4 bytes per int, 4 ints per row
    mov rdi, msg_left
    call printf
    
    ; neighborLookup[4][UP] - row 4, column UP (1)
    mov esi, [neighborLookup + 4*4*4 + UP*4]
    mov rdi, msg_up
    call printf
    
    ; neighborLookup[4][RIGHT] - row 4, column RIGHT (2)
    mov esi, [neighborLookup + 4*4*4 + RIGHT*4]
    mov rdi, msg_right
    call printf
    
    ; neighborLookup[4][DOWN] - row 4, column DOWN (3)
    mov esi, [neighborLookup + 4*4*4 + DOWN*4]
    mov rdi, msg_down
    call printf
    
    ; Exit program
    mov rdi, 0
    call exit

section .note.GNU-stack
