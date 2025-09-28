section .rodata
    ; String literals from C++ code
    LC0 db "|0", 0
    LC1 db "2|", 0  
    LC2 db "|3", 0
    LC3 db "5|", 0
    LC4 db "|6", 0
    LC5 db "8|", 0
    LC6 db "Cell to left of cell 4 is: %d", 10, 0
    LC7 db "Cell to up of cell 4 is: %d", 10, 0
    LC8 db "Cell to right of cell 4 is: %d", 10, 0
    LC9 db "Cell to down of cell 4 is: %d", 10, 0

section .text
    global main
    extern printf
    extern putchar

main:
    ; Function prologue
    push rbp
    mov rbp, rsp
    sub rsp, 192        ; Allocate 192 bytes on stack (same as GCC)
    
    ; This maze program does not necessarily progress the program,
    ; but provides a construct we will use eventually in the program.
    
    ; Initialize mazeWalls array - char mazeWalls[][3]
    ; mazeWalls[0] = { '|', ' ', '\0' }
    mov word [rbp-175], 0x207C    ; '|', ' '
    mov byte [rbp-173], 0         ; '\0'
    
    ; mazeWalls[1] = { ' ', ' ', ' ' }
    mov word [rbp-172], 0x2020    ; ' ', ' '
    mov byte [rbp-170], 0x20      ; ' '
    
    ; mazeWalls[2] = { ' ', '|', '\0' }
    mov word [rbp-169], 0x7C20    ; ' ', '|'
    mov byte [rbp-167], 0         ; '\0'
    
    ; mazeWalls[3] = { ' ', '-', ' ' }
    mov word [rbp-166], 0x2D20    ; ' ', '-'
    mov byte [rbp-164], 0x20      ; ' '
    
    ; mazeWalls[4] = { '|', ' ', '\0' }
    mov word [rbp-163], 0x207C    ; '|', ' '
    mov byte [rbp-161], 0         ; '\0'
    
    ; Initialize constants - const int LEFT = 0, UP = 1, RIGHT = 2, DOWN = 3
    mov dword [rbp-192], 0        ; LEFT = 0
    mov dword [rbp-188], 1        ; UP = 1
    mov dword [rbp-184], 2        ; RIGHT = 2
    mov dword [rbp-180], 3        ; DOWN = 3
    
    ; Initialize neighborLookup array - int neighborLookup[][4]
    ; neighborLookup[0] = { -1, -1, -1, 3 } // Cell 0
    mov dword [rbp-160], -1
    mov dword [rbp-156], -1
    mov dword [rbp-152], -1
    mov dword [rbp-148], 3
    
    ; neighborLookup[1] = { -1, -1, 2, 4 } // Cell 1
    mov dword [rbp-144], -1
    mov dword [rbp-140], -1
    mov dword [rbp-136], 2
    mov dword [rbp-132], 4
    
    ; neighborLookup[2] = { 1, -1, -1, 5 } // Cell 2
    mov dword [rbp-128], 1
    mov dword [rbp-124], -1
    mov dword [rbp-120], -1
    mov dword [rbp-116], 5
    
    ; neighborLookup[3] = { -1, 0, 4, 6 } // Cell 3
    mov dword [rbp-112], -1
    mov dword [rbp-108], 0
    mov dword [rbp-104], 4
    mov dword [rbp-100], 6
    
    ; neighborLookup[4] = { 3, 1, -1, -1 } // Cell 4
    mov dword [rbp-96], 3
    mov dword [rbp-92], 1
    mov dword [rbp-88], -1
    mov dword [rbp-84], -1
    
    ; neighborLookup[5] = { -1, 2, -1, 8 } // Cell 5
    mov dword [rbp-80], -1
    mov dword [rbp-76], 2
    mov dword [rbp-72], -1
    mov dword [rbp-68], 8
    
    ; neighborLookup[6] = { -1, 3, -1, -1 } // Cell 6
    mov dword [rbp-64], -1
    mov dword [rbp-60], 3
    mov dword [rbp-56], -1
    mov dword [rbp-52], -1
    
    ; neighborLookup[7] = { -1, -1, 8, -1 } // Cell 7
    mov dword [rbp-48], -1
    mov dword [rbp-44], -1
    mov dword [rbp-40], 8
    mov dword [rbp-36], -1
    
    ; neighborLookup[8] = { 7, 5, -1, -1 } // Cell 8
    mov dword [rbp-32], 7
    mov dword [rbp-28], 5
    mov dword [rbp-24], -1
    mov dword [rbp-20], -1
    
    ; Initialize topBottomRow array - char topBottomRow[] = { '+', '-', '+', '-', '+', '-', '+' }
    mov dword [rbp-15], 757804331   ; Contains packed characters
    mov dword [rbp-12], 724380461   ; Contains packed characters
    
    ; This sort of code will look better when we have loops and functions
    ; We see how a 3x3 maze is created from the array of data, and using the
    ; neighbor lookup provides -1 where a wall is in that direction, otherwise
    ; provides the cell number in that direction.
    
    ; Print top border: topBottomRow[0] through topBottomRow[6]
    movzx eax, byte [rbp-15]      ; topBottomRow[0]
    movsx eax, al
    mov edi, eax
    call putchar
    
    movzx eax, byte [rbp-14]      ; topBottomRow[1]
    movsx eax, al
    mov edi, eax
    call putchar
    
    movzx eax, byte [rbp-13]      ; topBottomRow[2]
    movsx eax, al
    mov edi, eax
    call putchar
    
    movzx eax, byte [rbp-12]      ; topBottomRow[3]
    movsx eax, al
    mov edi, eax
    call putchar
    
    movzx eax, byte [rbp-11]      ; topBottomRow[4]
    movsx eax, al
    mov edi, eax
    call putchar
    
    movzx eax, byte [rbp-10]      ; topBottomRow[5]
    movsx eax, al
    mov edi, eax
    call putchar
    
    movzx eax, byte [rbp-9]       ; topBottomRow[6]
    movsx eax, al
    mov edi, eax
    call putchar
    
    ; Print newline
    mov edi, 10
    call putchar
    
    ; printf( "|0" );
    lea rax, [rel LC0]
    mov rdi, rax
    mov eax, 0
    call printf
    
    ; printf( "%c", mazeWalls[ 0 ][ 0 ] );
    movzx eax, byte [rbp-175]     ; mazeWalls[0][0]
    movsx eax, al
    mov edi, eax
    call putchar
    
    ; printf( "1" );
    mov edi, 49                   ; ASCII '1'
    call putchar
    
    ; printf( "%c", mazeWalls[ 0 ][ 1 ] );
    movzx eax, byte [rbp-174]     ; mazeWalls[0][1]
    movsx eax, al
    mov edi, eax
    call putchar
    
    ; printf( "2|" );
    lea rax, [rel LC1]
    mov rdi, rax
    mov eax, 0
    call printf
    
    ; printf( "\n" );
    mov edi, 10
    call putchar
    
    ; printf( "+" );
    mov edi, 43                   ; ASCII '+'
    call putchar
    
    ; printf( "%c", mazeWalls[ 1 ][ 0 ] );
    movzx eax, byte [rbp-172]     ; mazeWalls[1][0]
    movsx eax, al
    mov edi, eax
    call putchar
    
    ; printf( "+" );
    mov edi, 43
    call putchar
    
    ; printf( "%c", mazeWalls[ 1 ][ 1 ] );
    movzx eax, byte [rbp-171]     ; mazeWalls[1][1]
    movsx eax, al
    mov edi, eax
    call putchar
    
    ; printf( "+" );
    mov edi, 43
    call putchar
    
    ; printf( "%c", mazeWalls[ 1 ][ 2 ] );
    movzx eax, byte [rbp-170]     ; mazeWalls[1][2]
    movsx eax, al
    mov edi, eax
    call putchar
    
    ; printf( "+" );
    mov edi, 43
    call putchar
    
    ; printf( "\n" );
    mov edi, 10
    call putchar
    
    ; printf( "|3" );
    lea rax, [rel LC2]
    mov rdi, rax
    mov eax, 0
    call printf
    
    ; printf( "%c", mazeWalls[ 2 ][ 0 ] );
    movzx eax, byte [rbp-169]     ; mazeWalls[2][0]
    movsx eax, al
    mov edi, eax
    call putchar
    
    ; printf( "4" );
    mov edi, 52                   ; ASCII '4'
    call putchar
    
    ; printf( "%c", mazeWalls[ 2 ][ 1 ] );
    movzx eax, byte [rbp-168]     ; mazeWalls[2][1]
    movsx eax, al
    mov edi, eax
    call putchar
    
    ; printf( "5|" );
    lea rax, [rel LC3]
    mov rdi, rax
    mov eax, 0
    call printf
    
    ; printf( "\n" );
    mov edi, 10
    call putchar
    
    ; printf( "+" );
    mov edi, 43
    call putchar
    
    ; printf( "%c", mazeWalls[ 3 ][ 0 ] );
    movzx eax, byte [rbp-166]     ; mazeWalls[3][0]
    movsx eax, al
    mov edi, eax
    call putchar
    
    ; printf( "+" );
    mov edi, 43
    call putchar
    
    ; printf( "%c", mazeWalls[ 3 ][ 1 ] );
    movzx eax, byte [rbp-165]     ; mazeWalls[3][1]
    movsx eax, al
    mov edi, eax
    call putchar
    
    ; printf( "+" );
    mov edi, 43
    call putchar
    
    ; printf( "%c", mazeWalls[ 3 ][ 2 ] );
    movzx eax, byte [rbp-164]     ; mazeWalls[3][2]
    movsx eax, al
    mov edi, eax
    call putchar
    
    ; printf( "+" );
    mov edi, 43
    call putchar
    
    ; printf( "\n" );
    mov edi, 10
    call putchar
    
    ; printf( "|6" );
    lea rax, [rel LC4]
    mov rdi, rax
    mov eax, 0
    call printf
    
    ; printf( "%c", mazeWalls[ 4 ][ 0 ] );
    movzx eax, byte [rbp-163]     ; mazeWalls[4][0]
    movsx eax, al
    mov edi, eax
    call putchar
    
    ; printf( "7" );
    mov edi, 55                   ; ASCII '7'
    call putchar
    
    ; printf( "%c", mazeWalls[ 4 ][ 1 ] );
    movzx eax, byte [rbp-162]     ; mazeWalls[4][1]
    movsx eax, al
    mov edi, eax
    call putchar
    
    ; printf( "8|" );
    lea rax, [rel LC5]
    mov rdi, rax
    mov eax, 0
    call printf
    
    ; printf( "\n" );
    mov edi, 10
    call putchar
    
    ; Print bottom border: topBottomRow[0] through topBottomRow[6]
    movzx eax, byte [rbp-15]      ; topBottomRow[0]
    movsx eax, al
    mov edi, eax
    call putchar
    
    movzx eax, byte [rbp-14]      ; topBottomRow[1]
    movsx eax, al
    mov edi, eax
    call putchar
    
    movzx eax, byte [rbp-13]      ; topBottomRow[2]
    movsx eax, al
    mov edi, eax
    call putchar
    
    movzx eax, byte [rbp-12]      ; topBottomRow[3]
    movsx eax, al
    mov edi, eax
    call putchar
    
    movzx eax, byte [rbp-11]      ; topBottomRow[4]
    movsx eax, al
    mov edi, eax
    call putchar
    
    movzx eax, byte [rbp-10]      ; topBottomRow[5]
    movsx eax, al
    mov edi, eax
    call putchar
    
    movzx eax, byte [rbp-9]       ; topBottomRow[6]
    movsx eax, al
    mov edi, eax
    call putchar
    
    ; printf( "\n" );
    mov edi, 10
    call putchar
    
    ; So take cell number 4 and see what rooms are around it,
    ; cell 3 is to the left and cell 1 is up, but walls are right and down.
    
    ; printf( "Cell to left of cell 4 is: %d\n", neighborLookup[ 4 ][ LEFT ] );
    mov eax, [rbp-96]             ; neighborLookup[4][LEFT]
    mov esi, eax
    lea rax, [rel LC6]
    mov rdi, rax
    mov eax, 0
    call printf
    
    ; printf( "Cell to up of cell 4 is: %d\n", neighborLookup[ 4 ][ UP ] );
    mov eax, [rbp-92]             ; neighborLookup[4][UP]
    mov esi, eax
    lea rax, [rel LC7]
    mov rdi, rax
    mov eax, 0
    call printf
    
    ; printf( "Cell to right of cell 4 is: %d\n", neighborLookup[ 4 ][ RIGHT ] );
    mov eax, [rbp-88]             ; neighborLookup[4][RIGHT]
    mov esi, eax
    lea rax, [rel LC8]
    mov rdi, rax
    mov eax, 0
    call printf
    
    ; printf( "Cell to down of cell 4 is: %d\n", neighborLookup[ 4 ][ DOWN ] );
    mov eax, [rbp-84]             ; neighborLookup[4][DOWN]
    mov esi, eax
    lea rax, [rel LC9]
    mov rdi, rax
    mov eax, 0
    call printf
    
    ; return 0;
    mov eax, 0
    
    ; Function epilogue
    leave
    ret

section .note.GNU-stack noalloc noexec nowrite progbits
