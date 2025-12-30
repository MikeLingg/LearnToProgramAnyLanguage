section .data
    ; Note the structured example is assuming zero based indexing.
    ; Assembly naturally uses offsets from base addresses
    ; Arrays are just contiguous memory blocks with calculated offsets
    
    ; Arrays - stored as contiguous memory blocks
    temperatures dd 55, 58, 60, 65, 70, 73, 76, 79, 81, 83, 84, 85, 85, 84, 83, 81, 78, 75, 72, 69, 65, 62, 59, 57
    testScores dd 95, 75, 86, 86, 78, 94
    bookNumber dd 12495, 35786, 15863, 84962, 42697
    
    ; Variables for indexing
    bookTwoIndex dd 1
    hourCount dd 24
    firstTemperatureIndex dd 0
    lastTemperatureIndex dd 0 ; Will calculate the last index
    bookIndex dd 2
    largeArraySize dd 10000
    
    ; Large arrays - reserves space but doesn't initialize
    largeArray times 10000 db 0
    largeArray1 times 1000 dd 0
    largeArray2 times 5000 dq 0.0
    
    ; Character arrays
    myString times 100 db 0
    myString1 db 'H', 'e', 'l', 'l', 'o', ' ', 'W', 'o', 'r', 'l', 'd', '.', 0
    times 87 db 0  ; pad to 100 bytes
    myString2 db "Hello World.", 0
    times 87 db 0  ; pad to 100 bytes
    
    ; Test struct - memory layout matters here
    testStruct:
        .intArray times 10 dd 0
        .myInt dd 0
    
    ; 2D Array - stored in row-major order
    twoDArray times 16 db 0
    
    ; Color constants
    RED equ 0
    GREEN equ 1
    BLUE equ 2
    YELLOW equ 3
    CYAN equ 4
    MAGENTA equ 5
    WHITE equ 6

    RGB_RED equ 0
    RGB_GREEN equ 1
    RGB_BLUE equ 2

    RGB_COLUMN_COUNT equ 3
    VALUE_SIZE equ 4
    
    ; Color table - 7 rows x 3 columns of 32-bit integers
    colorTable dd 255, 0,   0,     ; Red
               dd 0,   255, 0,     ; Green
               dd 0,   0,   255,   ; Blue
               dd 255, 255, 0,     ; Yellow
               dd 0,   255, 255,   ; Cyan
               dd 255, 0,   255,   ; Magenta
               dd 255, 255, 255    ; White
    
    ; String literals for output
    msg_temp_tenth db "Temperature at tenth hour is %d", 10, 0
    msg_grade_fourth db "Fourth student grade is %d", 10, 0
    msg_book_second db "Second book index is %d", 10, 0
    msg_temp_first db "First temperature is %d", 10, 0
    msg_temp_last db "Last temperature is %d", 10, 0
    msg_temp_now db "First temperature is now %d", 10, 0
    msg_score_now db "Fourth test score is now %d", 10, 0
    msg_book_now db "Third book number is now %d", 10, 0
    msg_large_init db "First large array first and last initial values: %d %d", 10, 0
    msg_large_init2 db "Second large array first and last initial values: %d %d", 10, 0
    msg_large_init3 db "Third large array first and last initial values: %f %f", 10, 0
    msg_large_set db "First large array first and last values: %d %d", 10, 0
    msg_large_set2 db "Second large array first and last values: %d %d", 10, 0
    msg_large_set3 db "Third large array first and last values: %.1f %.2f", 10, 0
    msg_string db "%s", 10, 0
    msg_myint db "myInt value: %d", 10, 0
    msg_outofbounds db "Out of bounds array value: %d", 10, 0
    msg_2d_array db "twoDArray memory location as flat data: ", 0
    msg_char db "%c", 0
    msg_newline db 10, 0
    msg_cyan db "CYAN color values: %d %d %d", 10, 0

section .text
    global _start
    extern printf
    extern exit

_start:
    ; 10th entry (0-based index 9) - temperatures[9]
    mov esi, [temperatures + 9*4]  ; 9 * sizeof(int) = 9 * 4 bytes
    mov rdi, msg_temp_tenth
    call printf
    
    ; 4th entry (0-based index 3) - testScores[3]
    mov esi, [testScores + 3*4]
    mov rdi, msg_grade_fourth
    call printf
    
    ; 2nd entry (0-based index 1) - bookNumber[bookTwoIndex]
    mov eax, [bookTwoIndex]
    mov esi, [bookNumber + eax*4]
    mov rdi, msg_book_second
    call printf
    
    ; First temperature - temperatures[firstTemperatureIndex]
    mov eax, [firstTemperatureIndex]
    mov esi, [temperatures + eax*4]
    mov rdi, msg_temp_first
    call printf

    ; Calculate last temperature index (hourCount - 1)
    mov eax, [hourCount]         ; Load hourCount (24)
    dec eax                      ; Subtract 1 to get last index (23)
    mov [lastTemperatureIndex], eax  ; Store in lastTemperatureIndex variable
    
    ; Last temperature - temperatures[lastTemperatureIndex]
    mov eax, [lastTemperatureIndex]
    mov esi, [temperatures + eax*4]
    mov rdi, msg_temp_last
    call printf
    
    ; Set temperature first entry to 65
    mov dword [temperatures], 65
    mov esi, [temperatures]
    mov rdi, msg_temp_now
    call printf
    
    ; Set testScores fourth entry to 99
    mov dword [testScores + 3*4], 99
    mov esi, [testScores + 3*4]
    mov rdi, msg_score_now
    call printf
    
    ; Set bookNumber at third entry to 75681
    mov eax, [bookIndex]
    mov dword [bookNumber + eax*4], 75681
    mov esi, [bookNumber + eax*4]
    mov rdi, msg_book_now
    call printf
    
    ; Large arrays - show initial values (garbage)
    mov eax, 0                          ; Index 0
    mov esi, [largeArray + eax]       ; largeArray[0] - load actual value
    mov eax, [largeArraySize]           ; Load array size
    dec eax                             ; Calculate last index (size - 1)
    mov edx, [largeArray + eax]       ; largeArray[largeArraySize-1] - load actual value
    mov rdi, msg_large_init
    call printf
    
    mov esi, [largeArray1]
    mov edx, [largeArray1 + 999*4]
    mov rdi, msg_large_init2
    call printf

    ; Large array 2 - floating point values
    movsd xmm0, [largeArray2]           ; largeArray2[0] - first double
    mov eax, 5000                       ; Size of largeArray2
    dec eax                             ; Calculate last index (4999)
    movsd xmm1, [largeArray2 + eax*8]   ; largeArray2[4999] - last double (note *8 for qword)
    mov rdi, msg_large_init3
    call printf
    
    ; Set largeArray first entry to 1 (true)
    mov byte [largeArray], 1
    ; Set largeArray last entry to 0 (false)
    mov eax, [largeArraySize]
    dec eax
    mov byte [largeArray + eax], 0
    
    ; Display boolean values as integers
    movzx esi, byte [largeArray]
    mov eax, [largeArraySize]
    dec eax
    movzx edx, byte [largeArray + eax]
    mov rdi, msg_large_set
    call printf
    
    ; Set largeArray1 first and last entries
    mov dword [largeArray1], 25
    mov dword [largeArray1 + 999*4], 55
    mov esi, [largeArray1]
    mov edx, [largeArray1 + 999*4]
    mov rdi, msg_large_set2
    call printf

; Large array 2 - floating point values (initial 0s)
    ; Update largeArray2 values
    mov rax, __float64__(27.5)          ; Load 27.5 as 64-bit float constant
    mov [largeArray2], rax              ; Store to largeArray2[0]
    
    mov rax, __float64__(58.25)         ; Load 58.25 as 64-bit float constant
    mov ecx, 5000                       ; Size of largeArray2
    dec ecx                             ; Calculate last index (4999)
    mov [largeArray2 + ecx*8], rax      ; Store to largeArray2[4999]

    ; Print updated values
    movsd xmm0, [largeArray2]           ; largeArray2[0] - now 27.5
    mov eax, 5000                       ; Size of largeArray2
    dec eax                             ; Calculate last index (4999)
    movsd xmm1, [largeArray2 + eax*8]   ; largeArray2[4999] - now 58.25
    mov rdi, msg_large_init3
    call printf
    
    ; Character array - set each character individually
    mov byte [myString + 0], 'H'
    mov byte [myString + 1], 'e'
    mov byte [myString + 2], 'l'
    mov byte [myString + 3], 'l'
    mov byte [myString + 4], 'o'
    mov byte [myString + 5], ' '
    mov byte [myString + 6], 'W'
    mov byte [myString + 7], 'o'
    mov byte [myString + 8], 'r'
    mov byte [myString + 9], 'l'
    mov byte [myString + 10], 'd'
    mov byte [myString + 11], '.'
    mov byte [myString + 12], 0
    
    ; Print the strings
    mov rsi, myString
    mov rdi, msg_string
    call printf
    
    mov rsi, myString1
    mov rdi, msg_string
    call printf
    
    mov rsi, myString2
    mov rdi, msg_string
    call printf
    
    ; Dangerous buffer overflow - write beyond array bounds
    ; This demonstrates the C++ dangerous operation in assembly
    ; testStruct.intArray[10] = 55 - write to 11th element (past end)
    mov dword [testStruct.intArray + 10*4], 55  ; This writes to myInt!
    
    ; myInt is right after intArray, so this should show corruption
    mov esi, [testStruct.myInt]
    mov rdi, msg_myint
    call printf
    
    ; Increment myInt
    inc dword [testStruct.myInt]
    
    ; Read the out-of-bounds location
    mov esi, [testStruct.intArray + 10*4]  ; Reading myInt through array
    mov rdi, msg_outofbounds
    call printf
    
    ; 2D Array setup - store in row-major order
    mov byte [twoDArray + 0*4 + 0], '0'
    mov byte [twoDArray + 0*4 + 1], '1'
    mov byte [twoDArray + 0*4 + 2], '2'
    mov byte [twoDArray + 0*4 + 3], '3'
    mov byte [twoDArray + 1*4 + 0], '4'
    mov byte [twoDArray + 1*4 + 1], '5'
    mov byte [twoDArray + 1*4 + 2], '6'
    mov byte [twoDArray + 1*4 + 3], '7'
    mov byte [twoDArray + 2*4 + 0], '8'
    mov byte [twoDArray + 2*4 + 1], '9'
    mov byte [twoDArray + 2*4 + 2], 'A'
    mov byte [twoDArray + 2*4 + 3], 'B'
    mov byte [twoDArray + 3*4 + 0], 'C'
    mov byte [twoDArray + 3*4 + 1], 'D'
    mov byte [twoDArray + 3*4 + 2], 'E'
    mov byte [twoDArray + 3*4 + 3], 'F'
    
    ; Print 2D array header
    mov rdi, msg_2d_array
    call printf
    
    ; Print 2D array as flat data - nested loops in assembly
    mov rcx, 0  ; i = 0
outer_loop:
    cmp rcx, 4
    jge end_outer
    mov rbx, 0  ; j = 0
inner_loop:
    cmp rbx, 4
    jge end_inner
    ; Calculate offset: i*4 + j
    mov rax, rcx
    imul rax, 4
    add rax, rbx
    movzx esi, byte [twoDArray + rax]
    mov rdi, msg_char
    push rcx
    push rbx
    call printf
    pop rbx
    pop rcx
    inc rbx
    jmp inner_loop
end_inner:
    inc rcx
    jmp outer_loop
end_outer:
    
    mov rdi, msg_newline
    call printf
    
    ; Color table access - colorTable[CYAN][0], [1], [2]
    ; CYAN = 4, so we want row 4: offset = 4 * 3 * 4 = 48 bytes
    mov esi, [colorTable + CYAN*RGB_COLUMN_COUNT*VALUE_SIZE + RGB_RED*VALUE_SIZE]  ; CYAN red
    mov edx, [colorTable + CYAN*RGB_COLUMN_COUNT*VALUE_SIZE + RGB_GREEN*VALUE_SIZE]  ; CYAN green
    mov ecx, [colorTable + CYAN*RGB_COLUMN_COUNT*VALUE_SIZE + RGB_BLUE*VALUE_SIZE]  ; CYAN blue
    mov rdi, msg_cyan
    call printf
    
    ; Exit program
    mov rdi, 0
    call exit

section .note.GNU-stack
