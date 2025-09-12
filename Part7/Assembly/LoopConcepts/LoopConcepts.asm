section .data
    ; String constants
    simple_loop_msg db 'Simple 100 iteration loop.', 10, 0
    after_loop_msg db 'After Loop', 10, 0
    palindrome_msg db 'Palindrome checker.', 10, 0
    palindrome_text db 'step on no pets', 0
    is_palindrome_msg db ' is a palindrome.', 10, 0
    not_palindrome_msg db ' is NOT a palindrome.', 10, 0
    continuing_loop_msg db 'Loops continuing a loop variable.', 10, 0
    first_loop_msg db 'Loop Index First Loop: ', 0
    second_loop_msg db 'Loop Index Second Loop: ', 0
    float_loop_msg db 'Looping through a float variable.', 10, 0
    float_value_msg db 'Loop Value: ', 0
    range_msg db 'Looping over a range of values', 10, 0
    loop_index_msg db 'Loop Index: ', 0
    while_msg db 'Example while loop performing basic counting loop', 10, 0
    nested_msg db 'Nested loop, 2d, example.', 10, 0
    off_by_one_msg db 'Off by 1 loop error for array access.', 10, 0
    correct_loop_msg db 'Loop through array correct.', 10, 0
    reverse_wrong_msg db 'Wrong reverse loop.', 10, 0
    reverse_correct_msg db 'Reverse loop correct.', 10, 0
    newline db 10, 0
    space db ' ', 0
    
    ; Arrays for demonstrations
    myArray dd 1, 2, 3, 4
    arraySize equ 4
    twoDArray dd 1, 2, 3, 4, 5, 6, 7, 8, 9
    
    ; Format strings for printf
    int_format db '%d', 10, 0
    int_no_newline db '%d', 0
    string_format db '%s', 0
    float_format db '%.2f', 10, 0

section .bss
    ; Uninitialized variables
    loopIndex resd 1
    palindromeIndex resd 1
    halfLength resd 1
    isPalindrome resd 1
    loopValue resq 1    ; 64-bit for double
    intCount resd 1
    rowIndex resd 1
    columnIndex resd 1
    arrayIndex resd 1

section .text
    global _start
    extern printf
    extern exit

_start:
    ; Basic loop - count from 0 to 99
    mov rdi, string_format
    mov rsi, simple_loop_msg
    call printf
    
    mov dword [loopIndex], 0
basic_loop:
    cmp dword [loopIndex], 100
    jge basic_loop_end
    
    ; Print loop index
    mov rdi, int_format
    mov esi, dword [loopIndex]
    call printf
    
    inc dword [loopIndex]
    jmp basic_loop
basic_loop_end:
    mov rdi, string_format
    mov rsi, after_loop_msg
    call printf

    ; Palindrome checker
    mov rdi, string_format
    mov rsi, palindrome_msg
    call printf
    
    ; Calculate half length of palindrome string
    mov rcx, palindrome_text
    call strlen
    shr rax, 1              ; Divide by 2
    mov dword [halfLength], eax
    mov dword [isPalindrome], 1
    mov dword [palindromeIndex], 0

palindrome_loop:
    mov eax, dword [palindromeIndex]
    cmp eax, dword [halfLength]
    jge palindrome_check_end
    
    ; Get character from front
    mov rcx, palindrome_text
    mov eax, dword [palindromeIndex]
    add rcx, rax
    mov bl, byte [rcx]
    
    ; Get character from back
    mov rcx, palindrome_text
    call strlen
    dec rax
    sub eax, dword [palindromeIndex]
    add rcx, rax
    mov cl, byte [rcx]
    
    ; Compare characters
    cmp bl, cl
    je palindrome_continue
    mov dword [isPalindrome], 0
    jmp palindrome_check_end
    
palindrome_continue:
    inc dword [palindromeIndex]
    jmp palindrome_loop

palindrome_check_end:
    mov rdi, string_format
    mov rsi, palindrome_text
    call printf
    
    cmp dword [isPalindrome], 1
    je print_is_palindrome
    mov rdi, string_format
    mov rsi, not_palindrome_msg
    call printf
    jmp palindrome_done
print_is_palindrome:
    mov rdi, string_format
    mov rsi, is_palindrome_msg
    call printf
palindrome_done:

    ; Two loops continuing a loop variable
    mov rdi, string_format
    mov rsi, continuing_loop_msg
    call printf
    
    mov dword [loopIndex], 5
first_continuing_loop:
    cmp dword [loopIndex], 15
    jge first_continuing_end
    
    mov rdi, string_format
    mov rsi, first_loop_msg
    call printf
    mov rdi, int_format
    mov esi, dword [loopIndex]
    call printf
    
    inc dword [loopIndex]
    jmp first_continuing_loop
first_continuing_end:

    ; Continue with same variable
second_continuing_loop:
    cmp dword [loopIndex], 55
    jge second_continuing_end
    
    mov rdi, string_format
    mov rsi, second_loop_msg
    call printf
    mov rdi, int_format
    mov esi, dword [loopIndex]
    call printf
    
    inc dword [loopIndex]
    jmp second_continuing_loop
second_continuing_end:

    ; Range loop demonstration
    mov rdi, string_format
    mov rsi, range_msg
    call printf
    
    mov dword [loopIndex], 0
range_loop:
    cmp dword [loopIndex], 100
    jge range_loop_end
    
    mov rdi, string_format
    mov rsi, loop_index_msg
    call printf
    mov rdi, int_format
    mov esi, dword [loopIndex]
    call printf
    
    inc dword [loopIndex]
    jmp range_loop
range_loop_end:
    mov rdi, string_format
    mov rsi, after_loop_msg
    call printf

    ; While loop equivalent
    mov rdi, string_format
    mov rsi, while_msg
    call printf
    
    mov dword [loopIndex], 0
while_loop:
    cmp dword [loopIndex], 100
    jge while_loop_end
    
    mov rdi, int_format
    mov esi, dword [loopIndex]
    call printf
    
    inc dword [loopIndex]
    jmp while_loop
while_loop_end:
    mov rdi, string_format
    mov rsi, after_loop_msg
    call printf

    ; Nested loop for 2D array
    mov rdi, string_format
    mov rsi, nested_msg
    call printf
    
    mov dword [rowIndex], 0
nested_outer_loop:
    cmp dword [rowIndex], 3
    jge nested_outer_end
    
    mov dword [columnIndex], 0
nested_inner_loop:
    cmp dword [columnIndex], 3
    jge nested_inner_end
    
    ; Calculate array index: row * 3 + column
    mov eax, dword [rowIndex]
    mov ebx, 3
    mul ebx
    add eax, dword [columnIndex]
    
    ; Print array element
    mov rdi, int_no_newline
    mov esi, dword [twoDArray + eax*4]
    call printf
    
    mov rdi, string_format
    mov rsi, space
    call printf
    
    inc dword [columnIndex]
    jmp nested_inner_loop
nested_inner_end:
    mov rdi, string_format
    mov rsi, newline
    call printf
    
    inc dword [rowIndex]
    jmp nested_outer_loop
nested_outer_end:

    ; Off by 1 error demonstration
    mov rdi, string_format
    mov rsi, off_by_one_msg
    call printf
    
    mov dword [loopIndex], 0
off_by_one_loop:
    cmp dword [loopIndex], arraySize + 1  ; Intentional off-by-one
    jge off_by_one_end
    
    ; This will access beyond array bounds on last iteration
    cmp dword [loopIndex], arraySize
    jge skip_array_access  ; Skip to avoid crash
    
    mov eax, dword [loopIndex]
    mov rdi, int_format
    mov esi, dword [myArray + eax*4]
    call printf
    
skip_array_access:
    inc dword [loopIndex]
    jmp off_by_one_loop
off_by_one_end:

    ; Correct loop
    mov rdi, string_format
    mov rsi, correct_loop_msg
    call printf
    
    mov dword [loopIndex], 0
correct_loop:
    cmp dword [loopIndex], arraySize
    jge correct_loop_end
    
    mov eax, dword [loopIndex]
    mov rdi, int_format
    mov esi, dword [myArray + eax*4]
    call printf
    
    inc dword [loopIndex]
    jmp correct_loop
correct_loop_end:

    ; Wrong reverse loop
    mov rdi, string_format
    mov rsi, reverse_wrong_msg
    call printf
    
    mov dword [loopIndex], arraySize
wrong_reverse_loop:
    cmp dword [loopIndex], 0
    jle wrong_reverse_end
    
    mov rdi, int_format
    mov esi, dword [loopIndex]
    call printf
    
    inc dword [loopIndex]  ; Wrong direction!
    ; This would be infinite, so we'll break after a few iterations
    cmp dword [loopIndex], arraySize + 3
    jge wrong_reverse_end
    jmp wrong_reverse_loop
wrong_reverse_end:

    ; Correct reverse loop
    mov rdi, string_format
    mov rsi, reverse_correct_msg
    call printf
    
    mov dword [loopIndex], arraySize - 1
correct_reverse_loop:
    cmp dword [loopIndex], 0
    jl correct_reverse_end
    
    mov rdi, int_format
    mov esi, dword [loopIndex]
    call printf
    
    dec dword [loopIndex]
    jmp correct_reverse_loop
correct_reverse_end:

    ; Exit program
    mov rdi, 0
    call exit

; Stack function to pollute stack memory
myStackFunction:
    push rbp
    mov rbp, rsp
    
    ; Create local variables on stack
    mov dword [rbp-4], 10   ; firstInt
    mov dword [rbp-8], 20   ; secondInt  
    mov dword [rbp-12], 30  ; thirdInt
    mov dword [rbp-16], 40  ; fourthInt
    mov dword [rbp-20], 50  ; fifthInt
    
    mov eax, dword [rbp-4]  ; return firstInt
    
    mov rsp, rbp
    pop rbp
    ret

; String length function
strlen:
    push rcx
    mov rax, 0
strlen_loop:
    cmp byte [rcx + rax], 0
    je strlen_done
    inc rax
    jmp strlen_loop
strlen_done:
    pop rcx
    ret
