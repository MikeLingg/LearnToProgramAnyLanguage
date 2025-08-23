section .data
    test_string db 'radar', 0
    test_string_len equ 5
    palindrome_msg1 db 'Input string is a palindrome: ', 0
    palindrome_msg2 db 'Input string is a palindrome: ', 0
    true_str db 'true', 10, 0
    false_str db 'false', 10, 0
    fmt_s db '%s', 0
    
    ; Variables
    is_a_palindrome dq 0
    not_a_palindrome dq 0
    left_char_index dq 0
    right_char_index dq 0

section .text
    global _start
    extern printf
    extern exit

_start:
    ; Use nested conditions to test a palindrome up to a set length
    mov qword [is_a_palindrome], 0
    mov qword [left_char_index], 0
    mov qword [right_char_index], test_string_len - 1
    
    ; First character comparison
    mov rax, [left_char_index]
    mov rbx, [right_char_index]
    mov al, byte [test_string + rax]
    mov bl, byte [test_string + rbx]
    cmp al, bl
    jne .nested_not_palindrome1
    
    ; Move indices
    mov rax, [left_char_index]
    inc rax
    mov [left_char_index], rax
    mov rbx, [right_char_index]
    dec rbx
    mov [right_char_index], rbx
    
    ; Second character comparison
    mov rax, [left_char_index]
    mov rbx, [right_char_index]
    mov al, byte [test_string + rax]
    mov bl, byte [test_string + rbx]
    cmp al, bl
    jne .nested_not_palindrome1
    
    ; Move indices
    mov rax, [left_char_index]
    inc rax
    mov [left_char_index], rax
    mov rbx, [right_char_index]
    dec rbx
    mov [right_char_index], rbx
    
    ; Third character comparison
    mov rax, [left_char_index]
    mov rbx, [right_char_index]
    mov al, byte [test_string + rax]
    mov bl, byte [test_string + rbx]
    cmp al, bl
    jne .nested_not_palindrome1
    
    ; All comparisons passed
    mov qword [is_a_palindrome], 1
    
.nested_not_palindrome1:
    ; Print result
    push rbp
    mov rbp, rsp
    and rsp, -16  ; Align stack
    
    mov rdi, fmt_s
    mov rsi, palindrome_msg1
    xor rax, rax
    call printf
    
    mov rax, [is_a_palindrome]
    cmp rax, 0
    je .print_false1
    mov rdi, fmt_s
    mov rsi, true_str
    xor rax, rax
    call printf
    jmp .guarded_start
    
.print_false1:
    mov rdi, fmt_s
    mov rsi, false_str
    xor rax, rax
    call printf

.guarded_start:
    ; Guarded conditions version with length protections
    mov qword [not_a_palindrome], 0
    mov qword [left_char_index], 0
    mov qword [right_char_index], test_string_len - 1
    
    ; First guard check
    mov rax, [left_char_index]
    cmp rax, test_string_len
    jge .guard_check2
    
    mov rax, [left_char_index]
    mov rbx, [right_char_index]
    mov al, byte [test_string + rax]
    mov bl, byte [test_string + rbx]
    cmp al, bl
    je .guard_check2
    mov qword [not_a_palindrome], 1
    
.guard_check2:
    ; Second guard check
    mov rax, [left_char_index]
    cmp rax, test_string_len
    jge .guard_check3
    
    mov rax, [not_a_palindrome]
    cmp rax, 1
    je .guard_check3
    
    mov rax, [left_char_index]
    inc rax
    mov [left_char_index], rax
    mov rbx, [right_char_index]
    dec rbx
    mov [right_char_index], rbx
    
    mov rax, [left_char_index]
    mov rbx, [right_char_index]
    mov al, byte [test_string + rax]
    mov bl, byte [test_string + rbx]
    cmp al, bl
    je .guard_check3
    mov qword [not_a_palindrome], 1
    
.guard_check3:
    ; Third guard check
    mov rax, [left_char_index]
    cmp rax, test_string_len
    jge .print_guarded_result
    
    mov rax, [not_a_palindrome]
    cmp rax, 1
    je .print_guarded_result
    
    mov rax, [left_char_index]
    inc rax
    mov [left_char_index], rax
    mov rbx, [right_char_index]
    dec rbx
    mov [right_char_index], rbx
    
    mov rax, [left_char_index]
    mov rbx, [right_char_index]
    mov al, byte [test_string + rax]
    mov bl, byte [test_string + rbx]
    cmp al, bl
    je .print_guarded_result
    mov qword [not_a_palindrome], 1
    
.print_guarded_result:
    ; Print guarded result
    mov rdi, fmt_s
    mov rsi, palindrome_msg2
    xor rax, rax
    call printf
    
    mov rax, [not_a_palindrome]
    cmp rax, 1
    je .print_false2
    mov rdi, fmt_s
    mov rsi, true_str
    xor rax, rax
    call printf
    jmp .exit_program
    
.print_false2:
    mov rdi, fmt_s
    mov rsi, false_str
    xor rax, rax
    call printf

.exit_program:
    mov rsp, rbp
    pop rbp
    mov rdi, 0
    call exit
