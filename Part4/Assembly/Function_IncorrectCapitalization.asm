; error2_wrong_capitalization.asm
; Error: Function name capitalization is incorrect

section .data
    prompt db "Type 1 and press enter.", 10, 0
    format db "The user entered the integer %d", 10, 0
    input_buffer times 100 db 0

section .text
    extern printf
    extern fgets
    extern Atoi     ; ERROR: Should be 'atoi' not 'Atoi'
    extern stdin
    global _start

_start:
    ; Print prompt
    mov rdi, prompt
    call printf
    
    ; Read input
    mov rdi, input_buffer
    mov rsi, 100
    mov rdx, [stdin]
    call fgets
    
    ; ERROR: Wrong function name capitalization
    mov rdi, input_buffer
    call Atoi       ; Linker error: undefined reference to 'Atoi'
    
    ; Print result
    mov rdi, format
    mov rsi, rax
    call printf
    
    ; Exit
    mov rax, 60
    mov rdi, 0
    syscall
