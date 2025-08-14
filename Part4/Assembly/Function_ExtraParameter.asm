; error4_extra_parameter.asm
; Error: Passing too many parameters to function

section .data
    prompt db "Type 1 and press enter.", 10, 0
    format db "The user entered the integer %d", 10, 0
    input_buffer times 100 db 0
    extra_input dq 5

section .text
    extern printf
    extern fgets
    extern atoi
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
    
    ; ERROR: Passing extra parameter to atoi
    ; atoi only expects 1 parameter (string pointer in rdi)
    ; but we're setting up 2 parameters
    mov rdi, input_buffer   ; First parameter (correct)
    mov rsi, [extra_input]  ; Second parameter (WRONG! atoi doesn't expect this)
    call atoi
    ; Runtime error: atoi will interpret rsi as part of its execution
    ; This may cause undefined behavior or crashes
    
    ; Print result
    mov rdi, format
    mov rsi, rax
    call printf
    
    ; Exit
    mov rax, 60
    mov rdi, 0
    syscall
