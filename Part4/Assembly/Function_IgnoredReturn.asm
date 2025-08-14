; error1_ignored_return.asm
; Error: Ignored return value - function called but result not stored

section .data
    prompt db "Type 1 and press enter.", 10, 0
    format db "The user entered the integer %d", 10, 0
    input_buffer times 100 db 0

section .bss
    entered_integer resd 1      ; Uninitialized 32-bit integer in .bss section

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
    
    ; ERROR: Call atoi but ignore return value in rax
    mov rdi, input_buffer
    call atoi
    ; Result is in rax but we ignore it! We never store it in entered_integer
    
    ; Print using uninitialized memory location
    mov rdi, format
    mov esi, [entered_integer]  ; Load uninitialized data from memory
    ; This will print garbage - whatever random data was in that memory location
    call printf
    
    ; Exit
    mov rax, 60
    mov rdi, 0
    syscall