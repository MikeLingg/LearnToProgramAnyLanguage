; error5_missing_parameter.asm
; Error: Not providing required function parameter

section .data
    prompt db "Type 1 and press enter.", 10, 0
    format db "The user entered the integer %d", 10, 0
    input_buffer times 100 db 0

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
    
    ; ERROR: Not setting up required parameter for atoi
    ; atoi expects string pointer in rdi, but we don't set it
    ; rdi still contains input_buffer from fgets, but let's clear it to show the error
    xor rdi, rdi            ; Clear rdi (set to NULL)
    call atoi               ; Runtime error: atoi will try to read from NULL pointer
    ; This will likely cause segmentation fault
    
    ; Print result
    mov rdi, format
    mov rsi, rax
    call printf
    
    ; Exit
    mov rax, 60
    mov rdi, 0
    syscall
