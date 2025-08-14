; error6_function_variable_reversed.asm
; Error: Function name and variable name are reversed in assignment

section .data
    prompt db "Type 1 and press enter.", 10, 0
    format db "The user entered the integer %d", 10, 0
    input_buffer times 100 db 0

section .bss
    entered_integer resd 1      ; This should receive the result

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
    
    ; ERROR: Function and variable reversed in assignment
    ; Should be: call atoi, then mov [entered_integer], eax
    ; But instead we try to:
    mov rdi, input_buffer       ; Set up parameter
    call entered_integer        ; ERROR: Try to call the variable as function!
    mov [atoi], eax            ; ERROR: Try to store result in function name!
    ; This will cause: undefined reference to 'entered_integer' (as function)
    ; And: cannot use 'atoi' as memory location for storage
    
    ; Print result
    mov rdi, format
    mov esi, [entered_integer]  ; Load from the variable (which was never set correctly)
    call printf
    
    ; Exit
    mov rax, 60
    mov rdi, 0
    syscall
