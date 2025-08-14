; error8_comparison_instead_assignment.asm
; Error: Using comparison instead of assignment/move

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
    
    ; Call atoi to convert string
    mov rdi, input_buffer
    call atoi                   ; Result (e.g., 42) is now in rax
    
    ; ERROR: Using cmp (comparison) instead of mov (assignment)
    ; Should be: mov [entered_integer], eax
    ; But instead we do:
    cmp [entered_integer], eax  ; This compares entered_integer (0) with eax (42)
    ; This only sets flags! It doesn't store the value anywhere!
    ; entered_integer remains 0 (uninitialized)
    
    ; Print result - entered_integer still contains 0, not the user's input
    mov rdi, format
    mov esi, [entered_integer]  ; This loads 0, not 42!
    call printf                 ; Prints "The user entered the integer 0"
    
    ; Exit
    mov rax, 60
    mov rdi, 0
    syscall
