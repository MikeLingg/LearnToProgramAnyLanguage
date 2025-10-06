section .text
    global _start

; ------------------------------------
; print_string
; Input:
;   RSI = pointer to string
;   RDX = length of string
; Clobbers:
;   RAX, RDI
; ------------------------------------
print_string:
    mov rax, 1          ; sys_write
    mov rdi, 1          ; stdout (fd = 1)
    syscall
    ret

_start:
    ; Reserve space for the string on the stack
    sub rsp, 32         ; Reserve 32 bytes (more than enough)

    ; Write "Hello, World!" into stack memory
    mov byte [rsp],    'H'
    mov byte [rsp+1],  'e'
    mov byte [rsp+2],  'l'
    mov byte [rsp+3],  'l'
    mov byte [rsp+4],  'o'
    mov byte [rsp+5],  ','
    mov byte [rsp+6],  ' '
    mov byte [rsp+7],  'W'
    mov byte [rsp+8],  'o'
    mov byte [rsp+9],  'r'
    mov byte [rsp+10], 'l'
    mov byte [rsp+11], 'd'
    mov byte [rsp+12], '!'
    mov byte [rsp+13], 10     ; newline
    mov byte [rsp+14], 0      ; null

    ; Set up arguments
    mov rsi, rsp         ; Address of the string in stack memory

    call print_string

    ; Clean up stack (optional for _start but good practice)
    add rsp, 32

    ; Exit
    mov rax, 60          ; sys_exit
    xor rdi, rdi         ; Exit code 0
    syscall
