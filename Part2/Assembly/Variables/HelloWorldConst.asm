section .data
    msg db "Hello, World!", 10     ; Message + newline
    len equ $ - msg

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

; ------------------------------------
; Program start
; ------------------------------------
_start:
    mov rsi, msg        ; Address of the string
    mov rdx, len        ; Length of the string
    call print_string

    ; Exit
    mov rax, 60         ; sys_exit
    xor rdi, rdi        ; Exit code 0
    syscall
