; Basic function with no parameters or return value
section .data
    hello_msg db "Hello", 10, 0
    hello_len equ $ - hello_msg - 1

section .text
    global _start

printHello:
    ; Write system call
    mov rax, 1          ; sys_write
    mov rdi, 1          ; stdout
    mov rsi, hello_msg  ; message
    mov rdx, hello_len  ; length
    syscall
    ret

_start:
    call printHello
    
    ; Exit system call
    mov rax, 60         ; sys_exit
    mov rdi, 0          ; exit status
    syscall
