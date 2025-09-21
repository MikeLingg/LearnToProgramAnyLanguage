; Infinite recursive function
; Demonstrates how recursion works at assembly level - will cause stack overflow
section .text
    global _start

recursiveFunction:
    ; Each call pushes return address on stack
    call recursiveFunction  ; This will keep calling itself
    ret                     ; This line never executes

_start:
    call recursiveFunction
    
    ; This will never be reached due to stack overflow
    mov rax, 60
    mov rdi, 0
    syscall
