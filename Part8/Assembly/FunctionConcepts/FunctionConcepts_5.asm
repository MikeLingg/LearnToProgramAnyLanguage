; Recursive factorial function
; Demonstrates proper recursive function with base case
section .data
    result_msg db "Factorial of 10 is: ", 0
    newline db 10, 0

section .bss
    buffer resb 32

section .text
    global _start

; Function to convert integer to string
; rdi = number, rsi = buffer
int_to_string:
    push rbp
    mov rbp, rsp
    
    mov rax, rdi
    mov rbx, 10
    mov rcx, 0
    
    ; Handle zero case
    test rax, rax
    jnz .convert
    mov byte [rsi], '0'
    mov byte [rsi+1], 0
    jmp .done
    
.convert:
    ; Convert digits (in reverse)
.digit_loop:
    xor rdx, rdx
    div rbx
    add dl, '0'
    push rdx
    inc rcx
    test rax, rax
    jnz .digit_loop
    
    ; Pop digits in correct order
.pop_loop:
    pop rax
    mov [rsi], al
    inc rsi
    dec rcx
    jnz .pop_loop
    
    mov byte [rsi], 0   ; Null terminate
    
.done:
    pop rbp
    ret

; Function to get string length
strlen:
    mov rax, 0
.loop:
    cmp byte [rdi + rax], 0
    je .done
    inc rax
    jmp .loop
.done:
    ret

; Function to print string
print_string:
    push rdi
    call strlen
    mov rdx, rax
    pop rsi
    mov rax, 1
    mov rdi, 1
    syscall
    ret

; Function: factorial
; Parameter: rdi = number to calculate factorial of
; Returns: rax = factorial result
factorial:
    push rbp
    mov rbp, rsp
    
    ; Base case: if n <= 1, return 1
    cmp rdi, 1
    jle .base_case
    
    ; Recursive case: n * factorial(n-1)
    push rdi            ; Save current n
    dec rdi             ; n-1
    call factorial      ; factorial(n-1) -> result in rax
    pop rdi             ; Restore n
    mul rdi             ; n * factorial(n-1)
    
    pop rbp
    ret
    
.base_case:
    mov rax, 1          ; Return 1
    pop rbp
    ret

_start:
    ; Calculate factorial(10)
    mov rdi, 10
    call factorial
    
    ; Print the result message
    push rax            ; save factorial result
    mov rdi, result_msg
    call print_string
    
    ; Print the factorial result
    pop rdi             ; restore factorial result
    mov rsi, buffer
    call int_to_string
    mov rdi, buffer
    call print_string
    
    ; Print newline
    mov rdi, newline
    call print_string
    
    ; Exit
    mov rax, 60
    mov rdi, 0
    syscall
