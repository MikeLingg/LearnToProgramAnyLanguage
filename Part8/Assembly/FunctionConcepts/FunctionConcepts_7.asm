; Main function accessing program arguments
; Demonstrates how command line arguments are accessed in assembly
section .data
    argc_msg db "Number of arguments: ", 0
    argv_msg db "Arguments:", 10, 0
    arg_prefix db 9, "Argument ", 0
    colon_space db ": ", 0
    newline db 10, 0

section .bss
    buffer resb 32
    saved_argc resq 1
    saved_argv resq 16      ; Save up to 16 argument pointers

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
    mov rdx, rax        ; length
    pop rsi             ; string
    mov rax, 1          ; sys_write
    mov rdi, 1          ; stdout
    syscall
    ret

_start:
    ; Save argc and argv before any function calls
    mov rax, [rsp]              ; argc
    mov [saved_argc], rax
    
    ; Save all argv pointers
    mov rcx, 0
.save_argv:
    cmp rcx, rax                ; compare with argc
    jge .save_done
    mov rdx, [rsp + 8 + rcx*8]  ; argv[rcx]
    mov [saved_argv + rcx*8], rdx
    inc rcx
    jmp .save_argv
    
.save_done:
    ; Print "Number of arguments: "
    mov rdi, argc_msg
    call print_string
    
    ; Print argc
    mov rdi, [saved_argc]
    mov rsi, buffer
    call int_to_string
    mov rdi, buffer
    call print_string
    
    ; Print newline
    mov rdi, newline
    call print_string
    
    ; Print "Arguments:"
    mov rdi, argv_msg
    call print_string
    
    ; Print each argument
    mov r13, 0          ; counter (use different register)
    
.arg_loop:
    cmp r13, [saved_argc]       ; compare with saved argc
    jge .done
    
    ; Print tab and "Argument "
    mov rdi, arg_prefix
    call print_string
    
    ; Print argument number (the counter, not the address!)
    mov rdi, r13
    mov rsi, buffer
    call int_to_string
    mov rdi, buffer
    call print_string
    
    ; Print ": "
    mov rdi, colon_space
    call print_string
    
    ; Print the actual argument using saved pointer
    mov rdi, [saved_argv + r13*8]
    call print_string
    
    ; Print newline
    mov rdi, newline
    call print_string
    
    inc r13
    jmp .arg_loop
    
.done:
    ; Exit
    mov rax, 60
    mov rdi, 0
    syscall
