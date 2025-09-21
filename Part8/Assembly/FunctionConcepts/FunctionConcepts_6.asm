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
    ; At program start, the stack layout is:
    ; [rsp] = argc
    ; [rsp+8] = argv[0] (program name)
    ; [rsp+16] = argv[1] (first argument)
    ; [rsp+24] = argv[2] (second argument)
    ; etc.
    
    mov rbx, [rsp]      ; argc (number of arguments including program name)
    mov r12, rsp        ; save stack pointer for argv access
    
    ; Print "Number of arguments: "
    mov rdi, argc_msg
    call print_string
    
    ; Print argc
    mov rdi, rbx
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
    mov rcx, 0          ; counter
    
.arg_loop:
    cmp rcx, rbx        ; compare with argc
    jge .done
    
    ; Print tab and "Argument "
    mov rdi, arg_prefix
    call print_string
    
    ; Print argument number
    mov rdi, rcx
    mov rsi, buffer
    call int_to_string
    mov rdi, buffer
    call print_string
    
    ; Print ": "
    mov rdi, colon_space
    call print_string
    
    ; Print the actual argument
    ; argv[rcx] is at [r12 + 8 + rcx*8]
    mov rdi, [r12 + 8 + rcx*8]
    call print_string
    
    ; Print newline
    mov rdi, newline
    call print_string
    
    inc rcx
    jmp .arg_loop
    
.done:
    ; Exit
    mov rax, 60
    mov rdi, 0
    syscall
