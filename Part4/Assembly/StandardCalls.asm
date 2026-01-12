section .data
    prompt55 db "Type 55 and press enter.", 10
    prompt55_len equ $ - prompt55
    prompt55_5 db "Type 55.5 and press enter.", 10
    prompt55_5_len equ $ - prompt55_5
    prompthello db "Type Hello World! and press enter.", 10
    prompthello_len equ $ - prompthello
    prompt123abc db "Type 123abc and press enter.", 10
    prompt123abc_len equ $ - prompt123abc
    prompt123_45 db "Type 123.45 and press enter.", 10
    prompt123_45_len equ $ - prompt123_45
    promptabc123 db "Type abc123 and press enter.", 10
    promptabc123_len equ $ - promptabc123
    promptspace db "Type  567 and press enter.", 10
    promptspace_len equ $ - promptspace
    promptplus db "Type +567 and press enter.", 10
    promptplus_len equ $ - promptplus
    promptminus db "Type -567 and press enter.", 10
    promptminus_len equ $ - promptminus
    prompthello2 db "Type Hello World! and press enter.", 10
    prompthello2_len equ $ - prompthello2
    promptabc2 db "Type abc123 and press enter.", 10
    promptabc2_len equ $ - promptabc2

    int_fmt db "The user entered the integer %d", 10, 0
    float_fmt db "The user entered the float %f", 10, 0
    string_fmt db "The user entered the string %s", 0

    abs_fmt db "Abs of -5 is %ld", 10, 0
    fabs_fmt db "Fabs of -5 is %g", 10, 0
    sin_fmt db "Sin of 90 is %g", 10, 0

section .bss
    user_input resb 100
    output_buffer resb 256

section .text
    global _start
    extern sprintf, atoi, atof, labs, fabs, sin

; Helper function: write string using system call
sys_write:
    mov rax, 1
    mov rdx, rsi
    mov rsi, rdi
    mov rdi, 1
    syscall
    ret

; Helper function: read line using system call  
sys_read:
    push rbx
    mov rbx, rdi
    mov rax, 0
    mov rdx, rsi
    mov rsi, rdi
    mov rdi, 0
    syscall
    mov byte [rbx + rax - 1], 0
    pop rbx
    ret

; Helper function: calculate string length
strlen_func:
    xor rax, rax
strlen_loop:
    cmp byte [rdi + rax], 0
    je strlen_done
    inc rax
    jmp strlen_loop
strlen_done:
    ret

; Helper function: format and write
format_and_write:
    push rbp
    mov rbp, rsp
    ; rdi = format, rsi = value
    mov rdx, rsi        ; value
    mov rsi, rdi        ; format
    mov rdi, output_buffer
    xor rax, rax
    call sprintf
    
    mov rdi, output_buffer
    call strlen_func
    
    mov rdi, output_buffer
    mov rsi, rax
    call sys_write
    
    pop rbp
    ret

_start:
    and rsp, -16        ; Align stack
    
    ; Type 55 and press enter
    mov rdi, prompt55
    mov rsi, prompt55_len
    call sys_write
    
    mov rdi, user_input
    mov rsi, 100
    call sys_read
    
    mov rdi, user_input
    call atoi
    mov rdi, int_fmt
    mov rsi, rax
    call format_and_write

    ; Type 55.5 and press enter
    mov rdi, prompt55_5
    mov rsi, prompt55_5_len
    call sys_write
    
    mov rdi, user_input
    mov rsi, 100
    call sys_read
    
    mov rdi, user_input
    call atof
    mov rdi, output_buffer
    mov rsi, float_fmt
    mov rax, 1          ; 1 floating point arg
    call sprintf
    
    mov rdi, output_buffer
    call strlen_func
    mov rdi, output_buffer
    mov rsi, rax
    call sys_write

    ; Type Hello World! and press enter
    mov rdi, prompthello
    mov rsi, prompthello_len
    call sys_write
    
    mov rdi, user_input
    mov rsi, 100
    call sys_read
    
    mov rdi, output_buffer
    mov rsi, string_fmt
    mov rdx, user_input
    xor rax, rax
    call sprintf
    
    mov rdi, output_buffer
    call strlen_func
    mov rdi, output_buffer
    mov rsi, rax
    call sys_write

    ; Type 123abc and press enter
    mov rdi, prompt123abc
    mov rsi, prompt123abc_len
    call sys_write
    
    mov rdi, user_input
    mov rsi, 100
    call sys_read
    
    mov rdi, user_input
    call atoi
    mov rdi, int_fmt
    mov rsi, rax
    call format_and_write

    ; Type 123.45 and press enter
    mov rdi, prompt123_45
    mov rsi, prompt123_45_len
    call sys_write
    
    mov rdi, user_input
    mov rsi, 100
    call sys_read
    
    mov rdi, user_input
    call atoi
    mov rdi, int_fmt
    mov rsi, rax
    call format_and_write

    ; Type abc123 and press enter
    mov rdi, promptabc123
    mov rsi, promptabc123_len
    call sys_write
    
    mov rdi, user_input
    mov rsi, 100
    call sys_read
    
    mov rdi, user_input
    call atoi
    mov rdi, int_fmt
    mov rsi, rax
    call format_and_write

    ; Type  567 and press enter
    mov rdi, promptspace
    mov rsi, promptspace_len
    call sys_write
    
    mov rdi, user_input
    mov rsi, 100
    call sys_read
    
    mov rdi, user_input
    call atoi
    mov rdi, int_fmt
    mov rsi, rax
    call format_and_write

    ; Type +567 and press enter
    mov rdi, promptplus
    mov rsi, promptplus_len
    call sys_write
    
    mov rdi, user_input
    mov rsi, 100
    call sys_read
    
    mov rdi, user_input
    call atoi
    mov rdi, int_fmt
    mov rsi, rax
    call format_and_write

    ; Type -567 and press enter
    mov rdi, promptminus
    mov rsi, promptminus_len
    call sys_write
    
    mov rdi, user_input
    mov rsi, 100
    call sys_read
    
    mov rdi, user_input
    call atoi
    mov rdi, int_fmt
    mov rsi, rax
    call format_and_write

    ; Math demonstrations
    
    ; Labs of -5
    mov rdi, -5
    call labs
    mov rdi, abs_fmt
    mov rsi, rax
    call format_and_write

    ; Fabs of -5
    mov rax, -5
    cvtsi2sd xmm0, rax
    call fabs
    mov rdi, output_buffer
    mov rsi, fabs_fmt
    mov rax, 1
    call sprintf
    
    mov rdi, output_buffer
    call strlen_func
    mov rdi, output_buffer
    mov rsi, rax
    call sys_write

    ; Sin of 90
    mov rax, 90
    cvtsi2sd xmm0, rax
    call sin
    mov rdi, output_buffer
    mov rsi, sin_fmt
    mov rax, 1
    call sprintf
    
    mov rdi, output_buffer
    call strlen_func
    mov rdi, output_buffer
    mov rsi, rax
    call sys_write

    ; This will NOT crash in Assembly (atoi returns 0 for invalid input)
    mov rdi, prompthello2
    mov rsi, prompthello2_len
    call sys_write
    
    mov rdi, user_input
    mov rsi, 100
    call sys_read
    
    mov rdi, user_input
    call atoi
    mov rdi, int_fmt
    mov rsi, rax
    call format_and_write

    ; This will NOT crash in Assembly (atoi returns 0 for invalid input)
    mov rdi, promptabc2
    mov rsi, promptabc2_len
    call sys_write
    
    mov rdi, user_input
    mov rsi, 100
    call sys_read
    
    mov rdi, user_input
    call atoi
    mov rdi, int_fmt
    mov rsi, rax
    call format_and_write

    ; Exit program
    mov rax, 60
    mov rdi, 0
    syscall
