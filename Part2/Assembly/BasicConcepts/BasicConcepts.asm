; BasicConcepts.asm - Shows how data types are actually stored in memory
; Assemble with: nasm -f elf64 BasicConcepts.asm -o BasicConcepts.o
; Link with: ld BasicConcepts.o -o BasicConcepts.exe

section .data
    ; Boolean values - stored as bytes (0 = false, 1 = true)
    false_boolean   db 1        ; true stored as 1
    true_boolean    db 0        ; false stored as 0
    
    ; 8-bit integers - signed and unsigned are just interpretations
    min_signed8     db -128     ; 0x80 in memory
    max_signed8     db 127      ; 0x7F in memory
    max_unsigned8   db 255      ; 0xFF in memory
    
    ; 16-bit integers - little endian storage
    min_signed16    dw -32768   ; stored in little endian
    max_signed16    dw 32767
    max_unsigned16  dw 65535
    
    ; 32-bit integers
    min_signed32    dd -2147483648
    max_signed32    dd 2147483647
    max_unsigned32  dd 4294967295
    
    ; 64-bit integers
    min_signed64    dq -9223372036854775808
    max_signed64    dq 9223372036854775807
    
    ; 32-bit IEEE 754 floating point
    float_small     dd 0.1      ; Shows floating point precision issues
    float_max       dd 3.4e38   ; Large float value
    
    ; 64-bit IEEE 754 floating point
    double_small    dq 0.1      ; Double precision
    double_max      dq 1.7e308  ; Large double value
    
    ; Characters - stored as ASCII bytes
    char_one        db '1'          ; 0x31 in memory
    char_tab        db 9            ; Tab character (0x09)
    char_two        db '2'          ; 0x32 in memory
    char_newline    db 10           ; Newline character (0x0A)
    char_three      db '3'          ; 0x33 in memory
    
    ; Out of range demonstration
    out_of_range_char db 257    ; Only lower 8 bits stored (becomes 1)
    
    ; String messages
    memory_msg      db 'In assembly, everything is just bits in memory!', 10
    memory_msg_len  equ $ - memory_msg
    
    bool_msg        db 'Boolean range (as bytes): '
    bool_msg_len    equ $ - bool_msg
    
    int8_msg        db '8-bit signed range: -128 to 127, unsigned: 0 to 255', 10
    int8_msg_len    equ $ - int8_msg
    
    int16_msg       db '16-bit signed range: -32768 to 32767, unsigned: 0 to 65535', 10
    int16_msg_len   equ $ - int16_msg
    
    int32_msg       db '32-bit signed range: -2147483648 to 2147483647', 10
    int32_msg_len   equ $ - int32_msg
    
    int64_msg       db '64-bit signed range: -9223372036854775808 to 9223372036854775807', 10
    int64_msg_len   equ $ - int64_msg
    
    float_msg       db '32-bit float: IEEE 754 format, ~7 decimal digits precision', 10
    float_msg_len   equ $ - float_msg
    
    double_msg      db '64-bit double: IEEE 754 format, ~15 decimal digits precision', 10
    double_msg_len  equ $ - double_msg
    
    char_msg        db 'Characters (ASCII): '
    char_msg_len    equ $ - char_msg
    
    ranges_msg      db 'Memory storage demonstrates actual bit patterns:', 10
    ranges_msg_len  equ $ - ranges_msg
    
    out_range_msg   db 'Out of range char 257 becomes: '
    out_range_msg_len equ $ - out_range_msg
    
    newline         db 10
    space           db ' '

section .text
    global _start

_start:
    ; Print main explanation
    mov rax, 1
    mov rdi, 1
    mov rsi, memory_msg
    mov rdx, memory_msg_len
    syscall
    
    ; Print ranges explanation
    mov rax, 1
    mov rdi, 1
    mov rsi, ranges_msg
    mov rdx, ranges_msg_len
    syscall
    
    ; Print boolean message and values
    mov rax, 1
    mov rdi, 1
    mov rsi, bool_msg
    mov rdx, bool_msg_len
    syscall
    
    ; Print boolean values
    movzx rax, byte [false_boolean]
    add rax, '0'
    push rax
    mov rax, 1
    mov rdi, 1
    mov rsi, rsp
    mov rdx, 1
    syscall
    pop rax
    
    mov rax, 1
    mov rdi, 1
    mov rsi, space
    mov rdx, 1
    syscall
    
    movzx rax, byte [true_boolean]
    add rax, '0'
    push rax
    mov rax, 1
    mov rdi, 1
    mov rsi, rsp
    mov rdx, 1
    syscall
    pop rax
    
    mov rax, 1
    mov rdi, 1
    mov rsi, newline
    mov rdx, 1
    syscall
    
    ; Print integer ranges
    mov rax, 1
    mov rdi, 1
    mov rsi, int8_msg
    mov rdx, int8_msg_len
    syscall
    
    mov rax, 1
    mov rdi, 1
    mov rsi, int16_msg
    mov rdx, int16_msg_len
    syscall
    
    mov rax, 1
    mov rdi, 1
    mov rsi, int32_msg
    mov rdx, int32_msg_len
    syscall
    
    mov rax, 1
    mov rdi, 1
    mov rsi, int64_msg
    mov rdx, int64_msg_len
    syscall
    
    ; Print float information
    mov rax, 1
    mov rdi, 1
    mov rsi, float_msg
    mov rdx, float_msg_len
    syscall
    
    mov rax, 1
    mov rdi, 1
    mov rsi, double_msg
    mov rdx, double_msg_len
    syscall
    
    ; Print characters demonstration
    mov rax, 1
    mov rdi, 1
    mov rsi, char_msg
    mov rdx, char_msg_len
    syscall
    
    ; Print character sequence
    mov rax, 1
    mov rdi, 1
    mov rsi, char_one
    mov rdx, 1
    syscall
    
    mov rax, 1
    mov rdi, 1
    mov rsi, char_tab
    mov rdx, 1
    syscall
    
    mov rax, 1
    mov rdi, 1
    mov rsi, char_two
    mov rdx, 1
    syscall
    
    mov rax, 1
    mov rdi, 1
    mov rsi, char_newline
    mov rdx, 1
    syscall
    
    mov rax, 1
    mov rdi, 1
    mov rsi, char_three
    mov rdx, 1
    syscall
    
    mov rax, 1
    mov rdi, 1
    mov rsi, newline
    mov rdx, 1
    syscall
    
    ; Demonstrate out of range
    mov rax, 1
    mov rdi, 1
    mov rsi, out_range_msg
    mov rdx, out_range_msg_len
    syscall
    
    ; Show that 257 becomes 1 in a byte
    movzx rax, byte [out_of_range_char]
    add rax, '0'
    push rax
    mov rax, 1
    mov rdi, 1
    mov rsi, rsp
    mov rdx, 1
    syscall
    pop rax
    
    mov rax, 1
    mov rdi, 1
    mov rsi, newline
    mov rdx, 1
    syscall
    
    ; Exit program
    mov rax, 60
    mov rdi, 0
    syscall
