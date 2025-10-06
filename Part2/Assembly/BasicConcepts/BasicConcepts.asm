; BasicConcepts.asm - Shows how data types are actually stored in memory
; Matches C++ output but uses write syscall for booleans
; Assemble with: nasm -f elf64 BasicConcepts.asm -o BasicConcepts.o
; Link with: gcc BasicConcepts.o -o BasicConcepts.exe -no-pie

extern printf

section .data
    ; Boolean values - stored as bytes (0 = false, 1 = true)
    falseBoolean   db 1        ; true stored as 1
    trueBoolean    db 0        ; false stored as 0
    
    ; 8-bit integers
    minSigned8     db -128
    maxSigned8     db 127
    minUnsigned8   db 0
    maxUnsigned8   db 255
    
    ; 16-bit integers
    minSigned16    dw -32768
    maxSigned16    dw 32767
    minUnsigned16  dw 0
    maxUnsigned16  dw 65535
    
    ; 32-bit integers
    minSigned32    dd -2147483648
    maxSigned32    dd 2147483647
    minUnsigned32  dd 0
    maxUnsigned32  dd 4294967295
    
    ; 64-bit integers
    minSigned64    dq -9223372036854775808
    maxSigned64    dq 9223372036854775807
    minUnsigned64  dq 0
    maxUnsigned64  dq 18446744073709551615
    
    ; Floats
    floatMax       dd 3.402823e38
    floatMin       dd 1.175494e-38
    zeroPointOne   dd 0.1
    zeroPointTwo   dd 0.2
    zeroPointThree dd 0.3
    
    ; Doubles
    doubleMax      dq 1.797693e308
    doubleMin      dq 2.225074e-308
    
    ; Characters
    charOne        db '1'
    charTab        db 9
    singleQuotes   db 39
    charNewline    db 10
    doubleQuotes   db 34
    
    ; Out of range values
    outOfRangeBoolean db 55
    outOfRangeShort   dw 100000
    outOfRangeChar    db 257
    
    ; Format strings
    bool_msg       db "Boolean range: ", 0
    newline_str    db 10, 0
    space_str      db " ", 0
    
    int8_signed    db "8 bit signed int range: %d %d", 10, 0
    int8_unsigned  db "8 bit unsigned int range: %u %u", 10, 0
    int16_signed   db "16 bit signed int range: %d %d", 10, 0
    int16_unsigned db "16 bit unsigned int range %u %u", 10, 0
    int32_signed   db "32 bit signed int range: %d %d", 10, 0
    int32_unsigned db "32 bit unsigned int range: %u %u", 10, 0
    
    note64         db "Note: LLONG_MIN is used instead of -9,223,372,036,854,775,808 as C++ will first try to set +9,223,372,036,854,775,808, which is out of range, before applying the -.", 10, 0
    int64_signed   db "64 bit signed int range: %lld %lld", 10, 0
    int64_unsigned db "64 bit unsigned int range: %llu %llu", 10, 0
    
    note_float     db "Note that scientific notation must be used to print such a small number.", 10, 0
    float_range    db "32 bit float: %e %f", 10, 0
    float_precision db "Floating point 0.1, 0.2, 0.3 -> %.17f and %.17f and %.17f", 10, 0
    double_range   db "64 bit float range: %e %f", 10, 0
    
    char_msg       db "Characters: %c%c%c%c%c", 10, 0
    char_as_int    db "charOne as an integer: %d", 10, 0
    
    out_bool_msg   db "Out of range Boolean: %d", 10, 0
    out_short_msg  db "Out of range value: %d", 10, 0
    
    note_float_max db "Note that adding a small amount to FLT_MAX or DBL_MAX is lost in the precision, so the value is added to itself to be out of range.", 10, 0
    out_float_msg  db "Out of range float and double: %f %f", 10, 0
    out_char_msg   db "Out of range char:%c:", 10, 0

section .bss
    outOfRangeFloat  resd 1
    outOfRangeDouble resq 1

section .text
    global main

main:
    push rbp
    mov rbp, rsp
    
    ; Print "Boolean range: " using printf
    mov rdi, bool_msg
    xor rax, rax
    call printf
    
    ; Print boolean values using write syscall
    movzx rax, byte [falseBoolean]
    add rax, '0'
    push rax
    mov rax, 1
    mov rdi, 1
    mov rsi, rsp
    mov rdx, 1
    syscall
    pop rax
    
    ; Print space
    mov rax, 1
    mov rdi, 1
    mov rsi, space_str
    mov rdx, 1
    syscall
    
    ; Print second boolean
    movzx rax, byte [trueBoolean]
    add rax, '0'
    push rax
    mov rax, 1
    mov rdi, 1
    mov rsi, rsp
    mov rdx, 1
    syscall
    pop rax
    
    ; Print newline
    mov rax, 1
    mov rdi, 1
    mov rsi, newline_str
    mov rdx, 1
    syscall
    
    ; 8-bit signed integers
    mov rdi, int8_signed
    movsx rsi, byte [minSigned8]
    movsx rdx, byte [maxSigned8]
    xor rax, rax
    call printf
    
    ; 8-bit unsigned integers
    mov rdi, int8_unsigned
    movzx rsi, byte [minUnsigned8]
    movzx rdx, byte [maxUnsigned8]
    xor rax, rax
    call printf
    
    ; 16-bit signed integers
    mov rdi, int16_signed
    movsx rsi, word [minSigned16]
    movsx rdx, word [maxSigned16]
    xor rax, rax
    call printf
    
    ; 16-bit unsigned integers
    mov rdi, int16_unsigned
    movzx rsi, word [minUnsigned16]
    movzx rdx, word [maxUnsigned16]
    xor rax, rax
    call printf
    
    ; 32-bit signed integers
    mov rdi, int32_signed
    movsxd rsi, dword [minSigned32]
    movsxd rdx, dword [maxSigned32]
    xor rax, rax
    call printf
    
    ; 32-bit unsigned integers
    mov rdi, int32_unsigned
    mov esi, [minUnsigned32]
    mov edx, [maxUnsigned32]
    xor rax, rax
    call printf
    
    ; 64-bit note
    mov rdi, note64
    xor rax, rax
    call printf
    
    ; 64-bit signed integers
    mov rdi, int64_signed
    mov rsi, [minSigned64]
    mov rdx, [maxSigned64]
    xor rax, rax
    call printf
    
    ; 64-bit unsigned integers
    mov rdi, int64_unsigned
    mov rsi, [minUnsigned64]
    mov rdx, [maxUnsigned64]
    xor rax, rax
    call printf
    
    ; Float note
    mov rdi, note_float
    xor rax, rax
    call printf
    
    ; Float range
    mov rdi, float_range
    movss xmm0, [floatMin]
    cvtss2sd xmm0, xmm0
    movss xmm1, [floatMax]
    cvtss2sd xmm1, xmm1
    mov rax, 2
    call printf
    
    ; Float precision
    mov rdi, float_precision
    movss xmm0, [zeroPointOne]
    cvtss2sd xmm0, xmm0
    movss xmm1, [zeroPointTwo]
    cvtss2sd xmm1, xmm1
    movss xmm2, [zeroPointThree]
    cvtss2sd xmm2, xmm2
    mov rax, 3
    call printf
    
    ; Double note
    mov rdi, note_float
    xor rax, rax
    call printf
    
    ; Double range
    mov rdi, double_range
    movsd xmm0, [doubleMin]
    movsd xmm1, [doubleMax]
    mov rax, 2
    call printf
    
    ; Characters
    mov rdi, char_msg
    movzx rsi, byte [charOne]
    movzx rdx, byte [charTab]
    movzx rcx, byte [singleQuotes]
    movzx r8, byte [charNewline]
    movzx r9, byte [doubleQuotes]
    xor rax, rax
    call printf
    
    ; Character as integer
    mov rdi, char_as_int
    movzx rsi, byte [charOne]
    xor rax, rax
    call printf
    
    ; Out of range boolean
    mov rdi, out_bool_msg
    movzx rsi, byte [outOfRangeBoolean]
    xor rax, rax
    call printf
    
    ; Out of range short
    mov rdi, out_short_msg
    movsx rsi, word [outOfRangeShort]
    xor rax, rax
    call printf
    
    ; Float overflow note
    mov rdi, note_float_max
    xor rax, rax
    call printf
    
    ; Calculate out of range float (FLT_MAX + FLT_MAX)
    movss xmm0, [floatMax]
    addss xmm0, [floatMax]
    movss [outOfRangeFloat], xmm0
    
    ; Calculate out of range double (DBL_MAX + DBL_MAX)
    movsd xmm0, [doubleMax]
    addsd xmm0, [doubleMax]
    movsd [outOfRangeDouble], xmm0
    
    ; Print out of range float and double
    mov rdi, out_float_msg
    movss xmm0, [outOfRangeFloat]
    cvtss2sd xmm0, xmm0
    movsd xmm1, [outOfRangeDouble]
    mov rax, 2
    call printf
    
    ; Out of range char
    mov rdi, out_char_msg
    movzx rsi, byte [outOfRangeChar]
    xor rax, rax
    call printf
    
    ; Exit
    mov rsp, rbp
    pop rbp
    xor rax, rax
    ret
