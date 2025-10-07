; BasicConcepts.asm - Shows how data types are actually stored in memory
; Mimics C++ closely by allocating variables progressively as they're declared
; Each allocation is rounded up to 16 bytes to maintain stack alignment
; NOTE: This wastes space but keeps stack aligned for function calls
; Assemble with: nasm -f elf64 -g -F dwarf BasicConcepts.asm -o BasicConcepts.o
; Link with: gcc -g BasicConcepts.o -o BasicConcepts.exe -no-pie

extern printf

section .data
    ; Format strings (these ARE global constants, like string literals in C++)
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
    
    ; Float constants (literal values used in initialization)
    const_floatMax       dd 3.402823e38
    const_floatMin       dd 1.175494e-38
    const_zeroPointOne   dd 0.1
    const_zeroPointTwo   dd 0.2
    const_zeroPointThree dd 0.3
    const_doubleMax      dq 1.797693e308
    const_doubleMin      dq 2.225074e-308

section .text
    global main

main:
    push rbp
    mov rbp, rsp
    
    ; Ensure initial alignment
    and rsp, -16                ; Align to 16 bytes
    
    ; ===== Boolean variables =====
    ; bool falseBoolean = true;
    sub rsp, 16                 ; Allocate 16 bytes (round up from 1)
    mov byte [rsp], 1           ; falseBoolean = true (1)
    
    ; bool trueBoolean = false;
    sub rsp, 16                 ; Allocate 16 bytes (round up from 1)
    mov byte [rsp], 0           ; trueBoolean = false (0)
    
    ; printf("Boolean range: %d %d\n", falseBoolean, trueBoolean);
    ; Using write syscall instead for demonstration
    mov rdi, bool_msg
    xor rax, rax
    call printf
    
    ; Print falseBoolean using write syscall
    movzx rax, byte [rsp+16]    ; Load falseBoolean (16 bytes up)
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
    
    ; Print trueBoolean using write syscall
    movzx rax, byte [rsp]       ; Load trueBoolean (at current rsp)
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
    
    ; ===== 8-bit signed integers =====
    ; signed char minSigned8 = -128;
    sub rsp, 16                 ; Allocate 16 bytes (round up from 1)
    mov byte [rsp], -128        ; minSigned8 = -128
    
    ; signed char maxSigned8 = 127;
    sub rsp, 16                 ; Allocate 16 bytes (round up from 1)
    mov byte [rsp], 127         ; maxSigned8 = 127
    
    ; printf("8 bit signed int range: %d %d\n", minSigned8, maxSigned8);
    mov rdi, int8_signed
    movsx rsi, byte [rsp+16]    ; minSigned8
    movsx rdx, byte [rsp]       ; maxSigned8
    xor rax, rax
    call printf
    
    ; ===== 8-bit unsigned integers =====
    ; unsigned char minUnsigned8 = 0;
    sub rsp, 16                 ; Allocate 16 bytes (round up from 1)
    mov byte [rsp], 0           ; minUnsigned8 = 0
    
    ; unsigned char maxUnsigned8 = 255;
    sub rsp, 16                 ; Allocate 16 bytes (round up from 1)
    mov byte [rsp], 255         ; maxUnsigned8 = 255
    
    ; printf("8 bit unsigned int range: %u %u\n", minUnsigned8, maxUnsigned8);
    mov rdi, int8_unsigned
    movzx rsi, byte [rsp+16]    ; minUnsigned8
    movzx rdx, byte [rsp]       ; maxUnsigned8
    xor rax, rax
    call printf
    
    ; ===== 16-bit signed integers =====
    ; short minSigned16 = -32768;
    sub rsp, 16                 ; Allocate 16 bytes (round up from 2)
    mov word [rsp], -32768      ; minSigned16 = -32768
    
    ; short maxSigned16 = 32767;
    sub rsp, 16                 ; Allocate 16 bytes (round up from 2)
    mov word [rsp], 32767       ; maxSigned16 = 32767
    
    ; printf("16 bit signed int range: %d %d\n", minSigned16, maxSigned16);
    mov rdi, int16_signed
    movsx rsi, word [rsp+16]    ; minSigned16
    movsx rdx, word [rsp]       ; maxSigned16
    xor rax, rax
    call printf
    
    ; ===== 16-bit unsigned integers =====
    ; unsigned short minUnsigned16 = 0;
    sub rsp, 16                 ; Allocate 16 bytes (round up from 2)
    mov word [rsp], 0           ; minUnsigned16 = 0
    
    ; unsigned short maxUnsigned16 = 65535;
    sub rsp, 16                 ; Allocate 16 bytes (round up from 2)
    mov word [rsp], 65535       ; maxUnsigned16 = 65535
    
    ; printf("16 bit unsigned int range %u %u\n", minUnsigned16, maxUnsigned16);
    mov rdi, int16_unsigned
    movzx rsi, word [rsp+16]    ; minUnsigned16
    movzx rdx, word [rsp]       ; maxUnsigned16
    xor rax, rax
    call printf
    
    ; ===== 32-bit signed integers =====
    ; int minSigned32 = -2147483648;
    sub rsp, 16                 ; Allocate 16 bytes (round up from 4)
    mov dword [rsp], -2147483648 ; minSigned32 = -2147483648
    
    ; int maxSigned32 = 2147483647;
    sub rsp, 16                 ; Allocate 16 bytes (round up from 4)
    mov dword [rsp], 2147483647  ; maxSigned32 = 2147483647
    
    ; printf("32 bit signed int range: %d %d\n", minSigned32, maxSigned32);
    mov rdi, int32_signed
    movsxd rsi, dword [rsp+16]  ; minSigned32
    movsxd rdx, dword [rsp]     ; maxSigned32
    xor rax, rax
    call printf
    
    ; ===== 32-bit unsigned integers =====
    ; unsigned int minUnsigned32 = 0;
    sub rsp, 16                 ; Allocate 16 bytes (round up from 4)
    mov dword [rsp], 0          ; minUnsigned32 = 0
    
    ; unsigned int maxUnsigned32 = 4294967295;
    sub rsp, 16                 ; Allocate 16 bytes (round up from 4)
    mov dword [rsp], 4294967295 ; maxUnsigned32 = 4294967295
    
    ; printf("32 bit unsigned int range: %u %u\n", minUnsigned32, maxUnsigned32);
    mov rdi, int32_unsigned
    mov esi, [rsp+16]           ; minUnsigned32
    mov edx, [rsp]              ; maxUnsigned32
    xor rax, rax
    call printf
    
    ; printf("Note: LLONG_MIN is used instead of...");
    mov rdi, note64
    xor rax, rax
    call printf
    
    ; ===== 64-bit signed integers =====
    ; long long minSigned64 = LLONG_MIN;
    sub rsp, 16                 ; Allocate 16 bytes (round up from 8)
    mov rax, -9223372036854775808
    mov qword [rsp], rax        ; minSigned64 = -9223372036854775808
    
    ; long long maxSigned64 = 9223372036854775807;
    sub rsp, 16                 ; Allocate 16 bytes (round up from 8)
    mov rax, 9223372036854775807
    mov qword [rsp], rax        ; maxSigned64 = 9223372036854775807
    
    ; printf("64 bit signed int range: %lld %lld\n", minSigned64, maxSigned64);
    mov rdi, int64_signed
    mov rsi, [rsp+16]           ; minSigned64
    mov rdx, [rsp]              ; maxSigned64
    xor rax, rax
    call printf
    
    ; ===== 64-bit unsigned integers =====
    ; unsigned long long minUnsigned64 = 0ULL;
    sub rsp, 16                 ; Allocate 16 bytes (round up from 8)
    mov qword [rsp], 0          ; minUnsigned64 = 0
    
    ; unsigned long long maxUnsigned64 = 18446744073709551615ULL;
    sub rsp, 16                 ; Allocate 16 bytes (round up from 8)
    mov rax, 18446744073709551615
    mov qword [rsp], rax        ; maxUnsigned64 = 18446744073709551615
    
    ; printf("64 bit unsigned int range: %llu %llu\n", minUnsigned64, maxUnsigned64);
    mov rdi, int64_unsigned
    mov rsi, [rsp+16]           ; minUnsigned64
    mov rdx, [rsp]              ; maxUnsigned64
    xor rax, rax
    call printf
    
    ; ===== Float variables =====
    ; float floatMax = FLT_MAX;
    sub rsp, 16                 ; Allocate 16 bytes (round up from 4)
    movss xmm0, [const_floatMax]
    movss [rsp], xmm0           ; floatMax = FLT_MAX
    
    ; float floatMin = FLT_MIN;
    sub rsp, 16                 ; Allocate 16 bytes (round up from 4)
    movss xmm0, [const_floatMin]
    movss [rsp], xmm0           ; floatMin = FLT_MIN
    
    ; printf("Note that scientific notation...");
    mov rdi, note_float
    xor rax, rax
    call printf
    
    ; printf("32 bit float: %e %f\n", floatMin, floatMax);
    mov rdi, float_range
    movss xmm0, [rsp]           ; floatMin
    cvtss2sd xmm0, xmm0
    movss xmm1, [rsp+16]        ; floatMax
    cvtss2sd xmm1, xmm1
    mov rax, 2
    call printf
    
    ; float zeroPointOne = 0.1f;
    sub rsp, 16                 ; Allocate 16 bytes (round up from 4)
    movss xmm0, [const_zeroPointOne]
    movss [rsp], xmm0           ; zeroPointOne = 0.1f
    
    ; float zeroPointTwo = 0.2f;
    sub rsp, 16                 ; Allocate 16 bytes (round up from 4)
    movss xmm0, [const_zeroPointTwo]
    movss [rsp], xmm0           ; zeroPointTwo = 0.2f
    
    ; float zeroPointThree = 0.3f;
    sub rsp, 16                 ; Allocate 16 bytes (round up from 4)
    movss xmm0, [const_zeroPointThree]
    movss [rsp], xmm0           ; zeroPointThree = 0.3f
    
    ; printf("Floating point 0.1, 0.2, 0.3 -> %.17f and %.17f and %.17f\n", ...);
    mov rdi, float_precision
    movss xmm0, [rsp+32]        ; zeroPointOne
    cvtss2sd xmm0, xmm0
    movss xmm1, [rsp+16]        ; zeroPointTwo
    cvtss2sd xmm1, xmm1
    movss xmm2, [rsp]           ; zeroPointThree
    cvtss2sd xmm2, xmm2
    mov rax, 3
    call printf
    
    ; ===== Double variables =====
    ; double doubleMax = DBL_MAX;
    sub rsp, 16                 ; Allocate 16 bytes (round up from 8)
    movsd xmm0, [const_doubleMax]
    movsd [rsp], xmm0           ; doubleMax = DBL_MAX
    
    ; double doubleMin = DBL_MIN;
    sub rsp, 16                 ; Allocate 16 bytes (round up from 8)
    movsd xmm0, [const_doubleMin]
    movsd [rsp], xmm0           ; doubleMin = DBL_MIN
    
    ; printf("Note that scientific notation...");
    mov rdi, note_float
    xor rax, rax
    call printf
    
    ; printf("64 bit float range: %e %f\n", doubleMin, doubleMax);
    mov rdi, double_range
    movsd xmm0, [rsp]           ; doubleMin
    movsd xmm1, [rsp+16]        ; doubleMax
    mov rax, 2
    call printf
    
    ; ===== Character variables =====
    ; char charOne = '1';
    sub rsp, 16                 ; Allocate 16 bytes (round up from 1)
    mov byte [rsp], '1'         ; charOne = '1'
    
    ; char charTab = '\t';
    sub rsp, 16                 ; Allocate 16 bytes (round up from 1)
    mov byte [rsp], 9           ; charTab = '\t' (9)
    
    ; char singleQuotes = '\'';
    sub rsp, 16                 ; Allocate 16 bytes (round up from 1)
    mov byte [rsp], 39          ; singleQuotes = '\'' (39)
    
    ; char charNewLine = '\n';
    sub rsp, 16                 ; Allocate 16 bytes (round up from 1)
    mov byte [rsp], 10          ; charNewLine = '\n' (10)
    
    ; char doubleQuotes = '\"';
    sub rsp, 16                 ; Allocate 16 bytes (round up from 1)
    mov byte [rsp], 34          ; doubleQuotes = '\"' (34)
    
    ; printf("Characters: %c%c%c%c%c\n", charOne, charTab, singleQuotes, charNewLine, doubleQuotes);
    mov rdi, char_msg
    movzx rsi, byte [rsp+64]    ; charOne (4*16 bytes up)
    movzx rdx, byte [rsp+48]    ; charTab (3*16 bytes up)
    movzx rcx, byte [rsp+32]    ; singleQuotes (2*16 bytes up)
    movzx r8, byte [rsp+16]     ; charNewLine (1*16 bytes up)
    movzx r9, byte [rsp]        ; doubleQuotes (at current rsp)
    xor rax, rax
    call printf
    
    ; printf("charOne as an integer: %d\n", charOne);
    mov rdi, char_as_int
    movzx rsi, byte [rsp+64]    ; charOne
    xor rax, rax
    call printf
    
    ; ===== Out of range boolean =====
    ; bool outOfRangeBoolean = 55;
    sub rsp, 16                 ; Allocate 16 bytes (round up from 1)
    mov byte [rsp], 55          ; outOfRangeBoolean = 55
    
    ; printf("Out of range Boolean: %d\n", outOfRangeBoolean);
    mov rdi, out_bool_msg
    movzx rsi, byte [rsp]       ; outOfRangeBoolean
    xor rax, rax
    call printf
    
    ; ===== Out of range short =====
    ; short outOfRange = 100000;
    sub rsp, 16                 ; Allocate 16 bytes (round up from 2)
    mov word [rsp], 100000      ; outOfRange = 100000 (wraps around)
    
    ; printf("Out of range value: %d\n", outOfRange);
    mov rdi, out_short_msg
    movsx rsi, word [rsp]       ; outOfRange
    xor rax, rax
    call printf
    
    ; printf("Note that adding a small amount...");
    mov rdi, note_float_max
    xor rax, rax
    call printf
    
    ; ===== Out of range float and double =====
    ; float outOfRangeFloat = FLT_MAX + FLT_MAX;
    sub rsp, 16                 ; Allocate 16 bytes (round up from 4)
    movss xmm0, [const_floatMax]
    addss xmm0, [const_floatMax]
    movss [rsp], xmm0           ; outOfRangeFloat = FLT_MAX + FLT_MAX (infinity)
    
    ; double outOfRangeDouble = DBL_MAX + DBL_MAX;
    sub rsp, 16                 ; Allocate 16 bytes (round up from 8)
    movsd xmm0, [const_doubleMax]
    addsd xmm0, [const_doubleMax]
    movsd [rsp], xmm0           ; outOfRangeDouble = DBL_MAX + DBL_MAX (infinity)
    
    ; printf("Out of range float and double: %f %f\n", outOfRangeFloat, outOfRangeDouble);
    mov rdi, out_float_msg
    movss xmm0, [rsp+16]        ; outOfRangeFloat
    cvtss2sd xmm0, xmm0
    movsd xmm1, [rsp]           ; outOfRangeDouble
    mov rax, 2
    call printf
    
    ; ===== Out of range char =====
    ; char outOfRangeChar = 257;
    sub rsp, 16                 ; Allocate 16 bytes (round up from 1)
    mov byte [rsp], 257         ; outOfRangeChar = 257 (wraps to 1)
    
    ; printf("Out of range char:%c\n", outOfRangeChar);
    mov rdi, out_char_msg
    movzx rsi, byte [rsp]       ; outOfRangeChar
    xor rax, rax
    call printf
    
    ; Exit - restore stack and return
    mov rsp, rbp
    pop rbp
    xor rax, rax
    ret
