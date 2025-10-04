; BasicConcepts.asm - Shows how data types are actually stored in memory
; Assemble with: nasm -f elf64 BasicConcepts.asm -o BasicConcepts.o
; Link with: ld BasicConcepts.o -o BasicConcepts.exe

section .data
    ; Boolean values - stored as bytes (0 = false, 1 = true)
    falseBoolean   db 1        ; true stored as 1
    trueBoolean    db 0        ; false stored as 0
    
    ; 8-bit integers - signed and unsigned are just interpretations
    minSigned8     db -128     ; 0x80 in memory
    maxSigned8     db 127      ; 0x7F in memory
    maxUnsigned8   db 255      ; 0xFF in memory
    
    ; 16-bit integers - little endian storage
    minSigned16    dw -32768   ; stored in little endian
    maxSigned16    dw 32767
    maxUnsigned16  dw 65535
    
    ; 32-bit integers
    minSigned32    dd -2147483648
    maxSigned32    dd 2147483647
    maxUnsigned32  dd 4294967295
    
    ; 64-bit integers
    minSigned64    dq -9223372036854775808
    maxSigned64    dq 9223372036854775807
    
    ; 32-bit IEEE 754 floating point
    floatSmall     dd 0.1      ; Shows floating point precision issues
    floatMax       dd 3.4e38   ; Large float value
    
    ; 64-bit IEEE 754 floating point
    doubleSmall    dq 0.1      ; Double precision
    doubleMax      dq 1.7e308  ; Large double value
    
    ; Characters - stored as ASCII bytes
    charOne        db '1'          ; 0x31 in memory
    charTab        db 9            ; Tab character (0x09)
    singleQuotes   db 39           ; Single Quotes
    charNewline    db 10           ; Newline character (0x0A)
    doubleQuotes   db 34           ; Double Quotes
    
    ; Out of range demonstration
    outOfRangeChar db 257    ; Only lower 8 bits stored (becomes 1)
    
    ; String messages
    memoryMsg      db 'In assembly, everything is just bits in memory!', 10
    memoryMsgLen  equ $ - memoryMsg
    
    boolMsg        db 'Boolean range (as bytes): '
    boolMsgLen    equ $ - boolMsg
    
    int8Msg        db '8-bit signed range: -128 to 127, unsigned: 0 to 255', 10
    int8MsgLen    equ $ - int8Msg
    
    int16Msg       db '16-bit signed range: -32768 to 32767, unsigned: 0 to 65535', 10
    int16MsgLen   equ $ - int16Msg
    
    int32Msg       db '32-bit signed range: -2147483648 to 2147483647', 10
    int32MsgLen   equ $ - int32Msg
    
    int64Msg       db '64-bit signed range: -9223372036854775808 to 9223372036854775807', 10
    int64MsgLen   equ $ - int64Msg
    
    floatMsg       db '32-bit float: IEEE 754 format, ~7 decimal digits precision', 10
    floatMsgLen   equ $ - floatMsg
    
    doubleMsg      db '64-bit double: IEEE 754 format, ~15 decimal digits precision', 10
    doubleMsgLen  equ $ - doubleMsg
    
    charMsg        db 'Characters (ASCII): '
    charMsgLen    equ $ - charMsg
    
    rangesMsg      db 'Memory storage demonstrates actual bit patterns:', 10
    rangesMsgLen  equ $ - rangesMsg
    
    outRangeMsg   db 'Out of range char 257 becomes: '
    outRangeMsgLen equ $ - outRangeMsg
    
    newline         db 10
    space           db ' '

section .text
    global _start

_start:
    ; Print main explanation
    mov rax, 1
    mov rdi, 1
    mov rsi, memoryMsg
    mov rdx, memoryMsgLen
    syscall
    
    ; Print ranges explanation
    mov rax, 1
    mov rdi, 1
    mov rsi, rangesMsg
    mov rdx, rangesMsgLen
    syscall
    
    ; Print boolean message and values
    mov rax, 1
    mov rdi, 1
    mov rsi, boolMsg
    mov rdx, boolMsgLen
    syscall
    
    ; Print boolean values
    movzx rax, byte [falseBoolean]
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
    
    movzx rax, byte [trueBoolean]
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
    mov rsi, int8Msg
    mov rdx, int8MsgLen
    syscall
    
    mov rax, 1
    mov rdi, 1
    mov rsi, int16Msg
    mov rdx, int16MsgLen
    syscall
    
    mov rax, 1
    mov rdi, 1
    mov rsi, int32Msg
    mov rdx, int32MsgLen
    syscall
    
    mov rax, 1
    mov rdi, 1
    mov rsi, int64Msg
    mov rdx, int64MsgLen
    syscall
    
    ; Print float information
    mov rax, 1
    mov rdi, 1
    mov rsi, floatMsg
    mov rdx, floatMsgLen
    syscall
    
    mov rax, 1
    mov rdi, 1
    mov rsi, doubleMsg
    mov rdx, doubleMsgLen
    syscall
    
    ; Print characters demonstration
    mov rax, 1
    mov rdi, 1
    mov rsi, charMsg
    mov rdx, charMsgLen
    syscall
    
    ; Print character sequence
    mov rax, 1
    mov rdi, 1
    mov rsi, charOne
    mov rdx, 1
    syscall
    
    mov rax, 1
    mov rdi, 1
    mov rsi, charTab
    mov rdx, 1
    syscall
    
    mov rax, 1
    mov rdi, 1
    mov rsi, singleQuotes
    mov rdx, 1
    syscall
    
    mov rax, 1
    mov rdi, 1
    mov rsi, charNewline
    mov rdx, 1
    syscall
    
    mov rax, 1
    mov rdi, 1
    mov rsi, doubleQuotes
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
    mov rsi, outRangeMsg
    mov rdx, outRangeMsgLen
    syscall
    
    ; Show that 257 becomes 1 in a byte
    movzx rax, byte [outOfRangeChar]
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
