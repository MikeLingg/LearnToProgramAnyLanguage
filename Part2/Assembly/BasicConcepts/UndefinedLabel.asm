; UndefinedLabel.asm - Demonstrates the closest thing to "variable errors" in assembly
; This will cause assembler/linker errors - the assembly equivalent of undefined variables
; Assemble with: nasm -f elf64 UndefinedLabel.asm -o UndefinedLabel.o
; Link with: ld UndefinedLabel.o -o UndefinedLabel.exe

section .data
    ; Valid data declarations
    valid_message   db 'This message exists', 10, 0
    valid_number    dd 42
    
section .text
    global _start

_start:
    ; This will cause an assembler error - undefined label/symbol
    ; Similar to using an undeclared variable in high-level languages
    mov rax, 1
    mov rdi, 1
    mov rsi, undefined_message      ; ERROR: undefined_message not declared
    mov rdx, 20
    syscall
    
    ; This will also cause an error - undefined symbol
    mov eax, [undefined_variable]   ; ERROR: undefined_variable not declared
    
    ; This will cause an error - typo in label name (case sensitive)
    mov ebx, [Valid_Number]         ; ERROR: Valid_Number vs valid_number
    
    ; This would work if the above errors weren't present
    mov rax, 1
    mov rdi, 1
    mov rsi, valid_message
    mov rdx, 19
    syscall
    
    ; Trying to jump to undefined label
    jmp undefined_function          ; ERROR: undefined_function not declared
    
    ; This will cause an error - undefined external symbol
    call undefined_external_func    ; ERROR: external function not declared
    
    ; Exit (this line would never be reached due to errors above)
    mov rax, 60
    mov rdi, 0
    syscall

; Note: In assembly, you can also have:
; - Undefined labels (like undefined functions)
; - Undefined symbols (like undefined variables)  
; - Syntax errors (invalid instructions)
; - Wrong operand types (trying to move immediate to immediate)
; - But no "type checking" - mov rax, "string" is syntactically valid

; Examples of other assembly "errors":
; mov 5, 6              ; Can't move immediate to immediate  
; mov [rax], [rbx]      ; Can't move memory to memory directly
; add                   ; Missing operands
; xyz rax, rbx          ; Invalid instruction
