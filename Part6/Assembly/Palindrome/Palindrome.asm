; Palindrome.asm - NASM equivalent of GCC-compiled C++ palindrome checker
; This matches the structure and functionality of the GCC assembly output

section .data
    ; String constants
    radar_str db 'radar', 0
    true_str db 'true', 0
    false_str db 'false', 0
    palindrome_fmt db 'Input string is a palindrome: %s', 10, 0

section .bss
    ; Simulated std::string object for testString (32 bytes typical size)
    ; Structure: [data_ptr][length][capacity][local_buffer[16]]
    test_string resb 32
    
section .text
    global main
    extern printf
    extern malloc
    extern free
    extern strlen

; ============================================================================
; main function - int main()
; ============================================================================
main:
    ; Function prologue
    push rbp
    mov rbp, rsp
    push rbx                        ; Save callee-saved register
    sub rsp, 88                     ; Allocate 88 bytes for local variables
    
    ; Stack canary (security feature from GCC)
    mov rax, [fs:0x28]
    mov [rbp-24], rax
    xor eax, eax
    
    ; ========================================================================
    ; C++: string testString = "radar";
    ; Initialize string object on stack at rbp-64
    ; ========================================================================
    lea rax, [rbp-64]               ; Address of string object
    lea rsi, [radar_str]            ; Source string "radar"
    mov rdi, rax
    call string_construct           ; Construct string from C string
    
    ; ========================================================================
    ; C++: bool isAPalindrome = false;
    ; ========================================================================
    mov byte [rbp-82], 0            ; isAPalindrome = false
    
    ; ========================================================================
    ; C++: unsigned leftCharIndex = 0;
    ; ========================================================================
    mov dword [rbp-80], 0           ; leftCharIndex = 0
    
    ; ========================================================================
    ; C++: unsigned rightCharIndex = testString.length() - 1;
    ; ========================================================================
    lea rax, [rbp-64]
    mov rdi, rax
    call string_length              ; Get string length
    sub eax, 1                      ; length - 1
    mov dword [rbp-76], eax         ; rightCharIndex = length - 1
    
    ; ========================================================================
    ; C++: if ( testString[leftCharIndex] == testString[rightCharIndex] )
    ; First nested condition
    ; ========================================================================
    mov edx, [rbp-80]               ; leftCharIndex
    lea rax, [rbp-64]
    mov rsi, rdx
    mov rdi, rax
    call string_at                  ; testString[leftCharIndex]
    movzx ebx, byte [rax]           ; Load left character
    
    mov edx, [rbp-76]               ; rightCharIndex
    lea rax, [rbp-64]
    mov rsi, rdx
    mov rdi, rax
    call string_at                  ; testString[rightCharIndex]
    movzx eax, byte [rax]           ; Load right character
    
    cmp bl, al                      ; Compare characters
    jne .L7                         ; If not equal, skip nested blocks
    
    ; ========================================================================
    ; C++: leftCharIndex = leftCharIndex + 1;
    ; C++: rightCharIndex = rightCharIndex - 1;
    ; ========================================================================
    add dword [rbp-80], 1           ; leftCharIndex++
    sub dword [rbp-76], 1           ; rightCharIndex--
    
    ; ========================================================================
    ; C++: if ( testString[leftCharIndex] == testString[rightCharIndex] )
    ; Second nested condition
    ; ========================================================================
    mov edx, [rbp-80]
    lea rax, [rbp-64]
    mov rsi, rdx
    mov rdi, rax
    call string_at
    movzx ebx, byte [rax]
    
    mov edx, [rbp-76]
    lea rax, [rbp-64]
    mov rsi, rdx
    mov rdi, rax
    call string_at
    movzx eax, byte [rax]
    
    cmp bl, al
    jne .L7
    
    ; ========================================================================
    ; C++: leftCharIndex = leftCharIndex + 1;
    ; C++: rightCharIndex = rightCharIndex - 1;
    ; ========================================================================
    add dword [rbp-80], 1
    sub dword [rbp-76], 1
    
    ; ========================================================================
    ; C++: if ( testString[leftCharIndex] == testString[rightCharIndex] )
    ; Third nested condition
    ; ========================================================================
    mov edx, [rbp-80]
    lea rax, [rbp-64]
    mov rsi, rdx
    mov rdi, rax
    call string_at
    movzx ebx, byte [rax]
    
    mov edx, [rbp-76]
    lea rax, [rbp-64]
    mov rsi, rdx
    mov rdi, rax
    call string_at
    movzx eax, byte [rax]
    
    cmp bl, al
    jne .L7
    
    ; ========================================================================
    ; C++: isAPalindrome = true;
    ; ========================================================================
    mov byte [rbp-82], 1            ; isAPalindrome = true
    
.L7:
    ; ========================================================================
    ; C++: printf( "Input string is a palindrome: %s\n", 
    ;              isAPalindrome ? "true" : "false" );
    ; ========================================================================
    cmp byte [rbp-82], 0            ; Check isAPalindrome
    je .L8
    lea rax, [true_str]             ; Load "true"
    jmp .L9
.L8:
    lea rax, [false_str]            ; Load "false"
.L9:
    mov rsi, rax                    ; Second argument (true/false string)
    lea rax, [palindrome_fmt]
    mov rdi, rax                    ; First argument (format string)
    xor eax, eax                    ; No vector registers used
    call printf
    
    ; ========================================================================
    ; C++: testString = "radar";
    ; Reassign string to "radar" for second test
    ; ========================================================================
    lea rax, [rbp-64]
    lea rdx, [radar_str]
    mov rsi, rdx
    mov rdi, rax
    call string_assign              ; testString = "radar"
    
    ; ========================================================================
    ; C++: bool notAPalindrome = false;
    ; ========================================================================
    mov byte [rbp-81], 0            ; notAPalindrome = false
    
    ; ========================================================================
    ; C++: leftCharIndex = 0;
    ; ========================================================================
    mov dword [rbp-80], 0           ; leftCharIndex = 0
    
    ; ========================================================================
    ; C++: rightCharIndex = testString.length() - 1;
    ; ========================================================================
    lea rax, [rbp-64]
    mov rdi, rax
    call string_length
    sub eax, 1
    mov dword [rbp-76], eax         ; rightCharIndex = length - 1
    
    ; ========================================================================
    ; C++: if ( leftCharIndex < testString.length() )
    ; First guarded condition
    ; ========================================================================
    mov ebx, [rbp-80]               ; leftCharIndex
    lea rax, [rbp-64]
    mov rdi, rax
    call string_length              ; Get string length
    cmp rbx, rax                    ; leftCharIndex < length?
    jae .L10                        ; If not, skip block
    
    ; ========================================================================
    ; C++: if ( testString[leftCharIndex] != testString[rightCharIndex] )
    ; ========================================================================
    mov edx, [rbp-80]
    lea rax, [rbp-64]
    mov rsi, rdx
    mov rdi, rax
    call string_at
    movzx ebx, byte [rax]
    
    mov edx, [rbp-76]
    lea rax, [rbp-64]
    mov rsi, rdx
    mov rdi, rax
    call string_at
    movzx eax, byte [rax]
    
    cmp bl, al
    je .L10
    
    ; ========================================================================
    ; C++: notAPalindrome = true;
    ; ========================================================================
    mov byte [rbp-81], 1            ; notAPalindrome = true
    
.L10:
    ; ========================================================================
    ; C++: if ( leftCharIndex < testString.length() )
    ; Second guarded condition
    ; ========================================================================
    mov ebx, [rbp-80]
    lea rax, [rbp-64]
    mov rdi, rax
    call string_length
    cmp rbx, rax
    jae .L11
    
    ; ========================================================================
    ; C++: if ( notAPalindrome != true )
    ; ========================================================================
    movzx eax, byte [rbp-81]
    cmp eax, 1
    je .L11
    
    ; ========================================================================
    ; C++: leftCharIndex = leftCharIndex + 1;
    ; C++: rightCharIndex = rightCharIndex - 1;
    ; ========================================================================
    add dword [rbp-80], 1
    sub dword [rbp-76], 1
    
    ; ========================================================================
    ; C++: if ( testString[leftCharIndex] != testString[rightCharIndex] )
    ; ========================================================================
    mov edx, [rbp-80]
    lea rax, [rbp-64]
    mov rsi, rdx
    mov rdi, rax
    call string_at
    movzx ebx, byte [rax]
    
    mov edx, [rbp-76]
    lea rax, [rbp-64]
    mov rsi, rdx
    mov rdi, rax
    call string_at
    movzx eax, byte [rax]
    
    cmp bl, al
    je .L11
    
    ; ========================================================================
    ; C++: notAPalindrome = true;
    ; ========================================================================
    mov byte [rbp-81], 1
    
.L11:
    ; ========================================================================
    ; C++: if ( leftCharIndex < testString.length() )
    ; Third guarded condition
    ; ========================================================================
    mov ebx, [rbp-80]
    lea rax, [rbp-64]
    mov rdi, rax
    call string_length
    cmp rbx, rax
    jae .L12
    
    ; ========================================================================
    ; C++: if ( notAPalindrome != true )
    ; ========================================================================
    movzx eax, byte [rbp-81]
    cmp eax, 1
    je .L12
    
    ; ========================================================================
    ; C++: leftCharIndex = leftCharIndex + 1;
    ; C++: rightCharIndex = rightCharIndex - 1;
    ; ========================================================================
    add dword [rbp-80], 1
    sub dword [rbp-76], 1
    
    ; ========================================================================
    ; C++: if ( testString[leftCharIndex] != testString[rightCharIndex] )
    ; ========================================================================
    mov edx, [rbp-80]
    lea rax, [rbp-64]
    mov rsi, rdx
    mov rdi, rax
    call string_at
    movzx ebx, byte [rax]
    
    mov edx, [rbp-76]
    lea rax, [rbp-64]
    mov rsi, rdx
    mov rdi, rax
    call string_at
    movzx eax, byte [rax]
    
    cmp bl, al
    je .L12
    
    ; ========================================================================
    ; C++: notAPalindrome = true;
    ; ========================================================================
    mov byte [rbp-81], 1
    
.L12:
    ; ========================================================================
    ; C++: printf( "Input string is a palindrome: %s\n", 
    ;              (!notAPalindrome) ? "true" : "false" );
    ; ========================================================================
    cmp byte [rbp-81], 0            ; Check notAPalindrome
    je .L13
    lea rax, [false_str]            ; If true, print "false"
    jmp .L14
.L13:
    lea rax, [true_str]             ; If false, print "true"
.L14:
    mov rsi, rax
    lea rax, [palindrome_fmt]
    mov rdi, rax
    xor eax, eax
    call printf
    
    ; ========================================================================
    ; String destructor - clean up testString
    ; ========================================================================
    mov ebx, 0                      ; Return value = 0
    lea rax, [rbp-64]
    mov rdi, rax
    call string_destruct            ; Destroy string object
    
    ; ========================================================================
    ; C++: return 0;
    ; ========================================================================
    mov eax, ebx                    ; Return 0
    
    ; Check stack canary
    mov rdx, [rbp-24]
    sub rdx, [fs:0x28]
    jne .stack_chk_fail
    
    ; Function epilogue
    mov rbx, [rbp-8]
    leave
    ret

.stack_chk_fail:
    call __stack_chk_fail

; ============================================================================
; Helper function: string_construct
; Simulates std::string constructor from C string
; Parameters: rdi = string object, rsi = C string
; ============================================================================
string_construct:
    push rbp
    mov rbp, rsp
    push rbx
    sub rsp, 24
    
    mov [rbp-24], rdi               ; Save string object pointer
    mov [rbp-32], rsi               ; Save source string
    
    ; Get length of source string
    mov rdi, rsi
    call strlen
    mov rbx, rax                    ; rbx = length
    
    ; Set up string object (simplified - using small string optimization)
    mov rdi, [rbp-24]
    cmp rbx, 15                     ; If length <= 15, use local buffer
    jbe .use_local
    
    ; Allocate heap memory for longer strings
    mov rdi, rbx
    add rdi, 1                      ; +1 for null terminator
    call malloc
    mov rdi, [rbp-24]
    mov [rdi], rax                  ; Store data pointer
    jmp .copy_data
    
.use_local:
    ; Use local buffer (offset 16 in string object)
    add rdi, 16
    mov rax, [rbp-24]
    mov [rax], rdi                  ; Point to local buffer
    
.copy_data:
    ; Copy string data
    mov rdi, [rbp-24]
    mov rdi, [rdi]                  ; Get data pointer
    mov rsi, [rbp-32]               ; Source string
    mov rcx, rbx                    ; Length
    rep movsb                       ; Copy bytes
    mov byte [rdi], 0               ; Null terminate
    
    ; Set length
    mov rax, [rbp-24]
    mov [rax+8], rbx                ; Store length at offset 8
    
    add rsp, 24
    pop rbx
    pop rbp
    ret

; ============================================================================
; Helper function: string_length
; Simulates std::string::length()
; Parameters: rdi = string object
; Returns: rax = length
; ============================================================================
string_length:
    push rbp
    mov rbp, rsp
    
    mov rax, [rdi+8]                ; Length is at offset 8
    
    pop rbp
    ret

; ============================================================================
; Helper function: string_at
; Simulates std::string::operator[]
; Parameters: rdi = string object, rsi = index
; Returns: rax = pointer to character
; ============================================================================
string_at:
    push rbp
    mov rbp, rsp
    
    mov rax, [rdi]                  ; Get data pointer
    add rax, rsi                    ; Add index
    
    pop rbp
    ret

; ============================================================================
; Helper function: string_assign
; Simulates std::string::operator=(const char*)
; Parameters: rdi = string object, rsi = C string
; ============================================================================
string_assign:
    push rbp
    mov rbp, rsp
    push rbx
    sub rsp, 24
    
    mov [rbp-24], rdi
    mov [rbp-32], rsi
    
    ; Get new string length
    mov rdi, rsi
    call strlen
    mov rbx, rax
    
    ; For simplicity, just update if it fits in current allocation
    mov rdi, [rbp-24]
    mov rax, [rdi]                  ; Get data pointer
    mov rsi, [rbp-32]
    mov rcx, rbx
    mov rdi, rax
    rep movsb
    mov byte [rdi], 0
    
    ; Update length
    mov rax, [rbp-24]
    mov [rax+8], rbx
    
    add rsp, 24
    pop rbx
    pop rbp
    ret

; ============================================================================
; Helper function: string_destruct
; Simulates std::string destructor
; Parameters: rdi = string object
; ============================================================================
string_destruct:
    push rbp
    mov rbp, rsp
    push rbx
    
    mov rbx, rdi
    mov rax, [rbx]                  ; Get data pointer
    lea rdx, [rbx+16]               ; Local buffer address
    cmp rax, rdx                    ; Is it using heap?
    je .no_free                     ; If local buffer, skip free
    
    ; Free heap memory
    mov rdi, rax
    call free
    
.no_free:
    pop rbx
    pop rbp
    ret

; External symbols that would normally come from libstdc++
extern __stack_chk_fail
