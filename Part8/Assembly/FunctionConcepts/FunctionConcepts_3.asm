; Function returning a value (getPizzaCost)
; Demonstrates return values via rax register
section .data
    small_str db "small", 0
    medium_str db "medium", 0
    
    ; Price constants (in cents)
    small_price equ 1299
    medium_price equ 1599
    large_price equ 1899
    topping_price equ 150
    delivery_fee equ 300
    
    result_msg db "Total pizza price is: $", 0
    newline db 10, 0

section .bss
    buffer resb 32

section .text
    global _start

; Function to convert integer (cents) to dollar string
; rdi = cents, rsi = buffer
cents_to_string:
    push rbp
    mov rbp, rsp
    
    mov rax, rdi
    mov rbx, 100
    xor rdx, rdx
    div rbx         ; rax = dollars, rdx = cents
    
    push rdx        ; save cents
    
    ; Convert dollars
    mov rbx, 10
    mov rcx, 0
    
.dollar_loop:
    xor rdx, rdx
    div rbx
    add dl, '0'
    push rdx
    inc rcx
    test rax, rax
    jnz .dollar_loop
    
    ; Write dollars
.pop_dollars:
    pop rax
    mov [rsi], al
    inc rsi
    dec rcx
    jnz .pop_dollars
    
    ; Add decimal point
    mov byte [rsi], '.'
    inc rsi
    
    ; Add cents
    pop rax         ; restore cents
    mov rbx, 10
    xor rdx, rdx
    div rbx
    add al, '0'
    mov [rsi], al
    inc rsi
    add dl, '0'
    mov [rsi], dl
    inc rsi
    
    mov byte [rsi], 0   ; null terminate
    
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

; Function: getPizzaCost
; Parameters: rdi = size string, rsi = topping count, rdx = delivery flag
; Returns: rax = total cost in cents
getPizzaCost:
    push rbp
    mov rbp, rsp
    push rsi        ; save topping count
    push rdx        ; save delivery flag
    
    ; Determine base price
    call strcmp_small
    test rax, rax
    jz .small_pizza
    
    mov rdi, [rbp+24]   ; restore original rdi from stack
    call strcmp_medium
    test rax, rax
    jz .medium_pizza
    
    ; Default to large
    mov rax, large_price
    jmp .calculate_toppings
    
.small_pizza:
    mov rax, small_price
    jmp .calculate_toppings
    
.medium_pizza:
    mov rax, medium_price
    
.calculate_toppings:
    ; rax has base price
    ; Calculate topping cost
    mov rbx, [rbp-8]    ; topping count from stack
    imul rbx, topping_price
    add rax, rbx
    
    ; Check delivery
    cmp qword [rbp-16], 0   ; delivery flag from stack
    je .no_delivery
    add rax, delivery_fee
    
.no_delivery:
    ; rax contains total price - this is the return value
    pop rdx
    pop rsi
    pop rbp
    ret

strcmp_small:
    push rsi
    mov rsi, small_str
    call strcmp
    pop rsi
    ret
    
strcmp_medium:
    push rsi
    mov rsi, medium_str
    call strcmp
    pop rsi
    ret

strcmp:
    ; Compare strings in rdi and rsi
.loop:
    mov al, [rdi]
    mov bl, [rsi]
    cmp al, bl
    jne .not_equal
    test al, al
    jz .equal
    inc rdi
    inc rsi
    jmp .loop
.equal:
    mov rax, 0
    ret
.not_equal:
    mov rax, 1
    ret

_start:
    ; Call getPizzaCost("small", 2, 1)
    mov rdi, small_str
    mov rsi, 2
    mov rdx, 1
    call getPizzaCost
    
    ; Print the result message
    push rax            ; save total price
    mov rdi, result_msg
    call print_string
    
    ; Print the total price
    pop rdi             ; restore total price
    mov rsi, buffer
    call cents_to_string
    mov rdi, buffer
    call print_string
    
    ; Print newline
    mov rdi, newline
    call print_string
    
    ; Exit
    mov rax, 60
    mov rdi, 0
    syscall
