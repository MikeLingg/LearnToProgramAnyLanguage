; Pizza example with input parameters
; Demonstrates parameter passing via registers and stack
section .data
    pizza_msg db "Pizza order:", 10, 0
    base_msg db 9, "Base price: $", 0
    topping_msg db 9, "Topping price: $", 0
    delivery_msg db 9, "Delivery price: $", 0
    total_msg db 9, 9, "Total price: $", 0
    newline db 10, 0
    
    small_str db "small", 0
    medium_str db "medium", 0
    large_str db "large", 0
    
    ; Price constants (in cents to avoid floating point)
    small_price equ 1299    ; $12.99
    medium_price equ 1599   ; $15.99
    large_price equ 1899    ; $18.99
    topping_price equ 150   ; $1.50
    delivery_fee equ 300    ; $3.00

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

; Function: printPizzaCost
; Parameters: rdi = size string pointer, rsi = topping count, rdx = delivery flag
printPizzaCost:
    push rbp
    mov rbp, rsp
    sub rsp, 32
    
    ; Save parameters
    mov [rbp-8], rdi    ; size string
    mov [rbp-16], rsi   ; topping count
    mov [rbp-24], rdx   ; delivery flag
    
    ; Print "Pizza order:"
    mov rdi, pizza_msg
    call print_string
    
    ; Determine base price
    mov rdi, [rbp-8]
    call strcmp_small
    test rax, rax
    jz .small_pizza
    
    mov rdi, [rbp-8]
    call strcmp_medium
    test rax, rax
    jz .medium_pizza
    
    mov rax, large_price
    jmp .calculate_toppings
    
.small_pizza:
    mov rax, small_price
    jmp .calculate_toppings
    
.medium_pizza:
    mov rax, medium_price
    
.calculate_toppings:
    mov [rbp-32], rax   ; store base price
    
    ; Print base price
    mov rdi, base_msg
    call print_string
    mov rdi, [rbp-32]
    mov rsi, buffer
    call cents_to_string
    mov rdi, buffer
    call print_string
    mov rdi, newline
    call print_string
    
    ; Calculate topping cost
    mov rax, [rbp-16]   ; topping count
    mov rbx, topping_price
    mul rbx             ; topping cost in rax
    mov [rbp-40], rax   ; store topping cost
    
    ; Print topping price
    mov rdi, topping_msg
    call print_string
    mov rdi, rax
    mov rsi, buffer
    call cents_to_string
    mov rdi, buffer
    call print_string
    mov rdi, newline
    call print_string
    
    ; Check delivery
    mov rax, 0
    cmp qword [rbp-24], 0
    je .no_delivery
    mov rax, delivery_fee
    
.no_delivery:
    mov [rbp-48], rax   ; store delivery cost
    
    ; Print delivery price
    mov rdi, delivery_msg
    call print_string
    mov rdi, rax
    mov rsi, buffer
    call cents_to_string
    mov rdi, buffer
    call print_string
    mov rdi, newline
    call print_string
    
    ; Calculate and print total
    mov rax, [rbp-32]   ; base price
    add rax, [rbp-40]   ; add topping cost
    add rax, [rbp-48]   ; add delivery cost
    
    mov rdi, total_msg
    call print_string
    mov rdi, rax
    mov rsi, buffer
    call cents_to_string
    mov rdi, buffer
    call print_string
    mov rdi, newline
    call print_string
    
    mov rsp, rbp
    pop rbp
    ret

; String comparison functions
strcmp_small:
    mov rsi, small_str
    call strcmp
    ret
    
strcmp_medium:
    mov rsi, medium_str
    call strcmp
    ret

strcmp:
    ; Compare strings in rdi and rsi, return 0 if equal, 1 if not equal
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
    ; Call printPizzaCost("small", 2, 1)
    mov rdi, small_str
    mov rsi, 2
    mov rdx, 1
    call printPizzaCost
    
    ; Exit
    mov rax, 60
    mov rdi, 0
    syscall
