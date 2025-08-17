section .data
    ; String constants for output
    msg1 db "Our two variables %d and %d", 10, 0
    msg2 db "Our incorrectly assigned variables %d and %d", 10, 0
    msg3 db "First value ( %d ) plus second value ( %d ) is %d", 10, 0
    msg4 db "First value ( %d ) greater than or equal to second value ( %d ) is %d", 10, 0
    msg5 db "First value ( %d ) ORed with second value ( %d ) is %d", 10, 0
    msg6 db "The sum of 1 + 2 + 3 is %d", 10, 0
    msg7 db "Customer gets a free coffee %d", 10, 0
    msg8 db "Total item cost is %.2f", 10, 0
    
    ; Variable storage
    a dd 1
    b dd 0
    c dd 0
    value1 dd 5
    value2 dd 6
    sumValue dd 0
    value3 dd 7
    value4 dd 8
    comparisonValue dd 0
    value5 dd 1  ; true
    value6 dd 0  ; false
    logicalValue dd 0
    loyaltyMember dd 0      ; false
    purchased5Coffees dd 1  ; true
    haveCoupon dd 1         ; true
    couponNotExpired dd 1   ; true
    freeCoffee dd 0
    loyaltyAchieved dd 0
    haveValidCoupon dd 0
    itemPrice dq 99.99
    itemShipping dq 9.99
    purchaseQuantity dd 5
    totalCost dq 0.0
    SATURDAY dd 6
    SUNDAY dd 7
    day dd 6
    isWeekend dd 0

section .text
    global _start
    extern printf
    extern exit

_start:
    ; Most programs allow this operation, but where is the result? We can't use it!
    mov eax, 5
    add eax, 3  ; Result in eax but we don't use it

    ; Simple assignment of a constant and a variable to another variable:
    mov eax, [a]
    mov [b], eax
    
    ; Print our two variables
    mov rdi, msg1
    mov esi, [a]
    mov edx, [b]
    xor rax, rax
    call printf

    ; Assignment errors - This code demonstrates common mistakes
    ; Using uninitialized variable c (contains garbage)
    mov eax, [c]
    mov [a], eax
    
    ; Increment c (still garbage)
    inc dword [c]
    
    ; Note: "1 = a" would be a syntax error in assembly, can't assign to literal
    
    ; Print incorrectly assigned variables
    mov rdi, msg2
    mov esi, [a]
    mov edx, [c]
    xor rax, rax
    call printf

    ; Assign the result of a mathematical operation
    mov eax, [value1]
    add eax, [value2]
    mov [sumValue], eax
    
    ; Print mathematical result
    mov rdi, msg3
    mov esi, [value1]
    mov edx, [value2]
    mov ecx, [sumValue]
    xor rax, rax
    call printf

    ; Assign the result of a comparison operation ( value3 >= value4 )
    mov eax, [value3]
    cmp eax, [value4]
    setge al        ; Set al to 1 if greater or equal, 0 otherwise
    movzx eax, al   ; Zero extend to full register
    mov [comparisonValue], eax
    
    ; Print comparison result
    mov rdi, msg4
    mov esi, [value3]
    mov edx, [value4]
    mov ecx, [comparisonValue]
    xor rax, rax
    call printf

    ; Assign the result of a logical operation ( value5 || value6 )
    mov eax, [value5]
    or eax, [value6]  ; Logical OR
    mov [logicalValue], eax
    
    ; Print logical result
    mov rdi, msg5
    mov esi, [value5]
    mov edx, [value6]
    mov ecx, [logicalValue]
    xor rax, rax
    call printf

    ; Storing a basic complex operation, multiple additions
    mov eax, 1
    add eax, 2
    add eax, 3
    mov [sumValue], eax
    
    ; Print sum result
    mov rdi, msg6
    mov esi, [sumValue]
    xor rax, rax
    call printf

    ; Complex logical operation with operator precedence
    ; freeCoffee = loyaltyMember && purchased5Coffees || haveCoupon && couponNotExpired
    mov eax, [loyaltyMember]
    and eax, [purchased5Coffees]  ; loyaltyMember && purchased5Coffees
    
    mov ebx, [haveCoupon]
    and ebx, [couponNotExpired]   ; haveCoupon && couponNotExpired
    
    or eax, ebx                   ; Combine with OR
    mov [freeCoffee], eax
    
    ; Print free coffee result
    mov rdi, msg7
    mov esi, [freeCoffee]
    xor rax, rax
    call printf

    ; Breaking apart complex statements for clarity
    mov eax, [loyaltyMember]
    and eax, [purchased5Coffees]
    mov [loyaltyAchieved], eax
    
    mov eax, [haveCoupon]
    and eax, [couponNotExpired]
    mov [haveValidCoupon], eax
    
    mov eax, [loyaltyAchieved]
    and eax, [haveValidCoupon]
    mov [freeCoffee], eax
    
    ; Print free coffee result again
    mov rdi, msg7
    mov esi, [freeCoffee]
    xor rax, rax
    call printf

    ; Floating point operations for cost calculation
    ; This code shows incorrect operator precedence
    fld qword [itemShipping]      ; Load itemShipping
    fild dword [purchaseQuantity] ; Load and convert purchaseQuantity to float
    fmulp                         ; Multiply (shipping * quantity)
    fld qword [itemPrice]         ; Load itemPrice
    faddp                         ; Add (price + (shipping * quantity))
    fstp qword [totalCost]        ; Store result
    
    ; Print incorrect total
    mov rdi, msg8
    mov rax, 1  ; 1 floating point argument
    movsd xmm0, [totalCost]
    call printf

    ; Correct calculation with proper precedence
    fld qword [itemPrice]         ; Load itemPrice
    fld qword [itemShipping]      ; Load itemShipping
    faddp                         ; Add (price + shipping)
    fild dword [purchaseQuantity] ; Load and convert quantity
    fmulp                         ; Multiply ((price + shipping) * quantity)
    fstp qword [totalCost]        ; Store result
    
    ; Print correct total
    mov rdi, msg8
    mov rax, 1  ; 1 floating point argument
    movsd xmm0, [totalCost]
    call printf

    ; Weekend logic error example
    ; isWeekend = ( day == SATURDAY || SUNDAY )
    ; This is equivalent to: isWeekend = ( day == SATURDAY ) || SUNDAY
    mov eax, [day]
    cmp eax, [SATURDAY]
    sete al                 ; Set if equal to SATURDAY
    movzx eax, al
    
    ; Now OR with SUNDAY (which is 7, always true)
    or eax, [SUNDAY]        ; This makes the result always true!
    mov [isWeekend], eax

    ; Correct weekend logic
    ; isWeekend = ( day == SATURDAY ) || ( day == SUNDAY )
    mov eax, [day]
    cmp eax, [SATURDAY]
    sete al                 ; Set if equal to SATURDAY
    movzx ebx, al
    
    mov eax, [day]
    cmp eax, [SUNDAY]
    sete al                 ; Set if equal to SUNDAY
    movzx eax, al
    
    or eax, ebx             ; Combine the two comparisons
    mov [isWeekend], eax

    ; Exit program
    mov rdi, 0      ; exit status
    call exit
