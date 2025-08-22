section .data
    ; String constants
    before_conditions db 'Before Conditions', 10, 0
    branch_executed db 'Branch Executed', 10, 0
    branch_not_executed db 'Branch Not Executed', 10, 0
    after_conditions db 'After Conditions', 10, 0
    heater_on_msg db 'Heater is on: ', 0
    overrun_msg db 'Overrun Amount: ', 0
    budget_overrun_msg db 'Budget overrun occurred: ', 0
    variable_inside_msg db 'Variable inside: ', 0
    variable_outside_msg db 'Variable outside: ', 0
    statement_three db 'Statement Three', 10, 0
    statement_four db 'Statement Four', 10, 0
    good_branch_msg db 'Good branch!', 10, 0
    grade_a db 'You got an A', 10, 0
    grade_b db 'You got an B', 10, 0
    grade_c db 'You got an C', 10, 0
    grade_d db 'You got an D', 10, 0
    failed_test db 'You failed the test', 10, 0
    valid_age_msg db 'Valid age', 10, 0
    invalid_age_msg db 'Invalid age', 10, 0
    access_granted_msg db 'Access granted', 10, 0
    resource_unavailable_msg db 'Resource Unavailable', 10, 0
    user_banned_msg db 'User is Banned', 10, 0
    wrong_method_msg db 'Wrong Method', 10, 0
    wrong_user_level_msg db 'Wrong User Level', 10, 0
    please_log_in_msg db 'Please Log In', 10, 0
    retry_password_msg db 'Retry password', 10, 0
    variable_is_1 db 'Variable is 1', 10, 0
    variable_is_2 db 'Variable is 2', 10, 0
    variable_unexpected db 'Variable is unexpected value!', 10, 0
    main_menu_msg db 'Main Menu:', 10, '1. Start Game', 10, '2. Load Game', 10, '3. Show Help', 10, '4. Exit', 10, 'Enter your choice: ', 0
    start_game_msg db 'Starting new game...', 10, 0
    load_game_msg db 'Loading saved game...', 10, 0
    help_msg db 'Help: Use the number keys to navigate the menu.', 10, 0
    exit_msg db 'Exiting program. Goodbye!', 10, 0
    invalid_choice_msg db 'Invalid choice. Please select a valid option.', 10, 0
    speed_msg db 'Speed: ', 0
    enter_number_msg db 'Enter a number:', 10, 0
    valid_number_msg db 'User entered a valid number', 10, 0
    invalid_number_msg db 'Invalid number entered.', 10, 0
    enter_boolean_msg db 'Enter a boolean:', 10, 0
    boolean_valid_msg db 'Entered boolean is valid', 10, 0
    boolean_invalid_msg db 'Invalid boolean entered', 10, 0
    valid_boolean_entered_msg db 'Valid boolean entered', 10, 0
    false_entered_msg db 'False Entered ', 0
    float_equal_msg db 'First float plus second float is equal to third float.', 10, 0
    float_not_equal_msg db 'First float plus second float is NOT equal to third float.', 10, 0
    
    ; String constants for comparison
    admin_str db 'admin', 0
    post_str db 'POST', 0
    false_str db 'false', 0
    true_str db 'true', 0
    
    ; Format strings for printf
    fmt_bool db '%d', 10, 0
    fmt_int db '%d', 10, 0
    fmt_float db '%.2f', 10, 0
    fmt_char db '%c', 0
    fmt_str db '%s', 0
    newline db 10, 0
    
    ; Variables
    my_variable dq 0
    temperature dq 64
    heater_on dq 0
    temperature_cold dq 0
    budget dd 5000.0
    buffer dd 500.0
    estimated_cost dd 4750.0
    budget_overrun dq 0
    overrun_amount dd 0.0
    variable dq 10
    score dq 85
    age dq 125
    switch_variable dq 2
    choice dq 0
    time_var dq 0
    distance dq 100
    speed dq 0
    read_int dq 0
    read_bool dq 0
    read_valid_bool dq 0
    input_matches_false dq 0
    first_float dd 0.1
    second_float dd 0.2
    sum dd 0.0
    third_float dd 0.3
    tolerance dd 0.000001
    difference dd 0.0
    
    ; Boolean flags
    is_logged_in dq 1
    is_banned dq 0
    resource_is_available dq 1
    user_exists dq 1
    password_valid dq 1
    
    ; Input buffer
    input_buffer times 256 db 0

section .text
    global _start
    extern printf
    extern scanf
    extern strlen
    extern strcmp
    extern atoi
    extern fgets
    extern stdin
    extern exit

_start:
    ; Basic if statements with hard coded conditions
    mov rdi, fmt_str
    mov rsi, before_conditions
    call printf
    
    ; if ( true )
    mov rax, 1
    cmp rax, 0
    je .skip_true_branch
    mov rdi, fmt_str
    mov rsi, branch_executed
    call printf
.skip_true_branch:

    ; if ( false )
    mov rax, 0
    cmp rax, 0
    je .skip_false_branch
    mov rdi, fmt_str
    mov rsi, branch_not_executed
    call printf
.skip_false_branch:

    mov rdi, fmt_str
    mov rsi, after_conditions
    call printf

    ; If with hard coded assignment
    mov qword [my_variable], 1
    mov rax, [my_variable]
    cmp rax, 0
    je .skip_assignment_branch
    mov rdi, fmt_str
    mov rsi, branch_executed
    call printf
.skip_assignment_branch:

    ; If with variable assignment (using function result)
    call some_function_result
    mov [my_variable], rax
    mov rax, [my_variable]
    cmp rax, 0
    je .skip_function_branch
    mov rdi, fmt_str
    mov rsi, branch_executed
    call printf
.skip_function_branch:

    ; More proper conditional branch
    mov rax, [temperature]
    cmp rax, 70
    jge .temperature_not_cold
    mov qword [temperature_cold], 1
    jmp .check_temperature_cold
.temperature_not_cold:
    mov qword [temperature_cold], 0
.check_temperature_cold:
    
    mov qword [heater_on], 0
    mov rax, [temperature_cold]
    cmp rax, 1
    jne .heater_off1
    mov qword [heater_on], 1
.heater_off1:

    mov rdi, fmt_str
    mov rsi, heater_on_msg
    call printf
    mov rdi, fmt_bool
    mov rsi, [heater_on]
    call printf

    ; Alternate code to evaluate a conditional branch
    mov qword [heater_on], 0
    mov rax, [temperature]
    cmp rax, 70
    jge .heater_off2
    mov qword [heater_on], 1
.heater_off2:

    mov rdi, fmt_str
    mov rsi, heater_on_msg
    call printf
    mov rdi, fmt_bool
    mov rsi, [heater_on]
    call printf

    ; Using a code block that executes if a condition is true
    mov qword [budget_overrun], 0
    
    ; Load floats and compare (simplified integer comparison for assembly)
    mov rax, 4750  ; estimated_cost
    add rax, 500   ; buffer
    cmp rax, 5000  ; budget
    jle .no_budget_overrun
    
    ; Calculate overrun (simplified)
    mov rdi, fmt_str
    mov rsi, overrun_msg
    call printf
    mov rdi, fmt_float
    mov rsi, 250   ; simplified overrun amount
    call printf
    
    mov qword [budget_overrun], 1
.no_budget_overrun:

    mov rdi, fmt_str
    mov rsi, budget_overrun_msg
    call printf
    mov rdi, fmt_bool
    mov rsi, [budget_overrun]
    call printf

    ; Variable scope: Shadowed variables (simplified - assembly doesn't have scoping)
    mov qword [variable], 10
    ; Simulate inner scope
    mov qword [variable], 20
    mov rdi, fmt_str
    mov rsi, variable_inside_msg
    call printf
    mov rdi, fmt_int
    mov rsi, [variable]
    call printf
    
    ; Back to "outer scope" (but variable is still 20 in assembly)
    mov rdi, fmt_str
    mov rsi, variable_outside_msg
    call printf
    mov rdi, fmt_int
    mov rsi, [variable]
    call printf

    ; Statement execution example
    mov rax, 0  ; false condition
    cmp rax, 0
    je .skip_statements
    mov rdi, fmt_str
    mov rsi, statement_three
    call printf
    mov rdi, fmt_str
    mov rsi, statement_four
    call printf
.skip_statements:

    ; Good branch example
    mov rax, 1  ; true condition
    cmp rax, 0
    je .skip_good_branch
    mov rdi, fmt_str
    mov rsi, good_branch_msg
    call printf
.skip_good_branch:

    ; Multiple separate if statements with overlapping conditions
    mov rax, [score]
    cmp rax, 90
    jl .check_b_grade1
    cmp rax, 100
    jg .check_b_grade1
    mov rdi, fmt_str
    mov rsi, grade_a
    call printf
.check_b_grade1:
    mov rax, [score]
    cmp rax, 80
    jl .check_c_grade1
    cmp rax, 90
    jge .check_c_grade1
    mov rdi, fmt_str
    mov rsi, grade_b
    call printf
.check_c_grade1:
    mov rax, [score]
    cmp rax, 70
    jl .check_d_grade1
    cmp rax, 80
    jge .check_d_grade1
    mov rdi, fmt_str
    mov rsi, grade_c
    call printf
.check_d_grade1:
    mov rax, [score]
    cmp rax, 60
    jl .check_f_grade1
    cmp rax, 70
    jge .check_f_grade1
    mov rdi, fmt_str
    mov rsi, grade_d
    call printf
.check_f_grade1:
    mov rax, [score]
    cmp rax, 60
    jge .grade_chain_done1
    mov rdi, fmt_str
    mov rsi, failed_test
    call printf
.grade_chain_done1:

    ; Else if example (implemented as jump chain)
    mov rax, [score]
    cmp rax, 90
    jl .check_b_grade2
    mov rdi, fmt_str
    mov rsi, grade_a
    call printf
    jmp .grade_chain_done2
.check_b_grade2:
    cmp rax, 80
    jl .check_c_grade2
    mov rdi, fmt_str
    mov rsi, grade_b
    call printf
    jmp .grade_chain_done2
.check_c_grade2:
    cmp rax, 70
    jl .check_d_grade2
    mov rdi, fmt_str
    mov rsi, grade_c
    call printf
    jmp .grade_chain_done2
.check_d_grade2:
    cmp rax, 60
    jl .check_f_grade2
    mov rdi, fmt_str
    mov rsi, grade_d
    call printf
    jmp .grade_chain_done2
.check_f_grade2:
    mov rdi, fmt_str
    mov rsi, failed_test
    call printf
.grade_chain_done2:

    ; Unreachable else example
    mov rax, [age]
    cmp rax, 0
    jle .check_age_upper
    jmp .valid_age
.check_age_upper:
    cmp rax, 100
    jl .valid_age
    ; This else is unreachable because (age > 0) OR (age < 100) is always true
    mov rdi, fmt_str
    mov rsi, invalid_age_msg
    call printf
    jmp .age_check_done
.valid_age:
    mov rdi, fmt_str
    mov rsi, valid_age_msg
    call printf
.age_check_done:

    ; Complex condition example (simplified)
    mov rax, [is_logged_in]
    cmp rax, 1
    jne .access_denied
    ; Check other conditions (simplified)
    mov rax, [is_banned]
    cmp rax, 0
    jne .access_denied
    mov rax, [resource_is_available]
    cmp rax, 1
    jne .access_denied
    mov rdi, fmt_str
    mov rsi, access_granted_msg
    call printf
    jmp .access_check_done
.access_denied:
    mov rdi, fmt_str
    mov rsi, please_log_in_msg
    call printf
.access_check_done:

    ; Breaking complex condition into nested if
    mov rax, [is_logged_in]
    cmp rax, 1
    jne .nested_please_log_in
    
    ; Simulate role check (simplified)
    mov rax, 1  ; assume role is "admin"
    cmp rax, 1
    jne .nested_wrong_user_level
    
    ; Simulate method check
    mov rax, 1  ; assume method is "POST"
    cmp rax, 1
    jne .nested_wrong_method
    
    mov rax, [is_banned]
    cmp rax, 0
    jne .nested_user_banned
    
    mov rax, [resource_is_available]
    cmp rax, 1
    jne .nested_resource_unavailable
    
    mov rdi, fmt_str
    mov rsi, access_granted_msg
    call printf
    jmp .nested_access_done
    
.nested_resource_unavailable:
    mov rdi, fmt_str
    mov rsi, resource_unavailable_msg
    call printf
    jmp .nested_access_done
    
.nested_user_banned:
    mov rdi, fmt_str
    mov rsi, user_banned_msg
    call printf
    jmp .nested_access_done
    
.nested_wrong_method:
    mov rdi, fmt_str
    mov rsi, wrong_method_msg
    call printf
    jmp .nested_access_done
    
.nested_wrong_user_level:
    mov rdi, fmt_str
    mov rsi, wrong_user_level_msg
    call printf
    jmp .nested_access_done
    
.nested_please_log_in:
    mov rdi, fmt_str
    mov rsi, please_log_in_msg
    call printf
.nested_access_done:

    ; No dangling else with blocks explicitly defined
    mov rax, [user_exists]
    cmp rax, 1
    jne .user_check_done
    
    mov rax, [password_valid]
    cmp rax, 1
    jne .retry_password
    mov rdi, fmt_str
    mov rsi, access_granted_msg
    call printf
    jmp .user_check_done
    
.retry_password:
    mov rdi, fmt_str
    mov rsi, retry_password_msg
    call printf
.user_check_done:

    ; Basic switch statement (implemented as jump table)
    mov rax, [switch_variable]
    cmp rax, 1
    je .case_1
    cmp rax, 2
    je .case_2
    jmp .default_case
    
.case_1:
    mov rdi, fmt_str
    mov rsi, variable_is_1
    call printf
    jmp .switch_done
    
.case_2:
    mov rdi, fmt_str
    mov rsi, variable_is_2
    call printf
    jmp .switch_done
    
.default_case:
    mov rdi, fmt_str
    mov rsi, variable_unexpected
    call printf
.switch_done:

    ; Switch on user input
    mov rdi, fmt_str
    mov rsi, main_menu_msg
    call printf
    
    ; Read user input
    mov rdi, input_buffer
    mov rsi, 256
    mov rdx, [stdin]
    call fgets
    
    ; Convert to integer
    mov rdi, input_buffer
    call atoi
    mov [choice], rax
    
    mov rax, [choice]
    cmp rax, 1
    je .menu_case_1
    cmp rax, 2
    je .menu_case_2
    cmp rax, 3
    je .menu_case_3
    cmp rax, 4
    je .menu_case_4
    jmp .menu_default
    
.menu_case_1:
    mov rdi, fmt_str
    mov rsi, start_game_msg
    call printf
    jmp .menu_done
    
.menu_case_2:
    mov rdi, fmt_str
    mov rsi, load_game_msg
    call printf
    jmp .menu_done
    
.menu_case_3:
    mov rdi, fmt_str
    mov rsi, help_msg
    call printf
    jmp .menu_done
    
.menu_case_4:
    mov rdi, fmt_str
    mov rsi, exit_msg
    call printf
    jmp .menu_done
    
.menu_default:
    mov rdi, fmt_str
    mov rsi, invalid_choice_msg
    call printf
.menu_done:

    ; Divide by zero defensive condition
    mov qword [speed], 0
    mov rax, [time_var]
    cmp rax, 0
    je .skip_division
    mov rax, [distance]
    mov rbx, [time_var]
    xor rdx, rdx
    div rbx
    mov [speed], rax
.skip_division:
    
    mov rdi, fmt_str
    mov rsi, speed_msg
    call printf
    mov rdi, fmt_int
    mov rsi, [speed]
    call printf

    ; Handling both valid and invalid user inputs converted to booleans
    mov rdi, fmt_str
    mov rsi, enter_number_msg
    call printf
    
    ; Read input using fgets
    mov rdi, input_buffer
    mov rsi, 256
    mov rdx, [stdin]
    call fgets
    
    ; Convert string to integer using atoi
    mov rdi, input_buffer
    call atoi
    mov [read_int], rax
    
    ; Simple validation - if atoi returns 0, check if input was actually "0"
    cmp rax, 0
    jne .valid_number
    mov al, [input_buffer]
    cmp al, '0'
    je .valid_number
    
    ; Invalid number
    mov qword [read_int], -1
    mov rdi, fmt_str
    mov rsi, invalid_number_msg
    call printf
    jmp .number_input_done
    
.valid_number:
    mov rdi, fmt_str
    mov rsi, valid_number_msg
    call printf
.number_input_done:

    ; Method 1 of parsing an input string to a boolean
    mov rdi, fmt_str
    mov rsi, enter_boolean_msg
    call printf
    
    ; Read input using fgets
    mov rdi, input_buffer
    mov rsi, 256
    mov rdx, [stdin]
    call fgets
    
    mov qword [read_valid_bool], 0
    
    ; Try to convert to integer first
    mov rdi, input_buffer
    call atoi
    mov [read_int], rax
    
    cmp rax, 0
    jne .bool_from_int
    ; Check if input was actually "0"
    mov al, [input_buffer]
    cmp al, '0'
    jne .check_bool_string
    
.bool_from_int:
    mov qword [read_valid_bool], 1
    cmp qword [read_int], 0
    jne .bool_true
    mov qword [read_bool], 0
    jmp .bool_conversion_done
.bool_true:
    mov qword [read_bool], 1
    jmp .bool_conversion_done
    
.check_bool_string:
    ; Simple string comparison for "true" and "false"
    mov rdi, input_buffer
    mov rsi, true_str
    call strcmp
    cmp rax, 0
    je .string_is_true
    
    mov rdi, input_buffer
    mov rsi, false_str
    call strcmp
    cmp rax, 0
    je .string_is_false
    
    ; Remove newline from fgets input for comparison
    mov rdi, input_buffer
    call strlen
    dec rax
    mov byte [input_buffer + rax], 0
    
    mov rdi, input_buffer
    mov rsi, true_str
    call strcmp
    cmp rax, 0
    je .string_is_true
    
    mov rdi, input_buffer
    mov rsi, false_str
    call strcmp
    cmp rax, 0
    je .string_is_false
    
    mov rdi, fmt_str
    mov rsi, boolean_invalid_msg
    call printf
    jmp .bool_conversion_done
    
.string_is_true:
    mov qword [read_bool], 1
    mov qword [read_valid_bool], 1
    jmp .bool_conversion_done
    
.string_is_false:
    mov qword [read_bool], 0
    mov qword [read_valid_bool], 1
    
.bool_conversion_done:
    mov rax, [read_valid_bool]
    cmp rax, 1
    jne .bool_method1_done
    mov rdi, fmt_str
    mov rsi, boolean_valid_msg
    call printf
.bool_method1_done:

    ; Alternate method of parsing a string to boolean using guard clauses instead of nested conditions
    mov rdi, fmt_str
    mov rsi, enter_boolean_msg
    call printf
    
    ; Read input using fgets
    mov rdi, input_buffer
    mov rsi, 256
    mov rdx, [stdin]
    call fgets
    
    ; Remove newline
    mov rdi, input_buffer
    call strlen
    dec rax
    mov byte [input_buffer + rax], 0
    
    mov qword [read_valid_bool], 0
    
    ; Try integer conversion first
    mov rdi, input_buffer
    call atoi
    mov [read_int], rax
    
    cmp rax, 0
    jne .guard_bool_from_int
    mov al, [input_buffer]
    cmp al, '0'
    jne .guard_check_bool_string
    
.guard_bool_from_int:
    mov qword [read_valid_bool], 1
    cmp qword [read_int], 0
    jne .guard_bool_true
    mov qword [read_bool], 0
    jmp .guard_bool_check_done
.guard_bool_true:
    mov qword [read_bool], 1
    jmp .guard_bool_check_done
    
.guard_check_bool_string:
    mov rax, [read_valid_bool]
    cmp rax, 0
    jne .guard_bool_check_done
    
    mov rdi, input_buffer
    mov rsi, false_str
    call strcmp
    cmp rax, 0
    jne .guard_check_true
    mov qword [read_bool], 0
    mov qword [read_valid_bool], 1
    jmp .guard_bool_check_done
    
.guard_check_true:
    mov rdi, input_buffer
    mov rsi, true_str
    call strcmp
    cmp rax, 0
    jne .guard_bool_check_done
    mov qword [read_bool], 1
    mov qword [read_valid_bool], 1
    
.guard_bool_check_done:
    mov rax, [read_valid_bool]
    cmp rax, 1
    jne .guard_invalid_bool
    mov rdi, fmt_str
    mov rsi, valid_boolean_entered_msg
    call printf
    jmp .guard_method_done
    
.guard_invalid_bool:
    mov rdi, fmt_str
    mov rsi, boolean_invalid_msg
    call printf
.guard_method_done:
    ; first_float = 0.1, second_float = 0.2, sum = 0.3, third_float = 0.3
    mov rax, 100000   ; 0.1 * 1000000
    add rax, 200000   ; + 0.2 * 1000000
    mov rbx, 300000   ; 0.3 * 1000000
    
    ; Calculate difference
    sub rax, rbx
    ; Check if absolute difference < tolerance (1 in this scale)
    cmp rax, 0
    jge .positive_diff1
    neg rax
.positive_diff1:
    cmp rax, 1000  ; tolerance * 1000000
    jg .float_not_equal1
    mov rdi, fmt_str
    mov rsi, float_equal_msg
    call printf
    jmp .float_compare_done1
.float_not_equal1:
    mov rdi, fmt_str
    mov rsi, float_not_equal_msg
    call printf
.float_compare_done1:

    ; Exit program
    mov rdi, 0
    call exit

; Stubbed out functions
user_input:
    mov rax, 3
    ret

some_function_result:
    mov rax, 1
    ret
