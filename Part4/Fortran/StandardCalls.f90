program io_math_demo
    implicit none
    
    ! Note: For this program to function as expected, the user will have to correctly enter the requested values.

    ! Boolean strings of true/false cannot be converted to a bool variable without conditions,
    ! so we will discuss how that works in the branches video coming soon.
    ! Some languages will not even allow for reading values of 0 or 1 from the terminal as
    ! booleans so we will identify which languages this fails with, and revisit how to make 
    ! this work in the branches video.
    
    character(len=100) :: user_input
    logical :: entered_boolean
    integer :: entered_integer
    real :: entered_float
    integer :: ios_status
    
    ! Note: Fortran LOGICAL type has no standard input format
    ! We must read as string and manually convert - requires conditions

    write ( *, '(A)', advance='no' ) 'Type 55 and press enter.'
    read ( *, *, iostat=ios_status ) entered_integer
    write ( *, '(A,I0,A,I0,A)' ) 'The user entered the integer ', entered_integer, ' (iostat: ', ios_status, ')'
    
    write ( *, '(A)', advance='no' ) 'Type 55.5 and press enter.'
    read ( *, *, iostat=ios_status ) entered_float
    write ( *, '(A,F0.1,A,I0,A)' ) 'The user entered the float ', entered_float, ' (iostat: ', ios_status, ')'
    write ( *,'(A,F4.1)' ) 'The user entered the float ', entered_float
    write ( *,'(A,F3.1)' ) 'The user entered the float ', entered_float
    write ( *,'(A,F5.1)' ) 'The user entered the float ', entered_float
    write ( *,'(A,F4.0)' ) 'The user entered the float ', entered_float
    write ( *,'(A,F4.2)' ) 'The user entered the float ', entered_float

    write ( *, '(A)', advance='no' ) 'Type T and press enter.'
    read ( *, *, iostat=ios_status ) entered_boolean
    write ( *, '(A,L1,A,I0,A)' ) 'The user entered the logical ', entered_boolean, ' (iostat: ', ios_status, ')'
    
    write ( *, '(A)', advance='no' ) 'Type F and press enter.'
    read ( *, *, iostat=ios_status ) entered_boolean
    write ( *, '(A,L1,A,I0,A)' ) 'The user entered the logical ', entered_boolean, ' (iostat: ', ios_status, ')'
    
    write ( *, '(A)', advance='no' ) 'Type .true. and press enter.'
    read ( *, *, iostat=ios_status ) entered_boolean
    write ( *, '(A,L1,A,I0,A)' ) 'The user entered the string ', entered_boolean, ' (iostat: ', ios_status, ')'
    
    write ( *, '(A)', advance='no' ) 'Type .false. and press enter.'
    read ( *, *, iostat=ios_status ) entered_boolean
    write ( *, '(A,L1,A,I0,A)' ) 'The user entered the string ', entered_boolean, ' (iostat: ', ios_status, ')'
    
    write ( *, '(A)', advance='no' ) 'Type .TRUE. and press enter.'
    read ( *, *, iostat=ios_status ) entered_boolean
    write ( *, '(A,L1,A,I0,A)' ) 'The user entered the string ', entered_boolean, ' (iostat: ', ios_status, ')'
    
    write ( *, '(A)', advance='no' ) 'Type True and press enter.'
    read ( *, *, iostat=ios_status ) entered_boolean
    write ( *, '(A,L1,A,I0,A)' ) 'The user entered the string ', entered_boolean, ' (iostat: ', ios_status, ')'
    
    write ( *, '(A)', advance='no' ) 'Type 0 and press enter.'
    read ( *, *, iostat=ios_status ) entered_boolean
    write ( *, '(A,L1,A,I0,A)' ) 'The user entered the string ', entered_boolean, ' (iostat: ', ios_status, ')'
    
    write ( *, '(A)', advance='no' ) 'Type 1 and press enter.'
    read ( *, *, iostat=ios_status ) entered_boolean
    write ( *, '(A,L1,A,I0,A)' ) 'The user entered the string ', entered_boolean, ' (iostat: ', ios_status, ')'
    
    write ( *, '(A)', advance='no' ) 'Type 11 and press enter.'
    read ( *, *, iostat=ios_status ) entered_boolean
    write ( *, '(A,L1,A,I0,A)' ) 'The user entered the string ', entered_boolean, ' (iostat: ', ios_status, ')'
    
    write ( *, '(A)', advance='no' ) 'Type -1 and press enter.'
    read ( *, *, iostat=ios_status ) entered_boolean
    write ( *, '(A,L1,A,I0,A)' ) 'The user entered the string ', entered_boolean, ' (iostat: ', ios_status, ')'
    
    write ( *, '(A)', advance='no' ) 'Type Hello World! and press enter.'
    read ( *, '(A)', iostat=ios_status ) user_input
    write ( *, '(A,A,A,I0,A)' ) 'The user entered the string ', trim ( user_input ), ' (iostat: ', ios_status, ')'
    
    write ( *, '(A)', advance='no' ) 'Type 123abc and press enter.'
    read ( *, *, iostat=ios_status ) entered_integer
    write ( *, '(A,I0,A,I0,A)' ) 'The user entered the integer ', entered_integer, ' (iostat: ', ios_status, ')'
    
    write ( *, '(A)', advance='no' ) 'Type 123.45 and press enter.'
    read ( *, *, iostat=ios_status ) entered_integer
    write ( *, '(A,I0,A,I0,A)' ) 'The user entered the integer ', entered_integer, ' (iostat: ', ios_status, ')'
    
    write ( *, '(A)', advance='no' ) 'Type abc123 and press enter.'
    read ( *, *, iostat=ios_status ) entered_integer
    write ( *, '(A,I0,A,I0,A)' ) 'The user entered the integer ', entered_integer, ' (iostat: ', ios_status, ')'
    
    write ( *, '(A)', advance='no' ) 'Type  567 and press enter.'
    read ( *, *, iostat=ios_status ) entered_integer
    write ( *, '(A,I0,A,I0,A)' ) 'The user entered the integer ', entered_integer, ' (iostat: ', ios_status, ')'
    
    write ( *, '(A)', advance='no' ) 'Type +567 and press enter.'
    read ( *, *, iostat=ios_status ) entered_integer
    write ( *, '(A,I0,A,I0,A)' ) 'The user entered the integer ', entered_integer, ' (iostat: ', ios_status, ')'
    
    write ( *, '(A)', advance='no' ) 'Type -567 and press enter.'
    read ( *, *, iostat=ios_status ) entered_integer
    write ( *, '(A,I0,A,I0,A)' ) 'The user entered the integer ', entered_integer, ' (iostat: ', ios_status, ')'
    
    ! Fortran has intrinsic abs() function
    write ( *, '(A,I0)' ) 'Abs of -5 is ', abs ( -5 )
    write ( *, '(A,F0.1)' ) 'Abs of -5.5 is ', abs ( -5.5 )
    write ( *, '(A,I0)' ) 'Abs of a is ', abs ( iachar ( 'a' ) )
    
    write ( *, '(A,F0.1)' ) 'Pow of 2^5 is ', 2.0 ** 5
    write ( *, '(A,F0.1)' ) 'Pow of 2.2^5.2 is ', 2.2 ** 5.2
    !write ( *, '(A,F0.1)' ) 'Pow of a^b is ', 'a' ** 'b' )
    
    ! Note trig functions are almost always in radians, not degrees
    write ( *, '(A,F0.6)' ) 'Sin of 90 is ', sin ( 90.0 )
    write ( *, '(A,F0.6)' ) 'Sin of pi/2 is ', sin ( acos ( -1.0 ) / 2.0 )
    
    write ( *, '(A,F0.6)' ) 'Cos of 180 is ', cos ( 180.0 )
    write ( *, '(A,F0.6)' ) 'Cos of pi is ', cos ( acos ( -1.0 ) )
    
    ! Rounding type functions are very useful for explicit float to int conversions
    write ( *, '(A,I0)' ) 'Floor of 5.5 is ', floor ( 5.5 )
    write ( *, '(A,I0)' ) 'Floor of -5.5 is ', floor ( -5.5 )
    
    write ( *, '(A,I0)' ) 'Ceil of 5.5 is ', ceiling ( 5.5 )
    write ( *, '(A,I0)' ) 'Ceil of -5.5 is ', ceiling ( -5.5 )
    
    write ( *, '(A,F0.1)' ) 'Round of 5.5 is ', anint ( 5.5 )
    write ( *, '(A,F0.1)' ) 'Round of -5.5 is ', anint ( -5.5 )
    
    write ( *, '(A,F0.1)' ) 'Trunc of 5.5 is ', aint ( 5.5 )
    write ( *, '(A,F0.1)' ) 'Trunc of -5.5 is ', aint ( -5.5 )
    
    ! This will NOT crash in Fortran (iostat indicates error status)
    write ( *, '(A)', advance='no' ) 'Type Hello World! and press enter.'
    read ( *, *, iostat=ios_status ) entered_integer
    write ( *, '(A,I0,A,I0,A)' ) 'The user entered the integer ', entered_integer, ' (iostat: ', ios_status, ')'
    
    ! This will NOT crash in Fortran (iostat indicates error status)
    write ( *, '(A)', advance='no' ) 'Type abc123 and press enter.'
    read ( *, *, iostat=ios_status ) entered_integer
    write ( *, '(A,I0,A,I0,A)' ) 'The user entered the integer ', entered_integer, ' (iostat: ', ios_status, ')'
    
end program io_math_demo
