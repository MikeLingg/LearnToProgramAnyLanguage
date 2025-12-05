program BasicConcepts
    implicit none

    ! ANSI color codes
    character(len=*), parameter :: COLOR_RESET = char(27)//'[0m'
    character(len=*), parameter :: COLOR_BOLD = char(27)//'[1m'
        
    ! This signifies a number of basic concepts that would be included in one program, 
    ! except some of these concepts will prevent compilation or crash the program. 
    ! So this program will be broken up as specific languages require it.

    logical :: falseBoolean = .false.
    logical :: trueBoolean = .true.
    
    integer(kind=1) :: minSigned8 = -128
    integer(kind=1) :: maxSigned8 = 127
    integer(kind=1) :: minUnsigned8 = 0
    integer(kind=1) :: maxUnsigned8 = 127  ! Fortran doesn't have true unsigned types
    
    integer(kind=2) :: minSigned16 = -32768
    integer(kind=2) :: maxSigned16 = 32767
    integer(kind=2) :: minUnsigned16 = 0
    integer(kind=2) :: maxUnsigned16 = 32767  ! Fortran doesn't have true unsigned types
    
    ! Again, -2147483648 is first treated as positive which is out of range.
    integer(kind=4) :: minSigned32 = -2147483647 - 1
    integer(kind=4) :: maxSigned32 = 2147483647
    integer(kind=4) :: minUnsigned32 = 0
    integer(kind=4) :: maxUnsigned32 = 2147483647  ! Fortran doesn't have true unsigned types
    
    integer(kind=8) :: minSigned64 = -9223372036854775807_8 - 1
    integer(kind=8) :: maxSigned64 = 9223372036854775807_8
    integer(kind=8) :: minUnsigned64 = 0_8
    integer(kind=8) :: maxUnsigned64 = 9223372036854775807_8  ! Fortran doesn't have true unsigned types

    real(kind=4) :: floatMax = huge(1.0_4)
    real(kind=4) :: floatMin = tiny(1.0_4)
    
    real(kind=4) :: zeroPointOne = 0.1_4
    real(kind=4) :: zeroPointTwo = 0.2_4
    real(kind=4) :: zeroPointThree = 0.3_4
    
    real(kind=8) :: doubleMax = huge(1.0_8)
    real(kind=8) :: doubleMin = tiny(1.0_8)
    
    ! Shows the basics of ASCII characters, including special ones.
    ! This should print 1 followed by a tab followed by 2, then on the next line print 3.
    character(len=1) :: charOne = '1'
    character(len=1) :: charTab = char(9)   ! ASCII tab
    character(len=1) :: singleQuotes = ''''
    character(len=1) :: charNewLine = char(10)  ! ASCII newline
    character(len=1) :: doubleQuotes = """"

    ! Note some languages may not compile this code so it will be in a separate file, others may throw warnings.
    ! Fortran doesn't allow implicit conversion from int to logical
    logical :: outOfRangeBoolean = .true.
    
    ! Note some languages may not compile this code so it will be in a separate file, others may throw warnings.
    ! Fortran will handle overflow depending on compiler settings
    integer(kind=2) :: outOfRange = 32767

    real(kind=4) :: outOfRangeFloat = huge(1.0_4) * 2.0_4
    real(kind=8) :: outOfRangeDouble = huge(1.0_8) * 2.0_8

    ! Fortran character can handle values beyond ASCII range
    character(len=1) :: outOfRangeChar = char(65)  ! 'A' character

    write(*,*) 'Boolean range:', falseBoolean, trueBoolean
    
    write(*,*) '8 bit signed int range:', minSigned8, maxSigned8
    write(*,*) '8 bit unsigned int range:', minUnsigned8, maxUnsigned8
    
    write(*,*) '16 bit signed int range:', minSigned16, maxSigned16
    write(*,*) '16 bit unsigned int range:', minUnsigned16, maxUnsigned16
    
    write(*,*) '32 bit signed int range:', minSigned32, maxSigned32
    write(*,*) '32 bit unsigned int range:', minUnsigned32, maxUnsigned32
    
    write(*,*) 'Note: Fortran handles integers within specified kinds.'
    
    write(*,*) '64 bit signed int range:', minSigned64, maxSigned64
    write(*,*) '64 bit unsigned int range:', minUnsigned64, maxUnsigned64
    
    call demonstrate_8bit()
    call demonstrate_16bit()
    call demonstrate_32bit()
    call demonstrate_64bit()

    write(*,*) 'Note that scientific notation must be used to print such a small number.'
    write(*,*) '32 bit float:', floatMin, floatMax
    
    ! So let's look at how far off the actual floating point value is from the value it was set to.
    write(*,'(A,3(F0.17,A))') 'Floating point 0.1, 0.2, 0.3 -> ', &
                              zeroPointOne, ' and ', zeroPointTwo, ' and ', zeroPointThree
    
    write(*,*) 'Note that scientific notation must be used to print such a small number.'
    write(*,*) '64 bit float range:', doubleMin, doubleMax
    
    write(*,'(5A1)') charOne, charTab, singleQuotes, charNewLine, doubleQuotes
    
    ! Show how printing as an integer, not a character, can be confusing
    write(*,*) 'charOne as an integer:', ichar(charOne)
    
    write(*,*) 'Out of range Boolean:', outOfRangeBoolean
    
    write(*,*) 'Out of range value:', outOfRange
    
    write(*,*) 'Note that adding a small amount to float max is lost in the precision, so using infinity.'
    
    write(*,*) 'Out of range float and double:', outOfRangeFloat, outOfRangeDouble

    write(*,*) 'Out of range char:', outOfRangeChar

 
contains

    subroutine print_bg_rgb_color(r, g, b, text)
        integer, intent(in) :: r, g, b
        character(len=*), intent(in) :: text
        
        write(*, '(A,I0,A,I0,A,I0,A,A,A)', advance='no') &
            char(27)//'[48;2;', r, ';', g, ';', b, 'm', text, COLOR_RESET
    end subroutine print_bg_rgb_color
    
    subroutine value_to_color(normalized, r, g, b)
        real(8), intent(in) :: normalized
        integer, intent(out) :: r, g, b
        real(8) :: t
        
        if (normalized < 0.5d0) then
            ! Blue to Green
            t = normalized * 2.0d0
            r = 0
            g = int(t * 255.0d0)
            b = int((1.0d0 - t) * 255.0d0)
        else
            ! Green to Red
            t = (normalized - 0.5d0) * 2.0d0
            r = int(t * 255.0d0)
            g = int((1.0d0 - t) * 255.0d0)
            b = 0
        end if
    end subroutine value_to_color
    
    subroutine print_header()
        print '(A)', COLOR_BOLD//''
        print '(A)', 'FORTRAN INTEGER GRANULARITY VISUALIZATION'//COLOR_RESET
        print '(A)', 'Showing how bit-width affects numeric resolution'
        print '(A)', '==============================================='
    end subroutine print_header
    
    subroutine demonstrate_8bit()
        integer :: i, val, r, g, b
        real(8) :: normalized
        integer, dimension(12) :: values
        
        values = [10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 127, 255]
        
        print *, ''
        print '(A)', COLOR_BOLD//'=== 8-BIT UNSIGNED (0-255) ==='//COLOR_RESET
        print '(A)', '256 possible values - Coarse granularity'
        print *, ''
        
        do i = 1, size(values)
            val = values(i)
            normalized = real(val, 8) / 255.0d0
            call value_to_color(normalized, r, g, b)
            
            write(*, '(A,I3,A)', advance='no') '  ', val, ': '
            call print_bg_rgb_color(r, g, b, '    ')
            write(*, '(A,I3,A,I3,A,I3,A)') ' RGB(', r, ',', g, ',', b, ')'
        end do
    end subroutine demonstrate_8bit
    
    subroutine demonstrate_16bit()
        integer :: i, val, r, g, b
        real(8) :: normalized
        integer, dimension(21) :: values
        
        values = [10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 3000, 6000, 9000, &
                  12000, 15000, 18000, 21000, 24000, 27000, 32767, 65535]
        
        print *, ''
        print '(A)', COLOR_BOLD//'=== 16-BIT UNSIGNED (0-65535) ==='//COLOR_RESET
        print '(A)', '65,536 possible values - 256x finer than 8-bit'
        print *, ''
        
        do i = 1, size(values)
            val = values(i)
            normalized = real(val, 8) / 65535.0d0
            call value_to_color(normalized, r, g, b)
            
            write(*, '(A,I5,A)', advance='no') '  ', val, ': '
            call print_bg_rgb_color(r, g, b, '    ')
            write(*, '(A,I3,A,I3,A,I3,A)') ' RGB(', r, ',', g, ',', b, ')'
        end do
    end subroutine demonstrate_16bit
    
    subroutine demonstrate_32bit()
        integer :: i, r, g, b
        integer(8) :: val
        real(8) :: normalized
        integer(8), dimension(17) :: values
        
        values = [int(10,8), int(20,8), int(30,8), int(40,8), int(50,8), &
                  int(60,8), int(70,8), int(80,8), int(90,8), int(100,8), &
                  int(250000000,8), int(500000000,8), int(1000000000,8), &
                  int(1500000000,8), int(2000000000,8), int(2147483647,8), &
                  4294967295_8]
        
        print *, ''
        print '(A)', COLOR_BOLD//'=== 32-BIT UNSIGNED (0-4294967295) ==='//COLOR_RESET
        print '(A)', '4,294,967,296 possible values - 65,536x finer than 16-bit'
        print *, ''
        
        do i = 1, size(values)
            val = values(i)
            normalized = real(val, 8) / 4294967295.0d0
            call value_to_color(normalized, r, g, b)
            
            write(*, '(A,I10,A)', advance='no') '  ', val, ': '
            call print_bg_rgb_color(r, g, b, '    ')
            write(*, '(A,I3,A,I3,A,I3,A)') ' RGB(', r, ',', g, ',', b, ')'
        end do
    end subroutine demonstrate_32bit
    
    subroutine demonstrate_64bit()
        integer :: i, r, g, b
        integer(8) :: val
        real(8) :: normalized
        integer(8), dimension(16) :: values
        
        values = [int(10,8), int(20,8), int(30,8), int(40,8), int(50,8), &
                  int(60,8), int(70,8), int(80,8), int(90,8), int(100,8), &
                  625000000000000000_8, 1250000000000000000_8, &
                  2500000000000000000_8, 5000000000000000000_8, &
                  9223372036854775807_8, -1_8]  ! -1 in signed = max unsigned
        
        print *, ''
        print '(A)', COLOR_BOLD//'=== 64-BIT UNSIGNED (0-18446744073709551615) ==='//COLOR_RESET
        print '(A)', '18,446,744,073,709,551,616 possible values - 4,294,967,296x finer than 32-bit'
        print *, ''
        
        do i = 1, size(values)
            val = values(i)
            if (val < 0) then
                ! Handle max value specially (displayed as unsigned)
                normalized = 1.0d0
            else
                normalized = real(val, 8) / 18446744073709551615.0d0
            end if
            call value_to_color(normalized, r, g, b)
            
            if (val < 0) then
                write(*, '(A,I20,A)', advance='no') '  ', 0, ': '  ! Will print incorrectly, but show the color
                call print_bg_rgb_color(r, g, b, '    ')
                write(*, '(A,I3,A,I3,A,I3,A)') ' RGB(', r, ',', g, ',', b, ') [max value]'
            else
                write(*, '(A,I20,A)', advance='no') '  ', val, ': '
                call print_bg_rgb_color(r, g, b, '    ')
                write(*, '(A,I3,A,I3,A,I3,A)') ' RGB(', r, ',', g, ',', b, ')'
            end if
        end do
    end subroutine demonstrate_64bit
    
end program BasicConcepts

