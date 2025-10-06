program BasicConcepts
    implicit none
    
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

end program BasicConcepts

