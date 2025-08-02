program main
   implicit none
   
   ! Note the structured example is assuming zero based indexing.
   ! One based index languages will differ.
   ! Fortran uses 1-based indexing by default, but we can specify other ranges
   ! Also note how some references are array location, with ranges from first to last,
   ! while indexes being zero based are 0 to size - 1 in most languages but 1 to size in Fortran.

   ! Array declarations with Fortran's default 1-based indexing
   integer, dimension( 24 ) :: temperatures = [ 55, 58, 60, 65, 70, 73, 76, 79, 81, 83, 84, 85, &
                                                 85, 84, 83, 81, 78, 75, 72, 69, 65, 62, 59, 57 ]
   integer, dimension( 6 ) :: testScores = [ 95, 75, 86, 86, 78, 94 ]
   integer, dimension( 5 ) :: bookNumber = [ 12495, 35786, 15863, 84962, 42697 ]

   ! Variables for indexing
   integer :: bookTwoIndex = 2
   integer :: hourCount = 24
   integer :: firstTemperatureIndex = 1
   integer :: lastTemperatureIndex
   integer :: bookIndex = 3
   integer :: largeArraySize = 10000

   ! Large arrays with default 1-based indexing
   logical, dimension( 10000 ) :: largeArray
   integer, dimension( 1000 ) :: largeArray1
   real, dimension( 5000 ) :: largeArray2

   ! Character arrays with default 1-based indexing
   character(len=1), dimension( 100 ) :: myString
   character(len=1), dimension( 13 ) :: myString1 = [ 'H', 'e', 'l', 'l', 'o', ' ', 'W', 'o', 'r', 'l', 'd', '.', char(0) ]
   
   ! Note: Fortran strings are different from character arrays
   character(len=12) :: myString2 = "Hello World."

   ! 2D Array with default 1-based indexing
   character(len=1), dimension( 4, 4 ) :: twoDArray

   ! Color constants
   integer, parameter :: RED = 0
   integer, parameter :: GREEN = 1
   integer, parameter :: BLUE = 2
   integer, parameter :: YELLOW = 3
   integer, parameter :: CYAN = 4
   integer, parameter :: MAGENTA = 5
   integer, parameter :: WHITE = 6

   ! Color table
   integer, dimension( 0:6, 0:2 ) :: colorTable = reshape( [ &
      255, 0,   0,   &  ! Red
      0,   255, 0,   &  ! Green
      0,   0,   255, &  ! Blue
      255, 255, 0,   &  ! Yellow = Red + Green
      0,   255, 255, &  ! Cyan   = Green + Blue
      255, 0,   255, &  ! Magenta = Red + Blue
      255, 255, 255  &  ! White = Red + Green + Blue
   ], [7, 3] )

   ! Loop variables
   integer :: i, j, nullIndex

   lastTemperatureIndex = hourCount

   ! 10th entry (1-based index 10)
   write(*,'(A,I0)') "Temperature at tenth hour is ", temperatures( 10 )
   
   ! 4th entry (1-based index 4)
   write(*,'(A,I0)') "Fourth student grade is ", testScores( 4 )

   ! 2nd entry (1-based index 2)
   write(*,'(A,I0)') "Second book index is ", bookNumber( bookTwoIndex )

   ! First and last 1 based indexes, 1 and array size.
   write(*,'(A,I0)') "First temperature is ", temperatures( firstTemperatureIndex )
   write(*,'(A,I0)') "Last temperature is ", temperatures( lastTemperatureIndex )

   ! set temperature first entry to 65
   temperatures( 1 ) = 65
   write(*,'(A,I0)') "First temperature is now ", temperatures( 1 )

   ! set testScores fourth entry to 99
   testScores( 4 ) = 99
   write(*,'(A,I0)') "Fourth test score is now ", testScores( 4 )

   ! set bookNumber at index third entry to 75681
   bookNumber( bookIndex ) = 75681
   write(*,'(A,I0)') "Third book number is now ", bookNumber( bookIndex )

   ! Large arrays are automatically initialized to default values
   write(*,'(A,L1,A,L1)') "First large array first and last initial values: ", largeArray( 1 ), " ", &
                          largeArray( largeArraySize )
   write(*,'(A,I0,A,I0)') "Second large array first and last initial values: ", largeArray1( 1 ), " ", &
                          largeArray1( 1000 )
   write(*,'(A,F3.1,A,F5.2)') "Third large array first and last initial values: ", largeArray2( 1 ), " ", &
                               largeArray2( 5000 )

   ! set largeArray first entry to true
   largeArray( 1 ) = .true.
   ! set largeArray last entry to false
   largeArray( largeArraySize ) = .false.
   write(*,'(A,L1,A,L1)') "First large array first and last values: ", largeArray( 1 ), " ", &
                          largeArray( largeArraySize )

   ! set largeArray1 first entry to 25
   largeArray1( 1 ) = 25
   ! set largeArray1 last entry to 55
   largeArray1( 1000 ) = 55
   write(*,'(A,I0,A,I0)') "Second large array first and last values: ", largeArray1( 1 ), " ", &
                          largeArray1( 1000 )

   ! set largeArray2 first entry to 27.5
   largeArray2( 1 ) = 27.5
   ! set largeArray2 last entry to 58.25
   largeArray2( 5000 ) = 58.25
   write(*,'(A,F3.1,A,F5.2)') "Third large array first and last values: ", largeArray2( 1 ), " ", &
                               largeArray2( 5000 )

   ! Character array (Fortran style)
   myString( 1 ) = 'H'
   myString( 2 ) = 'e'
   myString( 3 ) = 'l'
   myString( 4 ) = 'l'
   myString( 5 ) = 'o'
   myString( 6 ) = ' '
   myString( 7 ) = 'W'
   myString( 8 ) = 'o'
   myString( 9 ) = 'r'
   myString( 10 ) = 'l'
   myString( 11 ) = 'd'
   myString( 12 ) = '.'
   myString( 13 ) = char( 0 )
   
   ! Print up to null terminator
   nullIndex = 0
   do i = 1, 100
      if ( myString( i ) == char( 0 ) ) then
         nullIndex = i
         exit
      end if
   end do
   write(*,'(*(A1))') ( myString( i ), i = 1, nullIndex - 1 )

   ! Print second string
   nullIndex = 0
   do i = 1, 13
      if ( myString1( i ) == char( 0 ) ) then
         nullIndex = i
         exit
      end if
   end do
   write(*,'(*(A1))') ( myString1( i ), i = 1, nullIndex - 1 )

   ! Note the null terminator is not needed in Fortran strings
   write(*,'(A)') myString2

   ! 2D Array
   twoDArray( 1, 1 ) = '0'
   twoDArray( 1, 2 ) = '1'
   twoDArray( 1, 3 ) = '2'
   twoDArray( 1, 4 ) = '3'
   twoDArray( 2, 1 ) = '4'
   twoDArray( 2, 2 ) = '5'
   twoDArray( 2, 3 ) = '6'
   twoDArray( 2, 4 ) = '7'
   twoDArray( 3, 1 ) = '8'
   twoDArray( 3, 2 ) = '9'
   twoDArray( 3, 3 ) = 'A'
   twoDArray( 3, 4 ) = 'B'
   twoDArray( 4, 1 ) = 'C'
   twoDArray( 4, 2 ) = 'D'
   twoDArray( 4, 3 ) = 'E'
   twoDArray( 4, 4 ) = 'F'

   ! Note: the actual implementation of this code will use some advanced
   ! techniques that will not be described, only the results of the code observed.
   write(*,'(A)', advance='no') "twoDArray memory location as flat data: "
   do i = 1, 4
      do j = 1, 4
         write(*,'(A1)', advance='no') twoDArray( i, j )
      end do
   end do
   write(*,*)

   write(*,'(A,I0,A,I0,A,I0)') "CYAN color values: ", colorTable( CYAN, 0 ), " ", &
                               colorTable( CYAN, 1 ), " ", colorTable( CYAN, 2 )

end program main
