program main
   implicit none
   
   integer :: outOfBoundsIndex

   ! Test derived type for demonstrating bounds checking
   type :: testStruct
      integer, dimension( 10 ) :: intArray
      integer :: myInt
   end type testStruct
   
   type( testStruct ) :: testStructInstance

   ! Initialize test struct
   testStructInstance%intArray = 0
   testStructInstance%myInt = 0

   outOfBoundsIndex = 10
   outOfBoundsIndex = outOfBoundsIndex + 1

   ! In Fortran, buffer overflows are prevented by bounds checking (when enabled)
   ! This code demonstrates what would happen
   ! The following will raise a runtime error in Fortran with bounds checking:
   testStructInstance%intArray( outOfBoundsIndex ) = 55  ! This would raise error

   write(*,'(A,I0)') "testStructInstance%intArray( 11 ): ", testStructInstance%intArray( outOfBoundsIndex )

   write(*,'(A,I0)') "myInt value: ", testStructInstance%myInt

   testStructInstance%myInt = testStructInstance%myInt + 1
   write(*,'(A,I0)') "Out of bounds array: ", testStructInstance%intArray( outOfBoundsIndex )

end program main
