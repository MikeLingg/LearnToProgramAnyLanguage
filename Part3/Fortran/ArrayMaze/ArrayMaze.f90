program main
   implicit none
   
   ! This maze program does not necessarily progress the program, 
   ! but provides a construct we will use eventually in the program.
   
   character(len=1), dimension( 5, 3 ) :: mazeWalls
   integer, parameter :: LEFT = 1
   integer, parameter :: UP = 2
   integer, parameter :: RIGHT = 3
   integer, parameter :: DOWN = 4
   integer, dimension( 9, 4 ) :: neighborLookup
   character(len=1), dimension( 7 ) :: topBottomRow
   
   ! Initialize mazeWalls
   mazeWalls( 1, : ) = [ '|', ' ', char( 0 ) ]
   mazeWalls( 2, : ) = [ ' ', ' ', ' ' ]
   mazeWalls( 3, : ) = [ ' ', '|', char( 0 ) ]
   mazeWalls( 4, : ) = [ ' ', '-', ' ' ]
   mazeWalls( 5, : ) = [ '|', ' ', char( 0 ) ]
   
   ! Initialize neighborLookup
   neighborLookup( 1, : ) = [ -1, -1, -1, 3 ]  ! Cell 0
   neighborLookup( 2, : ) = [ -1, -1, 2, 4 ]   ! Cell 1
   neighborLookup( 3, : ) = [ 1, -1, -1, 5 ]   ! Cell 2
   neighborLookup( 4, : ) = [ -1, 0, 4, 6 ]    ! Cell 3
   neighborLookup( 5, : ) = [ 3, 1, -1, -1 ]   ! Cell 4
   neighborLookup( 6, : ) = [ -1, 2, -1, 8 ]   ! Cell 5
   neighborLookup( 7, : ) = [ -1, 3, -1, -1 ]  ! Cell 6
   neighborLookup( 8, : ) = [ -1, -1, 8, -1 ]  ! Cell 7
   neighborLookup( 9, : ) = [ 7, 5, -1, -1 ]   ! Cell 8
   
   ! Initialize topBottomRow
   topBottomRow = [ '+', '-', '+', '-', '+', '-', '+' ]
   
   ! This sort of code will look better when we have loops and functions
   ! We see how a 3x3 maze is created from the array of data, and using the 
   ! neighbor lookup provides -1 where a wall is in that direction, otherwise 
   ! provides the cell number in that direction.
   write( *,'(A1)', advance='no' ) topBottomRow( 1 )
   write( *,'(A1)', advance='no' ) topBottomRow( 2 )
   write( *,'(A1)', advance='no' ) topBottomRow( 3 )
   write( *,'(A1)', advance='no' ) topBottomRow( 4 )
   write( *,'(A1)', advance='no' ) topBottomRow( 5 )
   write( *,'(A1)', advance='no' ) topBottomRow( 6 )
   write( *,'(A1)', advance='no' ) topBottomRow( 7 )
   write( *,* )
   
   write( *,'(A)', advance='no' ) "|0"
   write( *,'(A1)', advance='no' ) mazeWalls( 1, 1 )
   write( *,'(A)', advance='no' ) "1"
   write( *,'(A1)', advance='no' ) mazeWalls( 1, 2 )
   write( *,'(A)', advance='no' ) "2|"
   write( *,* )
   
   write( *,'(A)', advance='no' ) "+"
   write( *,'(A1)', advance='no' ) mazeWalls( 2, 1 )
   write( *,'(A)', advance='no' ) "+"
   write( *,'(A1)', advance='no' ) mazeWalls( 2, 2 )
   write( *,'(A)', advance='no' ) "+"
   write( *,'(A1)', advance='no' ) mazeWalls( 2, 3 )
   write( *,'(A)', advance='no' ) "+"
   write( *,* )
   
   write( *,'(A)', advance='no' ) "|3"
   write( *,'(A1)', advance='no' ) mazeWalls( 3, 1 )
   write( *,'(A)', advance='no' ) "4"
   write( *,'(A1)', advance='no' ) mazeWalls( 3, 2 )
   write( *,'(A)', advance='no' ) "5|"
   write( *,* )
   
   write( *,'(A)', advance='no' ) "+"
   write( *,'(A1)', advance='no' ) mazeWalls( 4, 1 )
   write( *,'(A)', advance='no' ) "+"
   write( *,'(A1)', advance='no' ) mazeWalls( 4, 2 )
   write( *,'(A)', advance='no' ) "+"
   write( *,'(A1)', advance='no' ) mazeWalls( 4, 3 )
   write( *,'(A)', advance='no' ) "+"
   write( *,* )
   
   write( *,'(A)', advance='no' ) "|6"
   write( *,'(A1)', advance='no' ) mazeWalls( 5, 1 )
   write( *,'(A)', advance='no' ) "7"
   write( *,'(A1)', advance='no' ) mazeWalls( 5, 2 )
   write( *,'(A)', advance='no' ) "8|"
   write( *,* )
   
   write( *,'(A1)', advance='no' ) topBottomRow( 1 )
   write( *,'(A1)', advance='no' ) topBottomRow( 2 )
   write( *,'(A1)', advance='no' ) topBottomRow( 3 )
   write( *,'(A1)', advance='no' ) topBottomRow( 4 )
   write( *,'(A1)', advance='no' ) topBottomRow( 5 )
   write( *,'(A1)', advance='no' ) topBottomRow( 6 )
   write( *,'(A1)', advance='no' ) topBottomRow( 7 )
   write( *,* )
   
   ! So take cell number 4 and see what rooms are around it, 
   ! cell 3 is to the left and cell 1 is up, but walls are right and down.
   write( *,'(A,I0)' ) "Cell to left of cell 4 is: ", neighborLookup( 5, LEFT )
   write( *,'(A,I0)' ) "Cell to up of cell 4 is: ", neighborLookup( 5, UP )
   write( *,'(A,I0)' ) "Cell to right of cell 4 is: ", neighborLookup( 5, RIGHT )
   write( *,'(A,I0)' ) "Cell to down of cell 4 is: ", neighborLookup( 5, DOWN )

end program main
