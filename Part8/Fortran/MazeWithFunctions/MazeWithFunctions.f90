program maze_generator
    implicit none
    
    ! Define directions
    integer, parameter :: LEFT = 0
    integer, parameter :: UP = 1
    integer, parameter :: RIGHT = 2
    integer, parameter :: DOWN = 3
    
    integer, parameter :: FIRST_CELL = 1
    integer, parameter :: SECOND_CELL = 2
    
    integer, parameter :: NO_CELL = -1
    
    ! Global variables - now allocatable
    logical, allocatable :: all_horizontal_walls_up_glob(:)
    logical, allocatable :: walls_up_glob(:)
    integer :: interior_wall_count_glob
    integer :: maze_rows_glob
    integer :: maze_columns_glob
    
    call main_program()
    
contains



    ! +-+-+-+
    ! Prints a horizontal wall, similar to the example above, 
    ! with walls down based on the provided parameters.
    subroutine print_horizontal_walls ( horizontal_walls_up_par )
        implicit none
        logical, intent(in) :: horizontal_walls_up_par(:)
        integer :: horizontal_wall_index
        
        write(*,'(A)', advance='no') '+'
        do horizontal_wall_index = 1, maze_columns_glob
            if ( horizontal_walls_up_par(horizontal_wall_index) .eqv. .true. ) then
                write(*,'(A)', advance='no') '-'
            else
                write(*,'(A)', advance='no') ' '
            end if
            write(*,'(A)', advance='no') '+'
        end do
        write(*,*)
    end subroutine print_horizontal_walls



    ! +-+-+-+
    ! Prints a horizontal wall, similar to the example above, with all walls up.
    subroutine print_horizontal_walls_all()
        implicit none
        call print_horizontal_walls ( all_horizontal_walls_up_glob )
    end subroutine print_horizontal_walls_all



    ! | | | |
    ! Prints a vertical wall, similar to the example above, 
    ! with walls down based on the provided parameters.
    subroutine print_vertical_walls ( vertical_walls_up_par )
        implicit none
        logical, intent(in) :: vertical_walls_up_par(:)
        integer :: vertical_wall_index
        
        ! First wall is an exterior wall, always up.
        write(*,'(A)', advance='no') '|'
        do vertical_wall_index = 1, maze_columns_glob - 1
            write(*,'(A)', advance='no') ' '
            if ( vertical_walls_up_par(vertical_wall_index) .eqv. .true. ) then
                write(*,'(A)', advance='no') '|'
            else
                write(*,'(A)', advance='no') ' '
            end if
        end do
        ! Last wall exterior, always up.
        write(*,'(A)', advance='no') ' '
        write(*,'(A)', advance='no') '|'
        write(*,*)
    end subroutine print_vertical_walls



    ! Loop through the rows of the maze and print the maze 
    ! based on walls_up_glob
    subroutine print_maze()
        implicit none
        integer :: interior_wall_index
        integer :: row_index
        integer :: column_index
        logical, allocatable :: vertical_walls_up(:)
        logical, allocatable :: horizontal_walls_up(:)
        
        interior_wall_index = 1
        
        ! First row is exterior walls
        call print_horizontal_walls_all()
        do row_index = 1, maze_rows_glob
            allocate(vertical_walls_up(maze_columns_glob - 1))
            do column_index = 1, maze_columns_glob - 1
                vertical_walls_up(column_index) = walls_up_glob(interior_wall_index)
                interior_wall_index = interior_wall_index + 1
            end do
            
            call print_vertical_walls ( vertical_walls_up )
            deallocate(vertical_walls_up)
            
            if ( row_index .eq. maze_rows_glob ) then
                call print_horizontal_walls_all()
            else
                allocate(horizontal_walls_up(maze_columns_glob))
                do column_index = 1, maze_columns_glob
                    horizontal_walls_up(column_index) = walls_up_glob(interior_wall_index)
                    interior_wall_index = interior_wall_index + 1
                end do
                
                call print_horizontal_walls ( horizontal_walls_up )
                deallocate(horizontal_walls_up)
            end if
        end do
        
        write(*,*)
    end subroutine print_maze



    ! Simple sleep function
    subroutine sleep_half_second()
        implicit none
        ! Use standard sleep (1 second since half-second not available)
        call sleep(1)
    end subroutine sleep_half_second



    ! Kruskal's algorithm.
    subroutine build_maze_kruskal()
        implicit none
        integer, allocatable :: wall_connections(:,:)
        integer, allocatable :: wall_remove_list(:)
        integer, allocatable :: cell_to_group(:)
        integer, allocatable :: group_cells(:,:)
        integer :: wall_index
        integer :: row_index
        integer :: first_cell_in_row
        integer :: vertical_wall_index
        integer :: horizontal_wall_index
        integer :: left_cell, right_cell
        integer :: upper_cell, lower_cell
        integer :: cell_index
        integer :: inner_cell_index
        logical :: maze_complete
        integer :: wall_index_loop
        integer :: i, j, temp
        real :: random_val
        integer :: remove_wall_index
        integer :: next_wall_to_check
        integer :: first_cell_index
        integer :: first_cell_group_index
        integer :: second_cell_index
        integer :: second_cell_group_index
        integer :: next_empty_first_group_index
        integer :: group_cell_index
        integer :: cell_to_move
        integer :: total_cells
        integer :: iostat_val
        
        total_cells = maze_rows_glob * maze_columns_glob
        
        ! Allocate all arrays with exact sizes needed
        allocate(wall_connections(interior_wall_count_glob, 2), stat=iostat_val)
        if (iostat_val /= 0) stop 'Error allocating wall_connections'
        
        allocate(wall_remove_list(interior_wall_count_glob), stat=iostat_val)
        if (iostat_val /= 0) stop 'Error allocating wall_remove_list'
        
        allocate(cell_to_group(total_cells), stat=iostat_val)
        if (iostat_val /= 0) stop 'Error allocating cell_to_group'
        
        allocate(group_cells(total_cells, total_cells), stat=iostat_val)
        if (iostat_val /= 0) stop 'Error allocating group_cells'
        
        wall_index = 1
        do row_index = 1, maze_rows_glob
            ! Track the first cell in the current row
            first_cell_in_row = (row_index - 1) * maze_columns_glob + 1
            
            ! Note the 1..maze_columns_glob - 1, one less vertical wall
            ! than the number of columns.
            do vertical_wall_index = 1, maze_columns_glob - 1
                left_cell = first_cell_in_row + vertical_wall_index - 1
                right_cell = left_cell + 1
                wall_connections(wall_index, FIRST_CELL) = left_cell
                wall_connections(wall_index, SECOND_CELL) = right_cell
                wall_index = wall_index + 1
            end do
            
            ! The last row will have no interior horizontal walls below
            ! it, so will be skipped.
            if ( wall_index .le. interior_wall_count_glob ) then
                do horizontal_wall_index = 1, maze_columns_glob
                    upper_cell = first_cell_in_row + horizontal_wall_index - 1
                    lower_cell = upper_cell + maze_columns_glob
                    wall_connections(wall_index, FIRST_CELL) = upper_cell
                    wall_connections(wall_index, SECOND_CELL) = lower_cell
                    wall_index = wall_index + 1
                end do
            end if
        end do
        
        do cell_index = 1, total_cells
            cell_to_group(cell_index) = cell_index
            
            do inner_cell_index = 1, total_cells
                if ( inner_cell_index .eq. 1 ) then
                    group_cells(cell_index, inner_cell_index) = cell_index
                else
                    group_cells(cell_index, inner_cell_index) = NO_CELL
                end if
            end do
        end do
        
        maze_complete = .false.
        
        do wall_index_loop = 1, interior_wall_count_glob
            wall_remove_list(wall_index_loop) = wall_index_loop
        end do
        
        ! Fisher-Yates shuffle
        do i = interior_wall_count_glob, 2, -1
            call random_number(random_val)
            j = int(random_val * real(i)) + 1
            if ( j .ge. 1 .and. j .le. i ) then
                temp = wall_remove_list(i)
                wall_remove_list(i) = wall_remove_list(j)
                wall_remove_list(j) = temp
            end if
        end do
        
        ! Perform Kruskal's algorithm.
        do remove_wall_index = 1, interior_wall_count_glob
            next_wall_to_check = wall_remove_list(remove_wall_index)
            
            ! If the two cells connected to this wall are not part 
            ! of the same group, remove the wall and merge the 
            ! groups.
            first_cell_index = wall_connections(next_wall_to_check, FIRST_CELL)
            first_cell_group_index = cell_to_group(first_cell_index)
            second_cell_index = wall_connections(next_wall_to_check, SECOND_CELL)
            second_cell_group_index = cell_to_group(second_cell_index)
            if ( first_cell_group_index .ne. second_cell_group_index ) then
                walls_up_glob(next_wall_to_check) = .false.
                
                ! Loop through the indices of all cells in the first 
                ! group until we find a NO_CELL indicating no cell here.
                next_empty_first_group_index = 1
                do cell_index = 1, total_cells
                    if ( group_cells(first_cell_group_index, cell_index) .eq. NO_CELL ) then
                        next_empty_first_group_index = cell_index
                        exit
                    end if
                end do
                
                ! Loop through the indices of all cells in the second group,
                ! move each cell to the first group, and set that cell's 
                ! group to the first group index.
                do group_cell_index = total_cells, 1, -1
                    ! Skip until we reach valid cells
                    if ( group_cells(second_cell_group_index, group_cell_index) .ne. NO_CELL ) then
                        ! Get the id number of the cell to move from 
                        ! the second group to the first group
                        cell_to_move = group_cells(second_cell_group_index, group_cell_index)
                        
                        ! Move the cell number from the second group 
                        ! to the first group
                        group_cells(first_cell_group_index, next_empty_first_group_index) = cell_to_move
                        ! Move our empty index to the next cell in this array.
                        next_empty_first_group_index = next_empty_first_group_index + 1
                        ! Mark this cell as part of the first group.
                        cell_to_group(cell_to_move) = first_cell_group_index
                        ! Remove the cell from the second group (set the
                        ! array entry to NO_CELL)
                        group_cells(second_cell_group_index, group_cell_index) = NO_CELL
                        
                        if ( next_empty_first_group_index .ge. total_cells ) then
                            maze_complete = .true.
                        end if
                    end if
                end do
                
                call sleep_half_second()
                
                call print_maze()
                
                if ( maze_complete .eqv. .true. ) then
                    exit
                end if
            end if
        end do
        
        ! Clean up allocated arrays
        deallocate(wall_connections)
        deallocate(wall_remove_list)
        deallocate(cell_to_group)
        deallocate(group_cells)
    end subroutine build_maze_kruskal



    subroutine main_program()
        implicit none
        character(len=20) :: user_input
        integer :: temp_value
        integer :: ios
        integer :: i
        integer :: iostat_val
        
        ! Initialize random seed
        call random_seed()
        
        ! Prompt the user for maze size
        maze_columns_glob = 0
        do while ( maze_columns_glob .le. 0 )
            write(*,'(A)', advance='no') 'Please enter number of columns for maze, must be greater than 1: '
            read(*,'(A)') user_input
            read(user_input, *, iostat=ios) temp_value
            if ( ios .eq. 0 ) then
                if ( temp_value .gt. 1 ) then
                    maze_columns_glob = temp_value
                end if
            end if
        end do
        
        maze_rows_glob = 0
        do while ( maze_rows_glob .le. 0 )
            write(*,'(A)', advance='no') 'Please enter number of rows for maze, must be greater than 1: '
            read(*,'(A)') user_input
            read(user_input, *, iostat=ios) temp_value
            if ( ios .eq. 0 ) then
                if ( temp_value .gt. 1 ) then
                    maze_rows_glob = temp_value
                end if
            end if
        end do
        
        ! Calculate interior wall count
        interior_wall_count_glob = maze_rows_glob * ( maze_columns_glob - 1 ) + ( maze_rows_glob - 1 ) * maze_columns_glob
        
        ! Allocate global arrays with exact sizes needed
        allocate(all_horizontal_walls_up_glob(maze_columns_glob), stat=iostat_val)
        if (iostat_val /= 0) stop 'Error allocating all_horizontal_walls_up_glob'
        
        allocate(walls_up_glob(interior_wall_count_glob), stat=iostat_val)
        if (iostat_val /= 0) stop 'Error allocating walls_up_glob'
        
        ! Setup maze datastructures for the user entered size.
        do i = 1, maze_columns_glob
            all_horizontal_walls_up_glob(i) = .true.
        end do
        
        do i = 1, interior_wall_count_glob
            walls_up_glob(i) = .true.
        end do
        
        call build_maze_kruskal()
        
        ! Clean up global arrays
        deallocate(all_horizontal_walls_up_glob)
        deallocate(walls_up_glob)
    end subroutine main_program

end program maze_generator
