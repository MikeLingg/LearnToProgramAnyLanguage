; PrintMaze.asm - Simple string output demonstration
; Assemble with: nasm -f elf64 PrintMaze.asm -o PrintMaze.o  
; Link with: ld PrintMaze.o -o PrintMaze.exe

section .data
    ; Maze strings stored as byte sequences
    maze_line1  db '+-+-+-+', 10, 0     ; 10 = newline (LF)
    maze_line2  db '| | | |', 10, 0
    maze_line3  db '+-+-+-+', 10, 0
    maze_line4  db '| | | |', 10, 0  
    maze_line5  db '+-+-+-+', 10, 0
    maze_line6  db '| | | |', 10, 0
    maze_line7  db '+-+-+-+', 10, 0
    
    ; String lengths (including newline, excluding null terminator)
    line_len    equ 8

section .text
    global _start

_start:
    ; Print first line: +-+-+-+
    mov rax, 1          ; sys_write system call
    mov rdi, 1          ; stdout file descriptor
    mov rsi, maze_line1 ; address of string
    mov rdx, line_len   ; number of bytes to write
    syscall             ; invoke system call
    
    ; Print second line: | | | |
    mov rax, 1
    mov rdi, 1
    mov rsi, maze_line2
    mov rdx, line_len
    syscall
    
    ; Print third line: +-+-+-+
    mov rax, 1
    mov rdi, 1
    mov rsi, maze_line3
    mov rdx, line_len
    syscall
    
    ; Print fourth line: | | | |
    mov rax, 1
    mov rdi, 1
    mov rsi, maze_line4
    mov rdx, line_len
    syscall
    
    ; Print fifth line: +-+-+-+
    mov rax, 1
    mov rdi, 1
    mov rsi, maze_line5
    mov rdx, line_len
    syscall
    
    ; Print sixth line: | | | |
    mov rax, 1
    mov rdi, 1
    mov rsi, maze_line6
    mov rdx, line_len
    syscall
    
    ; Print seventh line: +-+-+-+
    mov rax, 1
    mov rdi, 1
    mov rsi, maze_line7
    mov rdx, line_len
    syscall
    
    ; Exit program successfully
    mov rax, 60         ; sys_exit system call
    mov rdi, 0          ; exit status (0 = success)
    syscall             ; invoke system call
