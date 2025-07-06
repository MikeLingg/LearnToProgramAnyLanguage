#!/bin/bash

rm -r PrintMaze.exe

# Assemble
nasm -f elf64 PrintMaze.asm -o PrintMaze.o
if [ $? -ne 0 ]; then
    echo "Assembly failed."
    exit 1
fi

# Link
ld PrintMaze.o -o PrintMaze.exe
if [ $? -ne 0 ]; then
    echo "Linking failed."
    exit 1
fi

echo "Build successful. Run with ./PrintMaze.exe"