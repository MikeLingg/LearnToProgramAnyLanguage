#!/bin/bash

rm -r ArrayMaze.exe

# Assemble
nasm -f elf64 ArrayMaze.asm -o ArrayMaze.o
if [ $? -ne 0 ]; then
    echo "Assembly failed."
    exit 1
fi

# Link
ld ArrayMaze.o -lc --dynamic-linker /lib64/ld-linux-x86-64.so.2 -o ArrayMaze
if [ $? -ne 0 ]; then
    echo "Linking failed."
    exit 1
fi

echo "Build successful. Run with ./ArrayMaze.exe"

