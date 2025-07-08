#!/bin/bash

rm -r SystemCalls.exe

# Assemble
nasm -f elf64 SystemCalls.asm -o SystemCalls.o
if [ $? -ne 0 ]; then
    echo "Assembly failed."
    exit 1
fi

# Link
ld SystemCalls.o -o SystemCalls.exe -lc -lm --dynamic-linker=/lib64/ld-linux-x86-64.so.2
if [ $? -ne 0 ]; then
    echo "Linking failed."
    exit 1
fi

echo "Build successful. Run with ./SystemCalls.exe"