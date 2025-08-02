#!/bin/bash

rm -r BasicConcepts.exe

# Assemble
nasm -f elf64 BasicConcepts.asm -o BasicConcepts.o
if [ $? -ne 0 ]; then
    echo "Assembly failed."
    exit 1
fi

# Link
ld BasicConcepts.o -lc --dynamic-linker /lib64/ld-linux-x86-64.so.2 -o BasicConcepts
if [ $? -ne 0 ]; then
    echo "Linking failed."
    exit 1
fi

echo "Build successful. Run with ./BasicConcepts.exe"

