#!/bin/bash

rm -r BasicConcepts.exe

# Assemble
nasm -f elf64 BasicConcepts.asm -o BasicConcepts.o
if [ $? -ne 0 ]; then
    echo "Assembly failed."
    exit 1
fi

# Link
ld BasicConcepts.o -o BasicConcepts.exe
if [ $? -ne 0 ]; then
    echo "Linking failed."
    exit 1
fi

echo "Build successful. Run with ./BasicConcepts.exe"

rm -r UndefinedLabel.exe

# Assemble
nasm -f elf64 UndefinedLabel.asm -o UndefinedLabel.o
if [ $? -ne 0 ]; then
    echo "Assembly failed."
    exit 1
fi

# Link
ld UndefinedLabel.o -o UndefinedLabel.exe
if [ $? -ne 0 ]; then
    echo "Linking failed."
    exit 1
fi

echo "Build successful. Run with ./UndefinedLabel.exe"
