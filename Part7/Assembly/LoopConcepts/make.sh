#!/bin/bash

FileNames=("LoopConcepts")

for fileName in "${FileNames[@]}"; do

    rm -r $fileName.exe

    # Assemble
    nasm -f elf64 $fileName.asm -o $fileName.o
    if [ $? -ne 0 ]; then
        echo "Assembly failed."
        exit 1
    fi

    # Link
    ld $fileName.o -o $fileName.exe -lc -lm --dynamic-linker=/lib64/ld-linux-x86-64.so.2
    if [ $? -ne 0 ]; then
        echo "Linking failed for ./$fileName.exe"
    else
    echo "Build successful. Run with ./$fileName.exe"
    fi

done
