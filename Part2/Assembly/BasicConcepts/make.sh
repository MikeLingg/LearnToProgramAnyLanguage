#!/bin/bash

FileNames=("BasicConcepts" "UndefinedLabel")

for fileName in "${FileNames[@]}"; do

    rm -r $fileName.exe

    # Assemble
    nasm -f elf64 $fileName.asm -o $fileName.o
    if [ $? -ne 0 ]; then
        echo "Assembly failed."
        exit 1
    fi

    # Link
    gcc -no-pie $fileName.o -o $fileName.exe
    if [ $? -ne 0 ]; then
        echo "Linking failed for ./$fileName.exe"
    else
    echo "Build successful. Run with ./$fileName.exe"
    fi

done
