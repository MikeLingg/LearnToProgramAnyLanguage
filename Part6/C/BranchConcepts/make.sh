#!/bin/bash

FileNames=("BranchConcepts")

for fileName in "${FileNames[@]}"; do

    rm -f $fileName.exe
    gcc -Wall -Wextra -O2 -o $fileName.exe $fileName.c

    if [ $? -ne 0 ]; then
        echo "Build failed for ./$fileName.exe"
    else
        echo "Build successful. Run with ./$fileName.exe"
    fi

done
