#!/bin/bash

FileNames=("Statements" "CompilerErrors")

for fileName in "${FileNames[@]}"; do

    rm -f $fileName.exe
    rustc $fileName.rs -o $fileName.exe || true

    if [ $? -ne 0 ]; then
        echo "Build failed for ./$fileName.exe"
    else
        echo "Build successful. Run with ./$fileName.exe"
    fi

done
