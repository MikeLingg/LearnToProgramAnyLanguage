#!/bin/bash

FileNames=("BranchConcepts" "CompilerErrors")

for fileName in "${FileNames[@]}"; do

    rm -f $fileName.exe
    go build -o $fileName.exe $fileName.go

    if [ $? -ne 0 ]; then
        echo "Build failed for ./$fileName.exe"
    else
        echo "Build successful. Run with ./$fileName.exe"
    fi

done
