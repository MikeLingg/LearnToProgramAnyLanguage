#!/bin/bash

FileNames=("BasicConcepts" "BasicConcepts_ArrayOutOfBounds" "CompileErrors")

for fileName in "${FileNames[@]}"; do

    rm -r $fileName.exe
    mcs /unsafe -out:$fileName.exe $fileName.cs

    if [ $? -ne 0 ]; then
        echo "Build failed for ./$fileName.exe"
    else
        echo "Build successful. Run with mono ./$fileName.exe"
    fi

done
