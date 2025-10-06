#!/bin/bash

FileNames=("BasicConcepts" "BasicConcepts_InvalidASCII" "BasicConcepts_InvalidNames" "BasicConcepts_MixedTypes" "BasicConcepts_Redeclaration" "BasicConcepts_Uninitialized")

for fileName in "${FileNames[@]}"; do

    rm -f $fileName.exe
    g++ -Wall -Wextra -O2 -o $fileName.exe $fileName.cpp

    if [ $? -ne 0 ]; then
        echo "Build failed for ./$fileName.exe"
    else
        echo "Build successful. Run with ./$fileName.exe"
    fi

done
