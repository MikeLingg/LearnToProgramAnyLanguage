#!/bin/bash

FileNames=("BasicConcepts" "BasicConcepts_InvalidASCII" "BasicConcepts_InvalidNames" "BasicConcepts_MixedTypes" "BasicConcepts_Redeclaration" "BasicConcepts_Uninitialized")

for fileName in "${FileNames[@]}"; do

    rm -r $fileName.exe
    mcs -out:$fileName.exe $fileName.cs

    if [ $? -ne 0 ]; then
        echo "Build failed for ./$fileName.exe"
    else
        echo "Build successful. Run with mono ./$fileName.exe"
    fi

done
