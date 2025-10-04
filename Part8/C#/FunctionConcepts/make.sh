#!/bin/bash

FileNames=("FunctionConcepts_1" "FunctionConcepts_2" "FunctionConcepts_3" "FunctionConcepts_4" "FunctionConcepts_5" "FunctionConcepts_6" "FunctionConcepts_7" "FunctionConcepts_8" "FunctionConcepts_9" "FunctionConcepts_10" "FunctionConcepts_11" "FunctionConcepts_12" "FunctionConcepts_13" "FunctionConcepts_14" "CompilerErrors")

for fileName in "${FileNames[@]}"; do

    rm -r $fileName.exe
    mcs -out:$fileName.exe $fileName.cs

    if [ $? -ne 0 ]; then
        echo "Build failed for ./$fileName.exe"
    else
        echo "Build successful. Run with mono ./$fileName.exe"
    fi

done
