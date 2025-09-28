#!/bin/bash

FileNames=("SystemCalls" "Function_AssignmentOutOfPlace" "Function_ExtraParameter" "Function_ReturnReversed" "Function_IngoredReturn" "Function_IncorrectCapitalization" "Function_IncorrectFunctionName" "Function_MissingParameter" "Function_UsedEquivalance")

for fileName in "${FileNames[@]}"; do

    rm -f $fileName.exe
    gcc -Wall -Wextra -O2 -o $fileName.exe $fileName.c

    if [ $? -ne 0 ]; then
        echo "Build failed for ./$fileName.exe"
    else
        echo "Build successful. Run with ./$fileName.exe"
    fi

done
