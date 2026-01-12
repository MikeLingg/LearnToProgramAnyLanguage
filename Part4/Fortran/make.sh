#!/bin/bash

FileNames=("StandardCalls" "Function_AssignmentOutOfPlace" "Function_ExtraParameter" "Function_FunctionReturnReversed" "Function_IgnoredReturn" "Function_IncorrectCapitalization" "Function_IncorrectFunctionName" "Function_MissingParameter" "Function_UsedEquivalance")

for fileName in "${FileNames[@]}"; do

    rm -f $fileName.exe
    gfortran -o $fileName.exe $fileName.f90

    if [ $? -ne 0 ]; then
        echo "Build failed for ./$fileName.exe"
    else
        echo "Build successful. Run with ./$fileName.exe"
    fi

done
