#!/bin/bash

FileNames=("SystemCalls" "Function_AssignmentOutOfPlace" "Function_ExtraParameters" "Function_FunctionReturnReversed" "Function_IgnoredReturn" "Function_IncorrectCapitalization" "Function_IncorrectFunctionName" "Function_MissingParameter" "Function_UsedEquivalance")

for fileName in "${FileNames[@]}"; do

    rm -f $fileName.exe
    gnatmake $fileName.adb -o $fileName.exe

    if [ $? -ne 0 ]; then
        echo "Build failed for ./$fileName.exe"
    else
        echo "Build successful. Run with ./$fileName.exe"
    fi

done
