#!/bin/bash

FileNames=("MultipleBuildMazeWithFunctions")

for fileName in "${FileNames[@]}"; do

    rm -f $fileName.exe
    gnatmake -g $fileName.adb -o $fileName.exe

    if [ $? -ne 0 ]; then
        echo "Build failed for ./$fileName.exe"
    else
        echo "Build successful. Run with ./$fileName.exe"
    fi

done
