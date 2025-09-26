#!/bin/bash

FileNames=("MultipleBuildMazeWithFunctions")

for fileName in "${FileNames[@]}"; do

    rm -f $fileName.exe
    gfortran -g -fcheck=bounds -Wall -o $fileName.exe $fileName.f90

    if [ $? -ne 0 ]; then
        echo "Build failed for ./$fileName.exe"
    else
        echo "Build successful. Run with ./$fileName.exe"
    fi

done
