#!/bin/bash

FileNames=("LoopConcepts")

for fileName in "${FileNames[@]}"; do

    rm -f $fileName.exe
    gfortran -o $fileName.exe $fileName.f90

    if [ $? -ne 0 ]; then
        echo "Build failed for ./$fileName.exe"
    else
        echo "Build successful. Run with ./$fileName.exe"
    fi

done
