#!/bin/bash

FileNames=("Maze_3_x_3")

for fileName in "${FileNames[@]}"; do

    rm -r $fileName.exe
    mcs -out:$fileName.exe $fileName.cs

    if [ $? -ne 0 ]; then
        echo "Build failed for ./$fileName.exe"
    else
        echo "Build successful. Run with mono ./$fileName.exe"
    fi

done
