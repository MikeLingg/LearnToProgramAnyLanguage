#!/bin/bash

# Don't stop if any command fails
set +e

echo "Building C++ program..."

rm -r PrintMaze.exe
g++ -Wall -Wextra -O2 -o PrintMaze.exe PrintMaze.cpp
echo "C++ program built: ./PrintMaze.exe"
