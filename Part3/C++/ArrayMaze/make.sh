#!/bin/bash

# Don't stop if any command fails
set +e

echo "Building C++ program..."

rm -f ArrayMaze.exe
g++ -Wall -Wextra -O2 -o ArrayMaze.exe ArrayMaze.cpp
echo "C++ program built: ./ArrayMaze.exe"

