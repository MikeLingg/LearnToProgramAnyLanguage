#!/bin/bash

# Stop if any command fails
set -e

echo "Building C program..."

rm -f PrintMaze.exe
gcc -Wall -Wextra -O2 -o PrintMaze.exe PrintMaze.c
echo "C program built: ./PrintMaze.exe"
