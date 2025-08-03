#!/bin/bash

# Don't stop if any command fails
set +e

echo "Building C program..."

rm -f ArrayMaze.exe
gcc -Wall -Wextra -O2 -o ArrayMaze.exe ArrayMaze.c
echo "C program built: ./ArrayMaze.exe"

