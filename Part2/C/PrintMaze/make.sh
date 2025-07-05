#!/bin/bash

# Stop if any command fails
set -e

echo "Building C program..."
rm -f PrintMaze
gcc -Wall -Wextra -O2 -o PrintMaze PrintMaze.c
echo "C program built: ./PrintMaze"
