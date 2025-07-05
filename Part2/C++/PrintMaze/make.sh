#!/bin/bash

# Stop if any command fails
set -e

echo "Building C++ program..."
g++ -Wall -Wextra -O2 -o PrintMaze PrintMaze.cpp
echo "C++ program built: ./PrintMaze"
