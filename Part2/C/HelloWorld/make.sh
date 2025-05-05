#!/bin/bash

# Stop if any command fails
set -e

echo "Building C program..."
gcc -Wall -Wextra -O2 -o HelloWorld HelloWorld.c
echo "C program built: ./HelloWorld"
