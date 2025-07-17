#!/bin/bash

# Don't stop if any command fails
set +e

echo "Building C program..."

rm -f BasicConcepts.exe
gcc -Wall -Wextra -O2 -o BasicConcepts.exe BasicConcepts.c
echo "C program built: ./BasicConcepts.exe"

