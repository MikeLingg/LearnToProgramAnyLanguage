#!/bin/bash

# Don't stop if any command fails
set +e

echo "Building C++ program..."

rm -f BasicConcepts.exe
g++ -Wall -Wextra -O2 -o BasicConcepts.exe BasicConcepts.cpp
echo "C++ program built: ./BasicConcepts.exe"

