#!/bin/bash

# Don't stop if any command fails
set +e

echo "Building C++ program..."

rm -f SystemCalls.exe
g++ -Wall -Wextra -O2 -o SystemCalls.exe SystemCalls.cpp
echo "C++ program built: ./SystemCalls.exe"

