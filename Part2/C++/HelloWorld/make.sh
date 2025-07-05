#!/bin/bash

# Don't stop if any command fails
set +e

echo "Building C++ program..."

rm -r HelloWorld.exe
g++ -Wall -Wextra -O2 -o HelloWorld.exe HelloWorld.cpp
echo "C++ program built: ./HelloWorld.exe"
