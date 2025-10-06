#!/bin/bash

set +e

echo "Building C program..."

rm -r ./HelloWorld.exe
gcc -Wall -Wextra -O2 -o HelloWorld.exe HelloWorld.c
echo "C program built: ./HelloWorld.exe"
