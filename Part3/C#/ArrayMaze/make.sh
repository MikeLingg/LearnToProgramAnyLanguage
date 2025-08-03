#!/bin/bash
set +e
echo "Building C# program..."

rm -r ArrayMaze.exe
mcs -out:ArrayMaze.exe ArrayMaze.cs
echo "C# program built: ArrayMaze.exe"

