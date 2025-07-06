#!/bin/bash
set +e
echo "Building C# program..."

rm -r PrintMaze.exe
mcs -out:PrintMaze.exe PrintMaze.cs
echo "C# program built: PrintMaze.exe"
