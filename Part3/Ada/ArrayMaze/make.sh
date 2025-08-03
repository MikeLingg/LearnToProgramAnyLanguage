#!/bin/bash
set +e
echo "Building Ada program..."

rm -f ArrayMaze.exe
gnatmake ArrayMaze.adb -o ArrayMaze.exe
echo "Ada program built: ./ArrayMaze.exe"

