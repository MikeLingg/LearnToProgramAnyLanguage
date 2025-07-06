#!/bin/bash
set +e
echo "Building Ada program..."

rm -f PrintMaze.exe
gnatmake PrintMaze.adb -o PrintMaze.exe
echo "Ada program built: ./PrintMaze.exe"
