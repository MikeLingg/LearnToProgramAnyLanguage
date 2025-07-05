#!/bin/bash
set +e

echo "Building Go program..."

rm -f PrintMaze.exe
go build -o PrintMaze.exe PrintMaze.go
echo "Go program built: ./PrintMaze.exe"

