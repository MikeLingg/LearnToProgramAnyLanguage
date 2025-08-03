#!/bin/bash
set +e

echo "Building Go program..."

rm -f ArrayMaze.exe
go build -o ArrayMaze.exe ArrayMaze.go
echo "Go program built: ./ArrayMaze.exe"

