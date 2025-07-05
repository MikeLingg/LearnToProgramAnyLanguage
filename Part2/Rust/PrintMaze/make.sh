#!/bin/bash
set -e
echo "Building Rust program..."

rm -f PrintMaze.exe
rustc PrintMaze.rs -o PrintMaze.exe
echo "Rust program built: ./PrintMaze.exe"
