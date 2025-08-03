#!/bin/bash
set +e
echo "Building Rust program..."

rm -f ArrayMaze.exe
rustc ArrayMaze.rs -o ArrayMaze.exe
echo "Rust program built: ./ArrayMaze.exe"


